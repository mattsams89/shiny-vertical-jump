# Load Packages / Set Options -----
library(shiny) # Backend for the app
library(shinydashboard) # Makes design much easier
library(data.table) # Bulk of data processing; no developmental functions like fcase (1.12.9) used
library(zoo) # Rolling average filter
library(signal) # Butterworth filter design
library(changepoint) # Identification of "changepoints" to aid in event detection
library(purrr) # Efficient index evaluation of conditions for event detection
library(pracma) # Trapezoidal integration for net impulse
library(plotly) # Plotting
library(knitr) # Table output
library(kableExtra) # Table output
library(shinyjs) # Javascript operations to reset inputs, etc.

options(shiny.maxRequestSize = 40 * 1024 ^ 2) # Accepts files up to 40 mB in size; adjust based on your file size requirements
options(knitr.table.format = "html") # This saves me from having to declare this several times later

# Output table headers -----
# These headers are used for creation of the metric table and during the data saving process
# They're partitioned out to allow for different sets on the Calculated Metrics tab
# The addition of new jump types will necessitate new header sets since DJ, RJ, etc.
# tend to examine different variables than what are present here
sj_table_headers <- c("Body Mass", "Flight Time", "Net Impulse", "Jump Height (FT)",
                      "Jump Height (NI)", "Takeoff Velocity", "Peak Force", "Peak Velocity",
                      "Peak Power", "Force @ Peak Power", "Velocity @ Peak Power",
                      "Time to Peak Force", "Avg RFD", "Contact Time", "RSI Modified",
                      "FP1 Net Impulse", "FP2 Net Impulse", "Net Impulse SI",
                      "FP1 Peak Force", "FP2 Peak Force", "Peak Force SI",
                      "FP1 Time to Peak Force", "FP2 Time to Peak Force",
                      "Time to Peak Force SI", "FP1 Avg RFD", "FP2 Avg RFD", "Avg RFD SI")

cmj_table_headers <- c(sj_table_headers, "Unweighting Duration", "Braking Duration", "Propulsive Duration",
                       "Peak Braking Force", "Avg Braking Force", "Min Braking Velocity", "Avg Braking Velocity",
                       "Peak Braking Power", "Avg Braking Power", "Braking Work", "Force @ Zero Velo",
                       "Peak Propulsive Force", "Avg Propulsive Force", "Peak Propulsive Velocity", "Avg Propulsive Velocity",
                       "Peak Propulsive Power", "Avg Propulsive Power", "Propulsive Work")

save_headers <- c("date",
                  "name",
                  "jump_type",
                  "trial_number",
                  "bar_load",
                  cmj_table_headers)


# Uploaded file parsing -----
# This iteration of the app allows multiple upload types, so we need to account for that in our file parser
# Broadly, the data can be Pasco-based data...or everything else. For the parser, the format of everything else doesn't really matter
# as long as there's a single header row and all columns are only force-time data with left as fp1 and right as fp2.
file_parser <- function(file_location, upload_type){
  
  if(upload_type == "Pasco"){
    
    # This was implemented to deal with countries that use commas as decimal points. Generally, R is smart enough to determine what locale the user is in,
    # but I received several reports of the app not working for users affected by this. So there's a check in place to examine the first row of data
    # for any commas. If the sum returns anything > 0 (logical operations return 1 [true] or 0 [false]), sub() is applied at the end of the function to replace
    # the commas with decimals.
    decimal_check <- sum(fread(file = file_location,
                               skip = 1,
                               header = TRUE,
                               nrows = 1)[1] %like% ",")
    
    data <- fread(file = file_location,
                  skip = 1,
                  header = TRUE,
                  na.strings = c("", "NA"),
                  stringsAsFactors = FALSE)
    
    data <- data[, grepl("Normal",
                         names(data)),
                 with = FALSE]
    
    if(decimal_check > 0)
      data <- data[, lapply(.SD, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))]
    
    return(data)
  } 
  else {
    
    # See above comment about the reasoning for this being here.
    decimal_check <- sum(fread(file = file_location,
                               header = TRUE,
                               nrows = 1)[1] %like% ",")
    
    data <- fread(file = file_location,
                  header = TRUE,
                  na.strings = c("", "NA"),
                  stringsAsFactors = FALSE)
    
    if(decimal_check > 0)
      data <- data[, lapply(.SD, function(x) as.numeric(sub(",", ".", x, fixed = TRUE)))]
    
    return(data)
  }
}

# Calibration and filtering function -----
# First of the conversions to a function instead of being written out in the upload handling in the main app
# Data are passed to this function via the wide_data_manipulation and long_data_manipulation functions that are below
# In the former case, a lapply() returns series of fp1 and fp2, whereas in the latter, there's only two columns anyway.
# This filter_function works in series with single_jump_event_detection or multi_peak_detection,
# depending on the file format
filter_function <- function(fp1, fp2, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept, filter_type, sampling_frequency){
  
  # If you have a calibration equation, that's applied here; you want to be careful about adjusting any of the inputs
  # associated with this function (Apply Filter? Sampling Frequency, etc.) because adjusting them forces
  # a complete re-analysis of all trials in your data. Not the case for adjusting individual trials via
  # quiet_standing and jump_start_location.
  data <- data.table(fp1, fp2)[, setNames(.SD, c("fp1", "fp2"))
                               ][, na.omit(.SD)
                                 ][, ":=" (fp1 = fp1 * fp1_slope + fp1_intercept,
                                           fp2 = fp2 * fp2_slope + fp2_intercept)]
  
  # Filter implementation; checks if not "no filter." If not "no filter," checks what type of filter to apply
  if(filter_type != "no_filter"){
    
    # The first implemented type is a Butterworth filter; in this case, second order with a 10Hz cutoff
    # Since this is a digital filter, we need to know the sampling frequency. Cutoff frequency is a
    # value between 0 and 1 based on the Nyquist frequency (0.5 * sampling frequency)
    if(filter_type == "butt_filter"){
      
      filter <- butter(1, 10 / (0.5 * sampling_frequency), "low")
      
      # filtfilt applies a zero-phase filter
      data <- data[, lapply(.SD, function(x) filtfilt(filter, x))
                   ][, total_force := fp1 + fp2]
    }
    else{
      
      # 10-point moving average; this was incorrectly set to a right alignment previously
      # Center alignment causes the smoothed data to still be in sync with changes in the raw data,
      # whereas right alignment pushes the changes to the right of when they occur in the raw data.
      data <- data[, lapply(.SD, function(x) rollapply(x, 10, mean, align = "center", partial = TRUE))
                   ][, total_force := fp1 + fp2]
    }
  }
  else{
    data[, total_force := fp1 + fp2] # No filter applied
  }
  
  return(data)
}

# Single jump event detection function -----
# Once the data are filtered, they're then passed to this function or the multiple peak detection function
# depending on whether we're dealing with single trial/wide format data or long format data
# The methods are broken out from one another since we want this to return a single peak pair (takeoff/landing),
# whereas we need the other to return all peak pairs with a certain amount of time < threshold between them.
single_jump_event_detection <- function(data, sampling_frequency){
  
  # Prior to checking for data that meet our criteria for takeoff, landing, etc. we need to remove any data
  # that could be accidentally misclassified as below threshold (e.g., trial starts with athlete off the plates)
  # This is due to how findpeaks() works. The athlete stepping on to the plates could be accidentally misinterpreted
  # as a peak, which could throw off the detection function I've written.
  # In this case, a minimum of 440N (~45kg) is required. If you're working with lighter athletes, you'll need to adjust
  # 440 to the respective weight (in N) of your lightest athlete
  # The same adjustment needs to be made in multi_peak_detection
  start_index <- detect_index(data[, total_force],
                              ~ .x >= 440)
  
  end_index <- detect_index(data[, total_force],
                            ~ .x >= 440,
                            .dir = "backward")
  
  # Constrain data between index values
  data <- data[start_index:end_index]
  
  # To find approximate locations for peak force and peak landing force, we leverage pracma's findpeaks(). I can't remember
  # who keyed me onto this idea, but basically, we use a threshold of 40% of peak fp1/fp2 force to minimize
  # erroneous peaks found in total_force. Then, we do a little work to determine which peaks are actually associated with
  # takeoff and landing by checking for which peaks have 202 - 903 ms worth of below-threshold data between them.
  # In this case, threshold = 10 N; 202 and 903 ms ~ 5cm and 100cm, respectively. In theory, this should
  # remove points where unweighting falls below threshold, the athlete falls off the plates at the end of the jump, etc.
  # The threshold is based off individual plate peak landing force (in theory...I've seen some weird athletes),
  # so the 40% threshold shouldn't cause it to miss any takeoff or landing events since it uses the threshold
  # on total_force.
  data_peaks <- data[, as.data.table(findpeaks(total_force,
                                               minpeakdistance = round(500 * (sampling_frequency / 1000)),
                                               minpeakheight = max(c(max(fp1), max(fp2))) * 0.4))
                     ][order(V2)
                       ][, .(start = shift(V2, # To create a search range for peak pairs, we need to add 1
                                           fill = 1), # at the start; hence, a shift of the peaks found above
                             end = V2)]
  
  # This lapply searches between each start-end pair to determine the amount of time below the force threshold
  # of 10N. Like I said above, only 202ms - 903ms are considered likely candidates for flight time.
  # All other peak pairs are thrown out.
  # Good luck if you have athletes jumping lower than that, and good on you if they're jumping higher than that.
  data_peaks <- data_peaks[, below_threshold :=
                             unlist(
                               lapply(1:nrow(.SD),
                                      function(x){
                                        data[1:nrow(data) %between% .SD[x], sum(total_force < 10)]
                                      })
                             )][below_threshold %between% c(round(202 * (sampling_frequency / 1000)),
                                                            round(903 * (sampling_frequency / 1000)))]
  
  # Finds the index value for the final instance the athlete's weight is above 10N. R is indexed on 1,
  # so we have to subtract 2 from the value for this to coincide with the final point in the data
  # Starts searching from data_peaks$start, which is the peak likely associated with peak force prior to takeoff
  takeoff_index <- detect_index(data[data_peaks$start:nrow(data),
                                     total_force],
                                ~ .x <= 10) + data_peaks$start - 2
  
  # Landing, however, doesn't need addition or subtraction since we actually want the first point
  # where the athlete's weight is > 10N
  # Searches between the two peaks since they represent takeoff and landing peak force
  landing_index <- detect_index(data[data_peaks$start:data_peaks$end,
                                     total_force],
                                ~ .x <= 10,
                                .dir = "backward") + data_peaks$start
  
  # We don't actually want the entire flight phase for calculating the true force value of the unweighted
  # force plates since ringing could be an issue. So, we take the middle 50% of the data instead
  # The mean + 5SD is used as the true threshold for takeoff and landing, based on the literature.
  flight_adjust <- (landing_index - takeoff_index) * 0.25
  
  flight_statistics <- data[(takeoff_index + flight_adjust):(landing_index - flight_adjust),
                            .(mean = mean(total_force),
                              sd = sd(total_force))
                            ][, threshold := mean + sd * 5]
  
  # Now we adjust takeoff and landing based on these new values
  takeoff_index <- detect_index(data[data_peaks$start:nrow(data),
                                     total_force],
                                ~ .x <= flight_statistics$threshold) + data_peaks$start - 2
  
  landing_index <- detect_index(data[data_peaks$start:data_peaks$end,
                                     total_force],
                                ~ .x <= flight_statistics$threshold,
                                .dir = "backward") + data_peaks$start
  
  # For our initial best guess at a quiet standing portion for the jump, we find a 1s interval with
  # the lowest variance prior to takeoff. cpt.var finds "changepoints" in the variance of the data
  # based on a minimum segment length of 1s. This can be adjusted for individual trials
  # via the Quiet Standing input, but this initial first pass is performed on all trials to
  # give us starting points.
  # We suppress warnings in case #changepoints < Q < #changepoints
  suppressWarnings({
    var_changepoints <- cpt.var(data[, total_force],
                                method = "BinSeg",
                                minseglen = round(1000 * (sampling_frequency / 1000)),
                                Q = 5,
                                class = FALSE)
  })
  
  # The above is a vector; we want it to be a data.table to perform some similar data.table magic to the data_peaks
  # stuff above
  # Similar to data_peaks, we shift cp1 down one and add 1 to allow it to begin searching from the start
  # of the trial data (e.g. 1:1000, 1000:whatever)
  var_changepoints <- data.table(cbind(cp1 = shift(var_changepoints,
                                                   fill = 1),
                                       cp2 = var_changepoints))
  
  # This is very ugly, but it gets the job done. We want to find the changepoint pair with the lowest variance
  # Using lapply along with which.min, we can find the lowest variance in the changepoint pairs. This pair represents
  # our quiet standing phase and sets the jump's initial initiation threshold
  most_stable_changepoint <- data[, list(lapply(1:nrow(var_changepoints), function(x){
    data[1:nrow(data) %between% var_changepoints[x, .(cp1, cp2)],
         sd(total_force)]
  }))][, which.min(V1)]
  
  # Armed with our trusty stable changepoint, we can constrain the data. We need to update peak force, takeoff,
  # landing, and peak landing force based on all the calculations we've done above. +1 to each since R is indexed on 1.
  data <- data[var_changepoints[most_stable_changepoint, cp1]:nrow(data)]
  
  takeoff_index <- takeoff_index - var_changepoints[most_stable_changepoint, cp1] + 1
  
  landing_index <- landing_index - var_changepoints[most_stable_changepoint, cp1] + 1
  
  # Not an issue for the peaks, since which.max() can be used to recover the index value at which each occurs
  peak_force_index <- data[1:takeoff_index, which.max(total_force)]
  
  peak_landing_force_index <- data[, which.max(total_force)]
  
  # Ditto which.min()
  minimum_force_index <- data[1:peak_force_index, which.min(total_force)]
  
  # Relevant data and time points are passed to a list; this is passed along in a reactive context for further processing
  trial_information <- list(data = data,
                            minimum_force_index = minimum_force_index,
                            peak_force_index = peak_force_index,
                            flight_threshold = flight_statistics$threshold,
                            takeoff_index = takeoff_index,
                            landing_index = landing_index,
                            peak_landing_force_index = peak_landing_force_index)
  
  return(trial_information)
}

# Wide data / single trial manipulation -----
wide_data_manipulation <- function(data, filter_type, sampling_frequency, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept){
  
  trials <- ncol(data) / 2
  
  if(trials == 1){
    
    fp1 <- data[, 1, with = FALSE]
    
    fp2 <- data[, 2, with = FALSE]
    
    trial_data <- filter_function(fp1, fp2, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept, filter_type, sampling_frequency)
    
    trial_list <- single_jump_event_detection(trial_data, sampling_frequency)
  }
  else{
    trial_list <- lapply(1:trials, function(x){
      
      fp1 <- data[, (x * 2 - 1),
                  with = FALSE]
      
      fp2 <- data[, (x * 2),
                  with = FALSE]
      
      trial_data <- filter_function(fp1, fp2, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept, filter_type, sampling_frequency)
      
      event_list <- single_jump_event_detection(trial_data, sampling_frequency)
    })
  }
  return(trial_list) # This is returned implicitly since it's the last thing in the function, but it's generally
} # recommended to perform an explicit return since it avoids accidental returns of the wrong thing.

# Multiple peak detection -----
# Used for long files right now; can be expanded to repeated jumps
# After some finagling, this function returns an output that can be read by single_jump_event_detection via a loop
# See long_data_manipulation() below for further information
multi_peak_detection <- function(data, sampling_frequency){
  
  # Some similarities to the single_jump_event_detection early on, but then some divergence to separate the peak pairs into individual trials
  start_index <- detect_index(data[, total_force], 
                              ~ .x >= 440)
  
  end_index <- detect_index(data[, total_force], 
                            ~ .x >= 440, 
                            .dir = "backward")
  
  data <- data[start_index:end_index]
  
  # Divergence from single trial / wide data begins here. There are multiple takeoff and landing peaks in long data.
  # Therefore, we need to find all peaks. We again leverage findpeaks
  data_peaks <- data[, as.data.table(findpeaks(total_force,
                                               minpeakdistance = round(500 * (sampling_frequency / 1000)),
                                               minpeakheight = max(c(max(fp1), max(fp2))) * 0.4))
                     ][order(V2)
                       ][, .(start = shift(V2,
                                           fill = 1),
                             end = V2)]
  
  # For now, we remove any peak pairs where the time between isn't 202 - 903 ms
  data_peaks <- data_peaks[, below_threshold :=
                             unlist(
                               lapply(1:nrow(.SD),
                                      function(x){
                                        data[1:nrow(data) %between% .SD[x], sum(total_force < 10)]
                                      })
                             )][below_threshold %between% c(round(202 * (sampling_frequency / 1000)),
                                                            round(903 * (sampling_frequency / 1000)))]
  
  # We explicitly return the data and the associated peaks this time since the following function will
  # loop between peak pairs. I accidentally forgot to export the constrained data, which caused some
  # very frustrating errors until realized what I'd done.
  return(list(data = data,
              data_peaks = data_peaks))
}

# Long data manipulation -----
# For use when there are multiple trials contained in a single fp1/fp2 pair
# Operates nearly identically to the single jump event detection of the wide data / single trial stuff above
# The biggest difference is that we're dealing with two columns containing multiple trials now
# We first filter the data like before, but this time have multi_peak_detection() return
# a list of both the data and the peaks that it found. Previously, I had forgotten to have it return both
# and was trying to constrain the unfiltered data instead of the filtered data. Whoops.
long_data_manipulation <- function(data, filter_type, sampling_frequency, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept){
  
  fp1 <- data[, 1, with = FALSE]
  
  fp2 <- data[, 2, with = FALSE]
  
  filtered_data <- filter_function(fp1, fp2, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept, filter_type, sampling_frequency)
  
  # Returns a list containing $data and $data_peaks
  # Loop through $data_peaks to constrain $data and then perform single_jump_event_detection()
  peaks <- multi_peak_detection(filtered_data, sampling_frequency)
  
  # This works almost identically to the wide data approach earlier. This time, however, we need to clip the data into
  # chunks based around each identified peak pair (again, long data have multiple trials in two long streams of fp1/fp2 data).
  # I settled on 4 seconds behind takeoff peak force and .5 seconds past peak landing force. Adjust as needed.
  trial_list <- lapply(1:nrow(peaks$data_peaks), function(x){
    trial_data <- peaks$data[1:nrow(peaks$data) %between% peaks$data_peaks[x, list(start - round(3999 * (sampling_frequency / 1000)),
                                                                                   end + round(500 * (sampling_frequency / 1000)))]]
    
    # We can loop over single_jump_event_detection with the constrained data created above.
    event_list <- single_jump_event_detection(trial_data, sampling_frequency)
  })
  
  # The lapply returns a list that we pass on similar to the wide data
  return(trial_list)
}

# Single jump analysis -----
# Alright, both wide_data_manipulation and long_data_manipulation have returned identically structured lists that can
# be read by single_jump_analysis
single_jump_analysis <- function(data_list, sampling_frequency, quiet_standing_length,
                                 offset_brush, jump_type, start_definition, session,
                                 date, athlete, trial, bar_load){
  
  # Sometimes you'll see me do this, and other times you'll see me refer to variables specifically
  # in functions. It mostly depends on how frequently I would need to re-type stuff. Better to designate it
  # here in cases where I'd be re-typing variables very frequently. Could probably do a sprintf type thing
  # instead, but that sounds painful to figure out.
  data <- data_list$data
  minimum_force_index <- data_list$minimum_force_index
  peak_force_index <- data_list$peak_force_index
  flight_threshold <- data_list$flight_threshold
  takeoff_index <- data_list$takeoff_index
  landing_index <- data_list$landing_index
  peak_landing_force_index <- data_list$peak_landing_force_index
  
  # Checks if a brush has been applied to the primary plot. Brushing constrains the data to the starting and ending
  # x-axis index values, which can be leveraged to remove noisy quiet standing data, cut the athlete from falling off
  # the plates, etc. If the brush exists, start_offset is brush[1] - 1 since R starts indices at 1.
  if(!is.null(offset_brush)){
    
    start_offset <- offset_brush[1] - 1
    
    data <- data[1:nrow(data) %between% offset_brush]
    minimum_force_index <- minimum_force_index - start_offset
    peak_force_index <- peak_force_index - start_offset
    takeoff_index <- takeoff_index - start_offset
    landing_index <- landing_index - start_offset
    peak_landing_force_index <- peak_landing_force_index - start_offset
  }
  
  # Define all the necessary variables to find jump start and various unilateral metrics.
  # These are reactive to the quiet_standing input on the input pane on the left.
  body_weight <- data[1:quiet_standing_length, mean(total_force)]
  body_weight_sd <- data[1:quiet_standing_length, sd(total_force)]
  body_mass <- body_weight / 9.81
  fp1_weight <- data[1:quiet_standing_length, mean(fp1)]
  fp2_weight <- data[1:quiet_standing_length, mean(fp2)]
  
  # Checks for the minimum force value between trial start and peak force prior to takeoff
  # This is leveraged to attempt to determine jump type automatically
  minimum_force <- data[1:peak_force_index, min(total_force)]
  
  # The app attempts to determine if the trial is SJ or CMJ on its own. A 250N difference between body weight and minimum force (prior to peak force)
  # is fairly arbitrary but has worked thus far with athletes I've analyzed in baseball, volleyball, and soccer. It's certainly possible to adjust this threshold
  # if a lot of your athletes aren't being detected correctly, but the simpler option may be manually setting the jump type in the input than determining
  # a threshold that better suits your needs. Up to you, though.
  if(jump_type == "auto"){
    if(body_weight - minimum_force > 250)
      jump_type <- "cmj"
    else
      jump_type <- "sj"
  }
  
  # Both SJ and CMJ follow a similar logic set in determining jump start, pre-movement, etc.
  # Start by determining the initiation threshold (quiet standing weight +/- 5*SD quiet standing force) alongside the
  # "inverse threshold." That is, -5SD for SJ and +5SD for CMJ.
  if(jump_type == "sj"){
    initiation_threshold <- body_weight + body_weight_sd * 5
    
    inverse_threshold <- body_weight - body_weight_sd * 5
    
    # For SJ, searches backward from peak force to find the first point below the initiation threshold.
    jump_threshold_index <- detect_index(data[1:peak_force_index, total_force],
                                         ~ .x <= initiation_threshold,
                                         .dir = "backward")
    
    # Also searches a 100ms window prior to the inititaion threshold to determine if there was a countermovement
    # below the inverse threshold. If so, re-searches for jump start from the minimum force
    # of the countermovement. First, finds the first point above the inverse threshold in a backward search of the data
    # and then adjusts the start either by moving backward an additional 30ms or until it finds a point near body weight,
    # dependent on your selection in the jump start location input.
    if(sum(data[(jump_threshold_index - 0.1 * sampling_frequency):jump_threshold_index,
                min(total_force)] <= inverse_threshold) > 0){
      
      pre_movement_minimum_force <- data[(jump_threshold_index - 0.1 * sampling_frequency):jump_threshold_index,
                                         min(total_force)]
      
      pre_movement_minimum_force_index <- detect_index(data[1:jump_threshold_index, total_force],
                                                       ~ .x == pre_movement_minimum_force,
                                                       .dir = "backward")
      
      if(start_definition == "5SD - BW"){
        jump_start_index <- detect_index(data[1:pre_movement_minimum_force_index, total_force],
                                         ~ .x >= body_weight,
                                         .dir = "backward") + 1
      }
      else{
        jump_start_index <- detect_index(data[1:pre_movement_minimum_force_index, total_force],
                                         ~ .x >= inverse_threshold,
                                         .dir = "backward") - round(0.029 * sampling_frequency)
      }
      
      showNotification(ui = "SJ detected with countermovement > 5SD body weight!", 
                       duration = 5, 
                       closeButton = FALSE, 
                       type = "warning")
    }
    else{
      
      # If no countermovement is detected, however, true jump start is defined as the first point below or equal to body weight
      # OR 30ms back from the initiation threshold.
      if(start_definition == "5SD - BW"){
        jump_start_index <- detect_index(data[1:jump_threshold_index, total_force],
                                         ~ .x <= body_weight,
                                         .dir = "backward") + 1
      }
      else{
        jump_start_index <- jump_threshold_index - round(0.029 * sampling_frequency)
      }
    }
  }
  else{
    # Same idea for CMJs. Determine the initiation threshold and inverse threshold, find the first point exceeding the initiation threshold
    # (by searching backward from minimum force in the countermovement), determine if the athlete exceeded the inverse threshold
    # prior to initiating their countermovement, and adjust the data as appropriate from there. 
    initiation_threshold <- body_weight - body_weight_sd * 5
    
    inverse_threshold <- body_weight + body_weight_sd * 5
    
    jump_threshold_index <- detect_index(data[1:minimum_force_index, total_force],
                                         ~ .x >= initiation_threshold,
                                         .dir = "backward")
    
    if(sum(data[(jump_threshold_index - 0.1 * sampling_frequency):jump_threshold_index,
                max(total_force)] >= inverse_threshold) > 0){
      
      pre_movement_maximum_force <- data[(jump_threshold_index - 0.1 * sampling_frequency):jump_threshold_index,
                                         max(total_force)]
      
      pre_movement_maximum_force_index <- detect_index(data[1:jump_threshold_index, total_force],
                                                       ~ .x == pre_movement_maximum_force,
                                                       .dir = "backward")
      
      if(start_definition == "5SD - BW"){
        jump_start_index <- detect_index(data[1:pre_movement_maximum_force_index, total_force],
                                         ~ .x <= body_weight,
                                         .dir = "backward") + 1
      }
      else{
        jump_start_index <- detect_index(data[1:pre_movement_maximum_force_index, total_force],
                                         ~ .x <= inverse_threshold,
                                         .dir = "backward") - round(0.029 * sampling_frequency)
      }
    }
    else{
      
      if(start_definition == "5SD - BW"){
        jump_start_index <- detect_index(data[1:jump_threshold_index, total_force],
                                         ~ .x >= body_weight,
                                         .dir = "backward") + 1
      }
      else{
        jump_start_index <- jump_threshold_index - round(0.029 * sampling_frequency)
      }
    }
  }
  
  # First list exported from the function; used for secondary plot
  descriptive_list <- list(data = data,
                           quiet_standing_length = quiet_standing_length,
                           body_weight = body_weight,
                           body_weight_sd = body_weight_sd,
                           body_mass = body_mass,
                           fp1_weight = fp1_weight,
                           fp2_weight = fp2_weight,
                           jump_type = jump_type,
                           sampling_frequency = sampling_frequency,
                           initiation_threshold = initiation_threshold,
                           jump_threshold_index = jump_threshold_index,
                           jump_start_index = jump_start_index,
                           minimum_force_index = minimum_force_index,
                           peak_force_index = peak_force_index,
                           flight_threshold = flight_threshold,
                           takeoff_index = takeoff_index,
                           landing_index = landing_index,
                           peak_landing_force_index = peak_landing_force_index)
  
  # Originally had this in a second function, but I think this saves a lot of re-typing
  # variables, etc. by moving it to here
  # The below calculates net impulse, velocity, power, etc. as well as jump-specific
  # variables. Both bilateral and unilateral calculations are performed for 
  # a number of the variables. Everything is eventually exported as lists
  jump_data <- data[jump_start_index:takeoff_index]
  
  # Define an impulse calculation function here to avoid typing it over and over again.
  impulse <- function(.data, force_data, weight, sampling_frequency){
    cumtrapz(1:nrow(.data), force_data - weight) / sampling_frequency
  }
  
  jump_data[, ":=" (net_impulse = impulse(jump_data,
                                          total_force,
                                          body_weight,
                                          sampling_frequency),
                    fp1_net_impulse = impulse(jump_data,
                                              fp1,
                                              fp1_weight,
                                              sampling_frequency),
                    fp2_net_impulse = impulse(jump_data,
                                              fp2,
                                              fp2_weight,
                                              sampling_frequency))
            ][, velocity := net_impulse / body_mass
              ][, power := total_force * velocity]
  
  # Landing index and takeoff index represent the first and last points the athlete is on the plates, respectively;
  # To only capture the time between these points, we need to subtract an additional index value before dividing by the sampling frequency
  flight_time <- (landing_index - takeoff_index - 1) / sampling_frequency
  net_impulse <- jump_data[, last(net_impulse)] # We only want the last point from the impulse calculation since it's cumulative in nature
  jump_height_ft <- 0.5 * 9.81 * (flight_time / 2) ^ 2
  takeoff_velocity <- jump_data[, last(velocity)]
  jump_height_ni <- takeoff_velocity ^ 2 / (2 * 9.81)
  peak_force <- jump_data[, max(total_force)]
  peak_velocity <- jump_data[, max(velocity)]
  peak_power <- jump_data[, max(power)]
  peak_power_index <- jump_data[, which.max(power)]
  force_peak_power <- jump_data[peak_power_index, total_force]
  velocity_peak_power <- jump_data[peak_power_index, velocity]
  time_to_peak_force <- jump_data[, which.max(total_force)] / sampling_frequency
  avg_rfd <- (peak_force - jump_data[, first(total_force)]) / time_to_peak_force
  contact_time <- nrow(jump_data) / sampling_frequency
  rsi_modified <- jump_height_ni / contact_time
  
  # This table is what we'll use to hold everything for the calculated metrics tab and for the data saving process
  metric_table <- data.table(
    date,
    athlete,
    jump_type,
    trial,
    bar_load,
    body_mass,
    flight_time,
    net_impulse,
    jump_height_ft,
    jump_height_ni,
    takeoff_velocity,
    peak_force,
    peak_velocity,
    peak_power,
    force_peak_power,
    velocity_peak_power,
    time_to_peak_force,
    avg_rfd,
    contact_time,
    rsi_modified
  )
  
  # Unilateral measures for certain variables; asymmetry is based on %symmetry index
  fp1_net_impulse <- jump_data[, last(fp1_net_impulse)]
  fp2_net_impulse <- jump_data[, last(fp2_net_impulse)]
  net_impulse_symmetry_index <- (fp1_net_impulse - fp2_net_impulse) / 
    mean(c(fp1_net_impulse, fp2_net_impulse)) * 100
  fp1_peak_force <- jump_data[, max(fp1)]
  fp2_peak_force <- jump_data[, max(fp2)]
  peak_force_symmetry_index <- (fp1_peak_force - fp2_peak_force) / 
    mean(c(fp1_peak_force, fp2_peak_force)) * 100
  fp1_peak_force_index <- jump_data[, which.max(fp1)]
  fp2_peak_force_index <- jump_data[, which.max(fp2)]
  fp1_time_to_peak_force <- fp1_peak_force_index / sampling_frequency
  fp2_time_to_peak_force <- fp2_peak_force_index / sampling_frequency
  ttpf_symmetry_index <- (fp1_time_to_peak_force - fp2_time_to_peak_force) / 
    mean(c(fp1_time_to_peak_force, fp2_time_to_peak_force)) * 100
  fp1_avg_rfd <- (fp1_peak_force - jump_data[, first(fp1)]) / fp1_time_to_peak_force
  fp2_avg_rfd <- (fp2_peak_force - jump_data[, first(fp2)]) / fp2_time_to_peak_force
  avg_rfd_symmetry_index <- (fp1_avg_rfd - fp2_avg_rfd) / mean(c(fp1_avg_rfd, fp2_avg_rfd)) * 100
  
  metric_table <- cbind(metric_table,
                        fp1_net_impulse,
                        fp2_net_impulse,
                        net_impulse_symmetry_index,
                        fp1_peak_force,
                        fp2_peak_force,
                        peak_force_symmetry_index,
                        fp1_time_to_peak_force,
                        fp2_time_to_peak_force,
                        ttpf_symmetry_index,
                        fp1_avg_rfd,
                        fp2_avg_rfd,
                        avg_rfd_symmetry_index)
  
  # CMJ-specific variables are calculated below. Unlike SJ, there are multiple phases we can examine (unweighting, braking, and propulsive at the basic level).
  # Also often called unweighting, eccentric, and concentric (or eccentric and concentric). 
  # If you want to get REALLY fancy, it's also possible to break the jump into unweighting,
  # braking, propulsion-acceleration, and propulsion-deceleration. From there, you can calculate shape factor, etc. (Mizuguchi, 2012; Sole, 2015).
  # That's pretty deep into the analysis weeds, though, and is not reflected here. Feel free to create your own fork to perform those analyses.
  if(jump_type == "cmj"){
    
    peak_force_index <- jump_data[, which.max(total_force)]
    
    minimum_force_index <- jump_data[1:peak_force_index, which.min(total_force)]
    
    # We want the final point less than or equal to initial force, so -2 instead of -1.
    unweight_end_index <- detect_index(jump_data[minimum_force_index:peak_force_index, total_force],
                                       ~ .x >= jump_data[, first(total_force)]) + minimum_force_index - 2
    
    # That means we have to +1 to accurately reflect the start of the braking phase. We only have to -1 to find the end of braking
    # instead of -2, though.
    braking_end_index <- detect_index(jump_data[(unweight_end_index + 1):nrow(jump_data), net_impulse],
                                      ~ .x >= 0) + unweight_end_index - 1
    
    unweight_duration <- unweight_end_index / sampling_frequency
    braking_duration <- (braking_end_index - unweight_end_index) / sampling_frequency
    propulsive_duration <- (nrow(jump_data) - braking_end_index) / sampling_frequency
    
    # We want the first point that crosses 0 velocity after unweighting, so we +1 since we searched backward through velocity
    zero_velo_index <- detect_index(jump_data[, velocity],
                                    ~ .x <= 0,
                                    .dir = "backward") + 1
    
    force_zero_velo <- jump_data[zero_velo_index,
                                 total_force]
    
    # Additional variables suggested by Lake et al. (in preparation)
    peak_braking_force <- jump_data[(unweight_end_index + 1):braking_end_index, max(total_force)]
    avg_braking_force <- jump_data[(unweight_end_index + 1):braking_end_index, mean(total_force)]
    
    min_braking_velocity <- jump_data[(unweight_end_index + 1):braking_end_index, min(velocity)]
    avg_braking_velocity <- jump_data[(unweight_end_index + 1):braking_end_index, mean(velocity)]
    
    peak_braking_power <- jump_data[(unweight_end_index + 1):braking_end_index, min(power)]
    avg_braking_power <- jump_data[(unweight_end_index + 1):braking_end_index, mean(power)]
    
    braking_work <- avg_braking_power * braking_duration
    
    peak_propulsive_force <- jump_data[(braking_end_index + 1):nrow(jump_data), max(total_force)]
    avg_propulsive_force <- jump_data[(braking_end_index + 1):nrow(jump_data), mean(total_force)]
    
    peak_propulsive_velocity <- jump_data[(braking_end_index + 1):nrow(jump_data), max(velocity)]
    avg_propulsive_velocity <- jump_data[(braking_end_index + 1):nrow(jump_data), mean(velocity)]
    
    peak_propulsive_power <- jump_data[(braking_end_index + 1):nrow(jump_data), max(power)]
    avg_propulsive_power <- jump_data[(braking_end_index + 1):nrow(jump_data), mean(power)]
    
    propulsive_work <- avg_propulsive_power * propulsive_duration
    
    metric_table <- cbind(metric_table,
                          unweight_duration,
                          braking_duration,
                          propulsive_duration,
                          peak_braking_force,
                          avg_braking_force,
                          min_braking_velocity,
                          avg_braking_velocity,
                          peak_braking_power,
                          avg_braking_power,
                          braking_work,
                          force_zero_velo,
                          peak_propulsive_force,
                          avg_propulsive_force,
                          peak_propulsive_velocity,
                          avg_propulsive_velocity,
                          peak_propulsive_power,
                          avg_propulsive_power,
                          propulsive_work)
  }
  
  # Similar to the list returned above, although this list is used
  # for the quick view, calculated metrics, and CSV export
  analysis_list <- list(descriptive_list = descriptive_list,
                        metric_table = metric_table,
                        subplot_data = jump_data)
  
  return(analysis_list)
}

# Plotting functions -----
# Function for plotting vertical lines on plots since plotly doesn't natively do vlines
vline <- function(x = 0, color = "black"){
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,
                dash = "dot",
                width = 1)
  )
}

# Ditto horizontal lines; a little more functionality here argument-wise
# to deal with different segment lengths
hline <- function(x0 = 0, x1 = 0, y = 0, color = "red"){
  list(
    type = "line",
    x0 = x0,
    x1 = x1,
    y0 = y,
    y1 = y,
    xref = "x",
    yref = "y",
    line = list(color = color)
  )
}

# This is an instance of a function where I didn't feel it was worth defining everything at the top
# Whether that's accurate, eh.
secondary_plot <- function(analysis_list){
  
  plot <- plot_ly(data = analysis_list$data,
                  x = ~seq_along(total_force)) %>%
    add_lines(y = ~fp1,
              name = "FP1") %>%
    add_lines(y = ~fp2,
              name = "FP2") %>%
    add_lines(y = ~total_force,
              name = "Total Force") %>%
    add_annotations(x = 0,
                    y = 1,
                    xref = "paper",
                    yref = "paper",
                    showarrow = FALSE,
                    text = paste0(
                      "Body Mass (kg): ", round(analysis_list$body_mass, 2),
                      "<br>Body Weight (N): ", round(analysis_list$body_weight, 2),
                      "<br>Body Weight SD (N): ", round(analysis_list$body_weight_sd, 2),
                      "<br>Jump Type: ", toupper(analysis_list$jump_type),
                      "<br>Initiation Threshold (N): ", round(analysis_list$initiation_threshold, 2),
                      "<br>Flight Threshold (N): ", round(analysis_list$flight_threshold, 2)
                    )
    ) %>%
    layout(
      shapes = list(
        vline(analysis_list$jump_start), # Actual jump start
        vline(analysis_list$jump_threshold_index), # The point that breaks the initiation threshold
        vline(analysis_list$peak_force), # Peak pre-takeoff force
        vline(analysis_list$takeoff), # Takeoff point
        vline(analysis_list$landing), # Ditto landing
        vline(analysis_list$peak_landing_force), # Ditto peak landing force
        hline(x0 = 1, 
              x1 = analysis_list$jump_threshold_index, 
              y = analysis_list$initiation_threshold), # Visual representation of the initiation threshold
        hline(x0 = analysis_list$takeoff,
              x1 = analysis_list$landing,
              y = analysis_list$flight_threshold), # Same for the flight threshold
        list(
          type = "rect", # Filled ribbon used to show the SD of quiet standing and resultant inverse threshold
          fillcolor = "blue", # Helps the user understand why they receive a warning about the inverse threshold
          line = list(color = "blue"),
          opacity = 0.3,
          x0 = 1,
          x1 = analysis_list$jump_threshold_index,
          xref = "x",
          y0 = analysis_list$body_weight - analysis_list$body_weight_sd * 5,
          y1 = analysis_list$body_weight + analysis_list$body_weight_sd * 5,
          yref = "y"
        )
      ),
      xaxis = list(title = "Index"),
      yaxis = list(title = "Force (N)")
    )
  
  return(plot)
}

# The subplots for the two jump type variants currently supported are vastly different in their displays
# This is because CMJ have different phases...I know some people have tried to define SJ phases, but get
# outta here with that noise. For SJ, the name of the variable is passed as a string argument,
# which is converted to the variable via get(). Subplot titles are manually set in the app.R
# since there was no real way to do that here without some serious refactoring.
sj_subplot_design <- function(data, variable){
  
  plot <- plot_ly(data,
                  x = ~1:nrow(data),
                  y = ~get(variable)) %>%
    add_lines(name = variable,
              showlegend = FALSE)
  
  return(plot)
}

# CMJ is unfortunately more complex. Originally, I manually created each section for the force
# data, but I realized I was being dumb and changed it to net force instead. That is,
# force - starting force value. This cuts down on clutter considerably.
cmj_subplot_design <- function(data, variable, unweight_end_index, 
                               braking_end_index){
  
  # We copy() here since data.table modifies by reference. Modifying by reference
  # *will* change the data specified in the data argument of the function, which I learned the hard
  # way for the calculated metrics tab. Copy() forces data.table to perform calculations on
  # a copy of the data.
  if(variable == "total_force")
    data <- copy(data[, total_force := total_force - first(total_force)])
  
  # We create a filled ribbon for each phase of the CMJ. Ribbons can be auto-filled
  # via "tozeroy". Hence why net force is used instead of the gross force.
    plot <- plot_ly(type = "scatter",
                    mode = "lines") %>%
      add_trace(data = data[1:unweight_end_index],
                x = ~seq_along(get(variable)),
                y = ~get(variable),
                fill = "tozeroy",
                color = I("red"),
                showlegend = FALSE) %>%
      add_trace(data = data[(unweight_end_index + 1):braking_end_index],
                x = ~((unweight_end_index + 1):braking_end_index),
                y = ~get(variable),
                fill = "tozeroy",
                color = I("orange"),
                showlegend = FALSE) %>%
      add_trace(data = data[(braking_end_index + 1):nrow(data)],
                x = ~((braking_end_index + 1):nrow(data)),
                y = ~get(variable),
                fill = "tozeroy",
                color = I("blue"),
                showlegend = FALSE)
  
  return(plot)
}

# Table design for the Calculated Metrics tab.
metric_table_design <- function(data){
  
  # Again, we want a copy to prevent modify by reference-induced bugs.
  table_data <- copy(data)
  jump_type <- table_data[, jump_type]
  
  if(jump_type == "cmj")
    headers <- cmj_table_headers
  else
    headers <- sj_table_headers
  
  table_data[, ":=" (date = NULL,
                     athlete = NULL,
                     jump_type = NULL,
                     trial = NULL,
                     bar_load = NULL)]
  setnames(table_data, headers)
  
  # I think this could be accomplished with transpose() as well, but
  # you know what they say about things that ain't broke.
  long_table <- melt(table_data,
                     measure.vars = 1:ncol(table_data),
                     variable.name = "Variable",
                     value.name = "Value")
  
  output_table <- kable(long_table,
                        digits = 3, # Standardize all values at 3 digits
                        col.names = NULL) %>% # No need for "variable" and "value" in the table
    kable_styling(bootstrap_options = "striped") %>%
    pack_rows("Bilateral Variables", # We know the exact number of variables in this case
              start_row = 1,
              end_row = 15,
              label_row_css = "background-color: #004687; color: #fff;") %>% # Edit these to change the look of your table
    pack_rows("Unilateral Variables",
              start_row = 16,
              end_row = 27,
              label_row_css = "background-color: #004687; color: #fff;")
  
  if(jump_type == "cmj"){
    output_table <- output_table %>%
      pack_rows("Phase Variables",
                start_row = 28,
                end_row = 45,
                label_row_css = "background-color: #004687; color: #fff;")
  }
  
  output_table <- output_table %>%
    scroll_box(width = "100%",
               height = "750px")
  
  return(output_table)
}

# Save function -----
save_function <- function(data_to_write){
  
  if(!dir.exists("Analyses"))
    dir.create("Analyses")
  
  save_file <- file.path("Analyses",
                         paste(Sys.Date(),
                               "Vertical Jump Analysis.csv"))
  
  if(!file.exists(save_file)){
    fwrite(transpose(list(save_headers)), 
           save_file)
  }
  
  fwrite(data_to_write, 
         save_file, 
         append = TRUE)
}