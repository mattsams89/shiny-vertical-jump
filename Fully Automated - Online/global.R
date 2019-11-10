library(shiny) # Backend for the app
library(shinydashboard) # Makes design much easier
library(data.table) # Bulk of data processing
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
options(knitr.table.format = "html")

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

# This iteration of the app allows multiple upload types, so we need to account for that in our file parser
# Broadly, the data can be Pasco-based data...or everything else. For the parser, the format of everything else doesn't really matter
# as long as there's a single header row and all columns are only force-time data with left as fp1 and right as fp2.
file_parser <- function(file_location, upload_type){
  
  if(upload_type == "Pasco"){
    
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

# Unlike the parser, data_manipulation does need to know what type of file it's working with as each file type is handled differently.
# Pasco and wide multi-trial data are assumed to follow the same format, and single trials are pretty easy to deal with as well.
# The implementation for long format multi-trial data is still experimental and needs work. I have several ideas for solutions,
# but testing said solutions is time-consuming.
# Regardless, we need to identify the upload type, the filter (if any) to apply, the sampling frequency (needed for filtering),
# and the calibration values for the force plates (if they exist). Defaults for slope and intercept are 1 and 0, respectively.
data_manipulation <- function(uploaded_data, upload_type, filter_type, sampling_frequency, fp1_slope, fp2_slope, fp1_intercept, fp2_intercept){
  
  # This function is called in a reactive context, so uploaded_data will come from a reactiveValues
  data <- uploaded_data
  
  #### Wide data analysis ####
  # Implementation is the same for Pasco and wide multi-trial data
  if(upload_type == "Pasco" | upload_type == "Multiple Trials - Wide"){
    
    # We assume pairs of columns are fp1 and fp2 for the trial; count total columns and divide by 2 to get total # of trials
    trials <- ncol(data) / 2
    
    # Once we know the number of trials, this function loops through the column pairs and applies any filtering and/or calibration
    # while also determining proposed quiet standing and takeoff/landing time points
    trial_list <- lapply(1:trials, function(x){
      
      # fp1 and fp2 are using data.table's with argument to return specific columns based on column index
      fp1 <- data[, (x * 2 - 1),
                  with = FALSE] * fp1_slope + fp1_intercept
      
      fp2 <- data[, (x * 2),
                  with = FALSE] * fp2_slope + fp2_intercept
      
      # The vectors are joined as a data.table and renamed to fp1 and fp2; na.omit removes any instances where one plate
      # recorded force values and the other plate didn't; only relevant for end of trials
      trial_data <- data.table(fp1, fp2)[, setNames(.SD, c("fp1", "fp2"))
                                         ][, na.omit(.SD)]
      
      # Filter implementation; if not "no filter", a check for filter type occurs and the appropriate filter is applied
      if(filter_type != "no_filter"){
        
        # The first implemented type is a Butterworth filter; in this case, second order with a 10Hz cutoff
        # Since this is a digital filter, we need to know the sampling frequency as the cutoff frequency is
        # represented as a value between 0 and 1.
        if(filter_type == "butt_filter"){
          
          filter <- butter(1, 10 / (0.5 * sampling_frequency), "low")
          
          # filtfilt for zero phase filtering
          trial_data <- trial_data[, lapply(.SD, function(x) filtfilt(filter, x))
                                   ][, total_force := fp1 + fp2]
        }
        else{
          
          # 10-point moving average filter
          trial_data <- trial_data[, lapply(.SD, function(x) rollapplyr(x, 10, mean, partial = TRUE))
                                   ][, total_force := fp1 + fp2]
        }
      }
      else{
        trial_data[, total_force := fp1 + fp2]
      }
      
      # Checks the beginning and end of each trial and removes any data where the force is < 440N (45kg)
      # Makes it OK for the athlete to not be on the force plates at the beginning of the trial
      # detect_index returns the index value of the first point satisfying the criterion.
      # the .dir = "backward" argument causes the function to search in reverse
      start_index <- detect_index(trial_data[, total_force], 
                                  ~ .x >= 440)
      
      end_index <- detect_index(trial_data[, total_force], 
                                ~ .x >= 440, 
                                .dir = "backward")
      
      # Constrain the trial between the two index values found above as necessary
      trial_data <- trial_data[start_index:end_index]
      
      # Instead of using changepoints, I'm trialing pracma's findpeaks. I can't remember who shared their idea for this,
      # but I know I shared findpeaks with them first...Regardless, we can use findpeaks to find peak propulsive force
      # and peak landing force. There's a chance this approach will return more peaks than we want, however,
      # so we need to do a little work to determine which peaks are actually associated with takeoff and landing.
      # We do that by counting the number of points below 10N and removing any pair not between 202 and 903 ms,
      # equivalent to jh_ft of 5cm and 100cm. In theory, this should remove points where the athlete unloads during the unweighting phase
      # or steps off the plates / falls off the plates at the end of a trial.
      data_peaks <- trial_data[, as.data.table(findpeaks(total_force,
                                                         minpeakdistance = round(500 * (sampling_frequency / 1000)),
                                                         minpeakheight = max(c(max(fp1), max(fp2))) * 0.4))
                               ][order(V2)
                                 ][, .(start = shift(V2, 
                                                     fill = 1),
                                       end = V2)]
      
      data_peaks <- data_peaks[, below_threshold := 
                                 unlist(
                                   lapply(1:nrow(.SD), 
                                          function(x){
                                            trial_data[1:nrow(trial_data) %between% .SD[x], sum(total_force < 10)]
                                          }))
                               ][below_threshold %between% c(round(202 * (sampling_frequency / 1000)), 
                                                             round(903 * (sampling_frequency / 1000)))]
      
      # Finds the index value for the final instance the athlete's total force is above 10N. R is indexed on 1,
      # so we have to subtract 2 from the value for this to actually coincide with the final point in the data.
      takeoff_index <- detect_index(trial_data[data_peaks$start:nrow(trial_data), 
                                               total_force],
                                    ~ .x < 10) + data_peaks$start - 2
      
      # Landing index, however, doesn't need any additional addition or subtraction as we want the first point the athlete's force
      # is above 10N
      landing_index <- detect_index(trial_data[data_peaks$start:data_peaks$end,
                                               total_force],
                                    ~ .x < 10,
                                    .dir = "backward") + data_peaks$start
      
      # We want to cut a portion of the flight phase away before calculating mean force and sd of force to (hopefully) account for
      # ringing in the plates at takeoff. I rather arbitrarily chose 25% from each side. That is, the middle 50% of the data
      # are used to calculate mean and SD to adjust takeoff and landing. Similar to the literature, 5x the SD of the flight
      # force is used as our threshold for takeoff and landing.
      flight_adjust <- (landing_index - takeoff_index) * 0.25
      
      flight_statistics <- trial_data[(takeoff_index + flight_adjust):(landing_index - flight_adjust),
                                      .(mean = mean(total_force),
                                        sd = sd(total_force) * 5)
                                      ][, threshold := mean + sd]
      
      # Now, let's adjust takeoff_index and landing_index
      takeoff_index <- detect_index(trial_data[data_peaks$start:nrow(trial_data), 
                                               total_force],
                                    ~ .x < flight_statistics$threshold) + data_peaks$start - 2
      
      landing_index <- detect_index(trial_data[data_peaks$start:data_peaks$end,
                                               total_force],
                                    ~ .x < flight_statistics$threshold,
                                    .dir = "backward") + data_peaks$start
      
      # We're going to find a 1 second interval with the lowest variance; this is going to be
      # quiet standing and will be used to calculate our initiation threshold
      # cpt.var calculates "changepoints" in the variance of the force data across minimum lengths (minseglen)
      suppressWarnings({
        var_changepoints <- cpt.var(trial_data[, total_force],
                                    method = "BinSeg",
                                    minseglen = round(1000 * (sampling_frequency / 1000)),
                                    Q = 5,
                                    class = FALSE)
      })
      
      # The above returned a vector, so we want to create a data.table of the time point pairs within the vector
      var_changepoints <- data.table(cbind(cp1 = shift(var_changepoints,
                                                       fill = 1),
                                           cp2 = var_changepoints))
      
      # This looks like a complete and utter cluster...and it kinda is. But we want to find the changepoint
      # with the lowest standard deviation in its force data. So we use this lapply along with which.min
      # to find the changepoint pair. This changepoint pair is our quiet standing and sets our initial
      # initiation threshold
      most_stable_changepoint <- trial_data[, list(lapply(1:nrow(var_changepoints), function(x){
        trial_data[1:nrow(trial_data) %between% var_changepoints[x, .(cp1, cp2)],
                   sd(total_force)]
      }))][, which.min(V1)]
      
      # Armed with our trusty stable changepoint, we can constrain our data; we'll need to update peak force, takeoff, landing,
      # and peak landing force as well. +1 to each since R is indexed on 1.
      trial_data <- trial_data[var_changepoints[most_stable_changepoint, cp1]:nrow(trial_data)]
      
      takeoff_index <- takeoff_index - var_changepoints[most_stable_changepoint, cp1] + 1
      
      landing_index <- landing_index - var_changepoints[most_stable_changepoint, cp1] + 1
      
      peak_force_index <- trial_data[1:takeoff_index, which.max(total_force)]
      
      peak_landing_force_index <- trial_data[, which.max(total_force)]
      
      minimum_force_index <- trial_data[1:peak_force_index, which.min(total_force)]
      
      # Save relevant information to a list; this will be passed along in a reactive context for further processing
      trial_information <- list(trial_data = trial_data,
                                minimum_force_index = minimum_force_index,
                                peak_force_index = peak_force_index,
                                flight_threshold = flight_statistics$threshold,
                                takeoff_index = takeoff_index,
                                landing_index = landing_index,
                                peak_landing_force_index = peak_landing_force_index)
    })
    
    return(trial_list)
  }
  #### Single trial analysis ####
  # Single trial analysis is the exact same. I'm an idiot and didn't create a function out of the above, so you get to see it again.
  else if(upload_type == "Single Trial"){
    
    trial_data <- uploaded_data
    
    setnames(trial_data, c("fp1", "fp2"))
    
    trial_data <- trial_data[, ":=" (fp1 = fp1 * fp1_slope + fp1_intercept,
                                     fp2 = fp2 * fp2_slope + fp2_intercept)
                             ][, na.omit(.SD)]
    
    # Filter implementation; if not "no filter", a check for filter type occurs and the appropriate filter is applied
    if(filter_type != "no_filter"){
      
      # The first implemented type is a Butterworth filter; in this case, second order with a 10Hz cutoff
      # Since this is a digital filter, we need to know the sampling frequency as the cutoff frequency is
      # represented as a value between 0 and 1.
      if(filter_type == "butt_filter"){
        
        filter <- butter(1, 10 / (0.5 * sampling_frequency), "low")
        
        # filtfilt for autoregressive filtering
        trial_data <- trial_data[, lapply(.SD, function(x) filtfilt(filter, x))
                                 ][, total_force := fp1 + fp2]
      }
      else{
        
        # 10-point moving average filter
        trial_data <- trial_data[, lapply(.SD, function(x) rollapplyr(x, 10, mean, partial = TRUE))
                                 ][, total_force := fp1 + fp2]
      }
    }
    else{
      trial_data[, total_force := fp1 + fp2]
    }
    
    # Checks the beginning and end of each trial and removes any data where the force is < 440N (45kg)
    # Makes it OK for the athlete to not be on the force plates at the beginning of the trial
    # detect_index returns the index value of the first point satisfying the criterion.
    # the .dir = "backward" argument causes the function to search in reverse
    start_index <- detect_index(trial_data[, total_force], 
                                ~ .x >= 440)
    
    end_index <- detect_index(trial_data[, total_force], 
                              ~ .x >= 440, 
                              .dir = "backward")
    
    # Constrain the trial between the two index values found above as necessary
    trial_data <- trial_data[start_index:end_index]
    
    # Instead of using changepoints, I'm trialing pracma's findpeaks. I can't remember who shared their idea for this,
    # but I know I shared findpeaks with them first...Regardless, we can use findpeaks to find peak propulsive force
    # and peak landing force. There's a chance this approach will return more peaks than we want, however,
    # so we need to do a little work to determine which peaks are actually associated with takeoff and landing.
    # We do that by counting the number of points below 10N and removing any pair not between 202 and 903 ms,
    # equivalent to jh_ft of 5cm and 100cm. In theory, this should remove points where the athlete unloads during the unweighting phase
    # or steps of the plates / falls off the plates at the end of a trial.
    data_peaks <- trial_data[, as.data.table(findpeaks(total_force,
                                                       minpeakdistance = round(500 * (sampling_frequency / 1000)),
                                                       minpeakheight = max(c(max(fp1), max(fp2))) * 0.4))
                             ][order(V2)
                               ][, .(start = shift(V2,
                                                   fill = 1),
                                     end = V2)]
    
    data_peaks <- data_peaks[, below_threshold := 
                               unlist(
                                 lapply(1:nrow(.SD), 
                                        function(x){
                                          trial_data[1:nrow(trial_data) %between% .SD[x], sum(total_force < 10)]
                                        }))
                             ][below_threshold %between% c(round(202 * (sampling_frequency / 1000)), 
                                                           round(903 * (sampling_frequency / 1000)))]
    
    # Finds the index value for the final instance the athlete's total force is above 10N. R is indexed on 1,
    # so we have to subtract 2 from the value for this to actually coincide with the final point in the data.
    takeoff_index <- detect_index(trial_data[data_peaks$start:nrow(trial_data), 
                                             total_force],
                                  ~ .x < 10) + data_peaks$start - 2
    
    # Landing index, however, doesn't need any additional addition or subtraction as we want the first point the athlete's force
    # is above 10N
    landing_index <- detect_index(trial_data[data_peaks$start:data_peaks$end,
                                             total_force],
                                  ~ .x < 10,
                                  .dir = "backward") + data_peaks$start
    
    # We want to cut a portion of the flight phase away before calculating mean force and sd of force to (hopefully) account for
    # ringing in the plates at takeoff. I rather arbitrarily chose 25% from each side. That is, the middle 50% of the data
    # are used to calculate mean and SD to adjust takeoff and landing. Similar to the literature, 5x the SD of the flight
    # force is used as our threshold for takeoff and landing.
    flight_adjust <- (landing_index - takeoff_index) * 0.25
    
    flight_statistics <- trial_data[(takeoff_index + flight_adjust):(landing_index - flight_adjust),
                                    .(mean = mean(total_force),
                                      sd = sd(total_force) * 5)
                                    ][, threshold := mean + sd]
    
    # Now, let's adjust takeoff_index and landing_index
    takeoff_index <- detect_index(trial_data[data_peaks$start:nrow(trial_data), 
                                             total_force],
                                  ~ .x < flight_statistics$threshold) + data_peaks$start - 2
    
    landing_index <- detect_index(trial_data[data_peaks$start:data_peaks$end,
                                             total_force],
                                  ~ .x < flight_statistics$threshold,
                                  .dir = "backward") + data_peaks$start
    
    # We're going to find a 1 second interval with the lowest variance; this is going to be
    # quiet standing and will be used to calculate our initiation threshold
    # cpt.var calculates "changepoints" in the variance of the force data across minimum lengths (minseglen)
    suppressWarnings({
      var_changepoints <- cpt.var(trial_data[, total_force],
                                  method = "BinSeg",
                                  minseglen = round(1000 * (sampling_frequency / 1000)),
                                  Q = 5,
                                  class = FALSE)
    })
    
    # The above returned a vector, so we want to create a data.table of the time point pairs within the vector
    var_changepoints <- data.table(cbind(cp1 = shift(var_changepoints,
                                                     fill = 1),
                                         cp2 = var_changepoints))
    
    # This looks like a complete and utter cluster...and it kinda is. But we want to find the changepoint
    # with the lowest standard deviation in its force data. So we use this lapply along with which.min
    # to find the changepoint pair. This changepoint pair is our quiet standing and sets our initial
    # initiation threshold
    most_stable_changepoint <- trial_data[, list(lapply(1:nrow(var_changepoints), function(x){
      trial_data[1:nrow(trial_data) %between% var_changepoints[x, .(cp1, cp2)],
                 sd(total_force)]
    }))][, which.min(V1)]
    
    # Armed with our trusty stable changepoint, we can constrain our data; we'll need to update peak force, takeoff, landing,
    # and peak landing force as well. +1 to each since R is indexed on 1.
    trial_data <- trial_data[var_changepoints[most_stable_changepoint, cp1]:nrow(trial_data)]
    
    takeoff_index <- takeoff_index - var_changepoints[most_stable_changepoint, cp1] + 1
    
    landing_index <- landing_index - var_changepoints[most_stable_changepoint, cp1] + 1
    
    peak_force_index <- trial_data[1:takeoff_index, which.max(total_force)]
    
    peak_landing_force_index <- trial_data[, which.max(total_force)]
    
    minimum_force_index <- trial_data[1:peak_force_index, which.min(total_force)]
    
    # Save relevant information to a list; this will be passed along in a reactive context for further processing
    trial_list <- list(trial_data = trial_data,
                              minimum_force_index = minimum_force_index,
                              peak_force_index = peak_force_index,
                              flight_threshold = flight_statistics$threshold,
                              takeoff_index = takeoff_index,
                              landing_index = landing_index,
                              peak_landing_force_index = peak_landing_force_index)
    
    return(trial_list)
  }
  #### Long data analysis ####
  # Get ready for a lot of the same song and dance early on. This diverges from the wide format data
  # once we get to event detection since we're trying to detect all pairs of jump takeoff and landing in a long file.
  # This is still experimental
  else{
    
    data <- uploaded_data
    
    setnames(data, c("fp1", "fp2"))
    
    data <- data[, ":=" (fp1 = fp1 * fp1_slope + fp1_intercept,
                         fp2 = fp2 * fp2_slope + fp2_intercept)
                 ][, na.omit(.SD)]
    
    # Filter implementation; if not "no filter", a check for filter type occurs and the appropriate filter is applied
    if(filter_type != "no_filter"){
      
      # The first implemented type is a Butterworth filter; in this case, second order with a 10Hz cutoff
      # Since this is a digital filter, we need to know the sampling frequency as the cutoff frequency is
      # represented as a value between 0 and 1.
      if(filter_type == "butt_filter"){
        
        filter <- butter(1, 10 / (0.5 * sampling_frequency), "low")
        
        # filtfilt for autoregressive filtering
        data <- data[, lapply(.SD, function(x) filtfilt(filter, x))
                     ][, total_force := fp1 + fp2]
      }
      else{
        
        # 10-point moving average filter
        data <- data[, lapply(.SD, function(x) rollapplyr(x, 10, mean, partial = TRUE))
                     ][, total_force := fp1 + fp2]
      }
    }
    else{
      data[, total_force := fp1 + fp2]
    }
    
    # This is still here for posterity's sake, but it's much less important than in wide data.
    start_index <- detect_index(data[, total_force], 
                                ~ .x >= 440)
    
    end_index <- detect_index(data[, total_force], 
                              ~ .x >= 440, 
                              .dir = "backward")
    
    data <- data[start_index:end_index]
    
    # Divergence begins here. Unlike the wide data, there will be multiple takeoff and landing force peaks.
    # As a result, we need to find all of said peaks. We again leverage findpeaks before doing things a little differently.
    data_peaks <- data[, as.data.table(findpeaks(total_force,
                                                 minpeakdistance = round(500 * (sampling_frequency / 1000)),
                                                 minpeakheight = max(c(max(fp1), max(fp2))) * 0.4))
                       ][order(V2)
                         ][, .(start = shift(V2,
                                             fill = 1),
                               end = V2)]
    
    # Again, we remove any pairs of peaks where the time between them isn't between 202 and 903 ms
    data_peaks <- data_peaks[, below_threshold := 
                               unlist(
                                 lapply(1:nrow(.SD), 
                                        function(x){
                                          data[1:nrow(data) %between% .SD[x], sum(total_force < 10)]
                                        }))
                             ][below_threshold %between% c(round(202 * (sampling_frequency / 1000)), 
                                                           round(903 * (sampling_frequency / 1000)))]
    
    # Unlike before, though, we want to loop through each of the trials identified via the peaks
    trial_list <- lapply(1:nrow(data_peaks), function(x){
      
      # We need some extra data on the ends, so I rather arbitrarily added a window of 4 seconds on the front end
      # and half a second on the back end
      trial_data <- data[1:nrow(data) %between% data_peaks[x, list(start - round(3999 * (sampling_frequency / 1000)),
                                                                   end + round(500 * (sampling_frequency / 1000)))]]
      
      # As a result, peak force index always occurs at 4 seconds
      peak_force_index <- round(4000 * (sampling_frequency / 1000))
      
      # And peak landing force is always half a second from the end of the trial
      # Note to self: Not an OOP error.
      peak_landing_force_index <- round(nrow(trial_data) - 500 * (sampling_frequency / 1000))
      
      #### From here, things go back to normal. The names of some points are a little different now, but
      #### the premises are still the same.
      
      # Finds the index value for the final instance the athlete's total force is above 10N. R is indexed on 1,
      # so we have to subtract 2 from the value for this to actually coincide with the final point in the data.
      takeoff_index <- detect_index(trial_data[peak_force_index:nrow(trial_data), 
                                               total_force],
                                    ~ .x < 10) + peak_force_index - 2
      
      # Landing index, however, doesn't need any additional addition or subtraction as we want the first point the athlete's force
      # is above 10N
      landing_index <- detect_index(trial_data[peak_force_index:peak_landing_force_index,
                                               total_force],
                                    ~ .x < 10,
                                    .dir = "backward") + peak_force_index
      
      # We want to cut a portion of the flight phase away before calculating mean force and sd of force to (hopefully) account for
      # ringing in the plates at takeoff. I rather arbitrarily chose 25% from each side. That is, the middle 50% of the data
      # are used to calculate mean and SD to adjust takeoff and landing. Similar to the literature, 5x the SD of the flight
      # force is used as our threshold for takeoff and landing.
      flight_adjust <- (landing_index - takeoff_index) * 0.25
      
      flight_statistics <- trial_data[(takeoff_index + flight_adjust):(landing_index - flight_adjust),
                                      .(mean = mean(total_force),
                                        sd = sd(total_force) * 5)
                                      ][, threshold := mean + sd]
      
      # Now, let's adjust takeoff_index and landing_index
      takeoff_index <- detect_index(trial_data[peak_force_index:nrow(trial_data), 
                                               total_force],
                                    ~ .x < flight_statistics$threshold) + peak_force_index - 2
      
      landing_index <- detect_index(trial_data[peak_force_index:peak_landing_force_index,
                                               total_force],
                                    ~ .x < flight_statistics$threshold,
                                    .dir = "backward") + peak_force_index
      
      # We're going to find a 1 second interval with the lowest variance; this is going to be
      # quiet standing and will be used to calculate our initiation threshold
      # cpt.var calculates "changepoints" in the variance of the force data across minimum lengths (minseglen)
      suppressWarnings({
        var_changepoints <- cpt.var(trial_data[, total_force],
                                    method = "BinSeg",
                                    minseglen = round(1000 * (sampling_frequency / 1000)),
                                    Q = 5,
                                    class = FALSE)
      })
      
      # The above returned a vector, so we want to create a data.table of the time point pairs within the vector
      var_changepoints <- data.table(cbind(cp1 = shift(var_changepoints,
                                                       fill = 1),
                                           cp2 = var_changepoints))
      
      # This looks like a complete and utter cluster...and it kinda is. But we want to find the changepoint
      # with the lowest standard deviation in its force data. So we use this lapply along with which.min
      # to find the changepoint pair. This changepoint pair is our quiet standing and sets our initial
      # initiation threshold
      most_stable_changepoint <- trial_data[, list(lapply(1:nrow(var_changepoints), function(x){
        trial_data[1:nrow(trial_data) %between% var_changepoints[x, .(cp1, cp2)],
                   sd(total_force)]
      }))][, which.min(V1)]
      
      # Armed with our trusty stable changepoint, we can constrain our data; we'll need to update peak force, takeoff, landing,
      # and peak landing force as well. +1 to each since R is indexed on 1.
      trial_data <- trial_data[var_changepoints[most_stable_changepoint, cp1]:nrow(trial_data)]
      
      takeoff_index <- takeoff_index - var_changepoints[most_stable_changepoint, cp1] + 1
      
      landing_index <- landing_index - var_changepoints[most_stable_changepoint, cp1] + 1
      
      peak_force_index <- trial_data[1:takeoff_index, which.max(total_force)]
      
      peak_landing_force_index <- trial_data[, which.max(total_force)]
      
      minimum_force_index <- trial_data[1:peak_force_index, which.min(total_force)]
      
      trial_information <- list(trial_data = trial_data,
                                minimum_force_index = minimum_force_index,
                                peak_force_index = peak_force_index,
                                flight_threshold = flight_statistics$threshold,
                                takeoff_index = takeoff_index,
                                landing_index = landing_index,
                                peak_landing_force_index = peak_landing_force_index)
    })
    
    return(trial_list)
  }
}

sj_table_headers <- c("Body Mass", "Flight Time", "Net Impulse", "Jump Height (FT)",
                      "Jump Height (NI)", "Takeoff Velocity", "Peak Force", "Peak Velocity",
                      "Peak Power", "Force @ Peak Power", "Velocity @ Peak Power",
                      "Time to Peak Force", "Avg RFD", "Contact Time", "RSI Modified",
                      "FP1 Net Impulse", "FP2 Net Impulse", "Net Impulse SI",
                      "FP1 Peak Force", "FP2 Peak Force", "Peak Force SI",
                      "FP1 Time to Peak Force", "FP2 Time to Peak Force",
                      "Time to Peak Force SI", "FP1 Avg RFD", "FP2 Avg RFD", "Avg RFD SI")

cmj_table_headers <- c(sj_table_headers, "Unweighting Duration", "Braking Duration", "Concentric Duration",
                       "Force @ Zero Velo")

save_headers <- c("date",
                  "name",
                  "jump_type",
                  "trial_number",
                  "bar_load",
                  cmj_table_headers)

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