pacman::p_load(shiny, shinydashboard, data.table, tidytable, signal,
               changepoint, pracma, plotly, knitr, shinyjs, kableExtra)

# Accepts files up to 40 mB in size; adjust based on your file size requirements
options(shiny.maxRequestSize = 40 * 1024 ^ 2)
options(knitr.table.format = "html")

# Misc definitions ----
# plotly doesn't contain a v/hline function by default, so we have to add
# them via shapes.
vline <- function(x = 0, color = "black") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(
      color = color,
      dash = "dot",
      width = 1
    )
  )
}

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

# Instead of having a bunch of extra, unsightly text boxes on the interpolation
# tab, this controls what inputs appear based on the selected jump type
# and interpolation method.
interpolation_ui <- function(jump_type, method, combine_phases) {
  if (jump_type == "sj" | method == "linear") {
    textInput(inputId = "curve_length",
              label = "Curve")
  } else {
    if (combine_phases) {
      splitLayout(
        textInput(inputId = "stretching_length",
                  label = "Stretching"),
        textInput(inputId = "propulsion_length",
                  label = "Propulsion")
      )
    } else {
      splitLayout(
        textInput(inputId = "unweighting_length",
                  label = "Unweighting"),
        textInput(inputId = "braking_length",
                  label = "Braking"),
        textInput(inputId = "propulsion_length",
                  label = "Propulsion")
      )
    }
  }
}

# save file headers ----
sj_table_headers <-
  c("Body Mass", "Flight Time", "Net Impulse", 
    "Jump Height (FT)", "Jump Height (NI)", 
    "Takeoff Velocity", "Peak Force", "Peak Velocity",
    "Peak Power", "Force @ Peak Power", 
    "Velocity @ Peak Power", "Time to Peak Force", 
    "Avg RFD", "Contact Time", "RSI Modified",
    "Left Net Impulse", "Right Net Impulse", "Net Impulse SI",
    "Left Peak Force", "Right Peak Force", "Peak Force SI",
    "Left Time to Peak Force", "Right Time to Peak Force",
    "Time to Peak Force SI", "Left Avg RFD", 
    "Right Avg RFD", "Avg RFD SI")

cmj_table_headers <- 
  c(sj_table_headers, "Unweighting Duration", "Braking Duration", 
    "Propulsive Duration", "Peak Braking Force", "Avg Braking Force", 
    "Min Braking Velocity", "Avg Braking Velocity", "Peak Braking Power", 
    "Avg Braking Power", "Braking Work", "Force @ Zero Velo",
    "Peak Propulsive Force", "Avg Propulsive Force", 
    "Peak Propulsive Velocity", "Avg Propulsive Velocity",
    "Peak Propulsive Power", "Avg Propulsive Power", "Propulsive Work",
    "Braking RFD", "Left Braking RFD", "Right Braking RFD", "Braking RFD SI",
    "Decel RFD", "Left Decel RFD", "Right Decel RFD", "Decel RFD SI")

save_headers <- 
  c("date", "name", "jump_type", "trial_number", "bar_load", cmj_table_headers)

# Data processing ----
# The app allows multiple file formats to be uploaded. Broadly, these formats
# are either Pasco file format...or everything else.
# Here, we're checking for decimals
# as commas since that's a format used in some countries. R is supposed to
# be smart enough to recognize this, but I've had some users message me with
# problems related to this in the past. So, we start off the data import
# process with a check for commas as decimals. If they're found, the
# mutate fixes this.
parse_file <- function(file_location, upload_type) {
  comma_check <-
    sum(fread(file = file_location,
              skip = ifelse.(upload_type == "Pasco", 1, 0),
              header = TRUE,
              nrows = 1)[1] %like% ",")
  
  data <-
    fread(file = file_location,
          skip = ifelse.(upload_type == "Pasco", 1, 0),
          header = TRUE,
          na.strings = c("", "NA"),
          check.names = TRUE,
          stringsAsFactors = FALSE)
  
  if (upload_type == "Pasco") {
    data <-
      data %>% 
      select.(matches("Normal"))
  }
  
  if (comma_check > 0) {
    data <-
      data %>% 
      mutate.(across.(.cols = everything(),
                      ~ as.numeric(sub(",", ".", .x, fixed = TRUE))))
  }
  
  return(data)
}

# Applies a calibration equation to your data...if you have a calibration
# equation. Likewise, applies the selected filter type to the data.
# The default is no filter, but the two additional options are
# a 50 Hz, 4th order, low-pass Butterworth filter and a
# moving average filter with a width of 10. Default is no filter.
apply_calibration <- function(.df, filter_type, sampling_rate,
                              fp1_slope, fp1_intercept, 
                              fp2_slope, fp2_intercept) {
  
  trial_data <- 
    .df %>% 
    select.(fp1 = 1, fp2 = 2) %>% 
    drop_na.() %>% 
    mutate.(time = row_number.() / sampling_rate,
            fp1 = fp1 * fp1_slope + fp1_intercept,
            fp2 = fp2 * fp2_slope + fp2_intercept) %>% 
    relocate.(time, .before = fp1)
  
  if (filter_type == "no_filter") {
    trial_data <-
      trial_data %>% 
      mutate.(total_force = fp1 + fp2)
  } else if (filter_type == "butt_filter") {
    filter <-
      butter(4, 50 / (0.5 * sampling_rate), "low")
    
    trial_data <-
      trial_data %>% 
      mutate.(across.(.cols = fp1:fp2,
                      ~ filtfilt(filter, .x)),
              total_force = fp1 + fp2)
  } else {
    trial_data <-
      trial_data %>% 
      mutate.(across.(.cols = fp1:fp2,
                      ~ as.numeric(stats::filter(.x, rep(1 / 10, 10), 
                                                 sides = 2))),
              total_force = fp1 + fp2)
  }
  
  return(trial_data)
}

# Peak detection is one half of the jump analysis process. Whether we're
# analyzing wide data, single-trial data, or continuous data, we can assume
# we're going to see the same peaks since vertical jumps have a relatively
# regular profile.
# As such, we're actually looking for peak pairs--one representing peak
# takeoff force and one representing peak landing force. The {pracma}
# package contains a very handy function called findpeaks(), which...
# well...finds peaks in a vector.
# Importantly, we can set minimum thresholds, widths, etc. for these peaks
# to help weed out erroneous events, such as the athletes stepping on the
# plates.
detect_peaks <- function(.df, sampling_rate) {
  # That last bit of the above explanation is handled here. We want to prevent
  # findpeaks from returning erroneous peaks, so we start by defining a minimum
  # peak height threshold. This used to be the max of the max of each limb,
  # but I modified it to the max of the mean of the 10 highest points from each
  # limb. This came after some work on an automated process, where I was
  # still occasionally catching some bad peaks.
  # We multiply this value by 0.4 to give a relatively generous starting
  # value.
  min_peak_height <-
    .df[, max(c(mean(tail(sort(fp1), 10)),
                  mean(tail(sort(fp2), 10)))) * 0.4]
  
  # Just as an FYI, repeat checks its condition at the end of its loop,
  # while a while checks at the beginning. This could be rewritten as a while,
  # but I don't think the speed gain would be all that noticeable.
  repeat {
    data_peaks <-
      .df[, as.data.table(
        findpeaks(
          total_force,
          # Requires peaks to be at least 250 points from one another.
          # This prevents multiple peaks being caught immediately adjacent to
          # one another
          minpeakdistance = round(sampling_rate * 0.25),
          minpeakheight = min_peak_height,
          zero = "-"))] %>% 
      arrange.(V2) %>% 
      summarize.(start = shift(V2, fill = 1),
                 end = V2) %>% 
      filter.(start != 1)
    
    # After creating the data_peaks tidytable above, we iterate through it here
    # to determine if we've accurately captured our peaks (remember, takeoff
    # and landing) and to throw out erroneously identified peaks.
    # The two biggies are going to be the time_diff and below_threshold.
    peak_pairs <-
      data_peaks[, map2_df.(start, end, function(x, y) {
        .df[seq_len(nrow(.df)) %between% c(x, y),
              .(start_time = first(time),
                end_time = last(time),
                time_diff = last(time) - first(time),
                below_threshold = sum(total_force <= 10))]
      })] %>% 
      # below_threshold is first. Only peak pairs where there are 202-903ms
      # below 10N are retained. This removes cases where the athletes are
      # stepping off and on the plates, etc.
      filter.(below_threshold %between% round(c(sampling_rate * 0.202,
                                                sampling_rate * 0.903)))
    
    # time_diff is our other major player. Once we've thrown out peak pairs
    # with extremely low/high below_threshold values, we also want to make sure
    # we haven't captured a landing-takeoff pair instead of a takeoff-landing
    # pair. This is only really relevant to the Multiple Trials - Long file
    # format, so the other file types will get passed through this repeat
    # in the first loop iteration.
    # The first check is if all peak pairs have more than 1.25 s between them.
    # If so, it will automatically decrease the min_peak_height and force a
    # repeat{}.
    if (all(peak_pairs[, time_diff] > 1.25))
      min_peak_height <- min_peak_height * 0.9
    else if (any(peak_pairs[, time_diff] > 1.25)) {
      # Likewise, if some of the peak pairs are > 1.25 s, this checks if more
      # than 1/3 of the pairs are outside 1.25s. If so, forces a repeat{}.
      # Otherwise, drops the peak pairs > 1.25s
      above_limit_count <- peak_pairs[time_diff > 1.25, .N]
      
      if (above_limit_count >= nrow(peak_pairs) * 0.3)
        min_peak_height <- min_peak_height * 0.9
      else
        peak_pairs <- peak_pairs[!(time_diff > 1.25)]
    }
    
    # Once all peak pairs have a time_diff < 1.25, breaks the repeat{} and
    # returns the force-time data between the takeoff peak and landing peak.
    # Importantly, the takeoff is padded with up to 4 seconds of lead time
    # and up to 0.5 are added to the trailing end of the trial. The lead
    # time will (hopefully) help capture the correct quiet standing phase
    # prior to jump initiation.
    if (!any(peak_pairs[, time_diff] > 1.25))
      return(peak_pairs[, .(start_time, end_time,
                            start_window = start_time - 4,
                            end_window = end_time + 0.5)])
  }
}

# Preps the uploaded data for further analysis. We convert the csv or txt to
# a list of lists that contains the (calibrated?) trial data alongside
# the peak takeoff force time and peak landing force time. I originally wasn't
# returning those last two and was relying on those locations being
# 4 seconds into the data and a half second from the end of the data, but
# not all trials are going to have that sort of lead and lag time. Hence,
# errors, errors everywhere. Instead, I just return the time points instead
# so it's a non-issue.
# Also, unlike previous versions of the app, this will return a list regardless
# of the number of trials we're analyzing. This greatly simplifies the analysis
# process.
create_trial_list <- function(.df, upload_type, filter_type, sampling_rate,
                              plate_layout, fp1_slope, fp1_intercept,
                              fp2_slope, fp2_intercept) {
  if (upload_type == "Pasco" | upload_type == "Multiple Trials - Wide" |
      upload_type == "Single Trial") {
    trials <- ncol(.df) / 2
    
    trial_list <-
      map.(1:trials, function(x) {
        if (plate_layout == "lr") {
          left_col <- x * 2 - 1
          right_col <- x * 2
        } else {
          left_col <- x * 2
          right_col <- x * 2 - 1
        }
        
        # tidyselect syntax allows us to pass the column number instead of the
        # name.
        trial_data <-
          .df %>% 
          select.(!!left_col, !!right_col)
        
        calibrated_data <-
          apply_calibration(trial_data, filter_type, sampling_rate,
                            fp1_slope, fp1_intercept,
                            fp2_slope, fp2_intercept)
        
        peaks <-
          detect_peaks(calibrated_data, sampling_rate)
        
        return(
          list(
            trial_data = calibrated_data[time %between% c(peaks$start_window,
                                                          peaks$end_window)],
            peak_force_time = peaks[, start_time],
            peak_landing_force_time = peaks[, end_time]
          )
        )
      })
  } else {
    # The Multiple Trials - Long format is a hair different than the other
    # file formats. detect_peaks is going to return multiple peak pairs, so
    # we're going to iterate across those instead of iterating column-wise
    # like above.
    if (plate_layout == "rl") {
      .df <-
        .df %>% 
        select.(2, 1)
    }
    
    calibrated_data <-
      apply_calibration(.df, filter_type, sampling_rate,
                        fp1_slope, fp1_intercept,
                        fp2_slope, fp2_intercept)
    
    peaks <-
      detect_peaks(calibrated_data, sampling_rate)
    
    # This map. returns a list identical to the list returned
    # from the other method above.
    trial_list <-
      map.(seq_len(nrow(peaks)), function(x) {
        return(
          list(
            trial_data = calibrated_data[time %between% c(peaks[x, start_window],
                                                          peaks[x, end_window])],
            peak_force_time = peaks[x, start_time],
            peak_landing_force_time = peaks[x, end_time]
          )
        )
      })
  }
  
  return(trial_list)
}

# Once we have our trial_list from the above function, we want to identify
# takeoff, landing, and time @ minimum force (prior to time @ peak force).
# We also take an initial stab at finding quiet standing and further
# constraining the trial data.
detect_events <- function(trial_list, sampling_rate) {
  map.(trial_list, function(x) {
    trial_data <-
      x$trial_data
    
    peak_force_time <-
      x$peak_force_time
    
    landing_peak_force_time <-
      x$peak_landing_force_time
    
    # Importantly, we start by tentatively identifying takeoff and landing
    # as the first and last points <= 10 N between peak force and peak landing
    # force.
    takeoff_time <-
      trial_data[time %between% c(peak_force_time, landing_peak_force_time)
      ][total_force <= 10, first(time)]
    
    landing_time <-
      trial_data[time %between% c(takeoff_time, landing_peak_force_time)
      ][total_force <= 10, last(time)]
    
    # However, to deal with the possibility of noisy force plates, we're going
    # to look at the middle 50% of the identified flight time and calculate
    # the mean and SD of the force during that portion of flight.
    flight_duration <-
      landing_time - takeoff_time
    
    adjusted_flight_duration <-
      flight_duration * 0.25
    
    # We'll use those values to calculate an updated flight threshold...
    flight_statistics <-
      trial_data[time >= takeoff_time + adjusted_flight_duration &
                   time <= landing_time - adjusted_flight_duration] %>% 
      summarize.(mean = mean(total_force),
                 sd = sd(total_force)) %>% 
      mutate.(threshold = mean + sd * 5)
    
    # ...where takeoff and landing occur when the total_force passes
    # the newly calculated threshold value.
    takeoff_time <-
      trial_data[time %between% c(peak_force_time, landing_peak_force_time)
      ][total_force <= flight_statistics$threshold, first(time)]
    
    landing_time <-
      trial_data[time %between% c(takeoff_time, landing_peak_force_time)
      ][total_force <= flight_statistics$threshold, last(time)]
    
    # This takes an initial stab at identifying quiet standing. The
    # {changepoints} package is used to identify "changepoints" in the data,
    # where the mean and variance change compared to the preceding/following
    # data. I honestly don't fully understand the process behind
    # the cpt.meanvar() function, but it's served me well for the four-ish
    # years this app has existed.
    # At any rate, a minimum segment length of 1 second is required before a
    # changepoint can occur. This coincides quite nicely with the literature,
    # but can occasionally lead to some mis-identified quiet standing periods.
    # That's where the app's adjustment features will come in later.
    suppressWarnings({
      changepoints <-
        cpt.meanvar(trial_data[, total_force],
                    method = "BinSeg",
                    minseglen = sampling_rate,
                    # minseglen = round(1000 * (sampling_rate / 1000)), # What was I even doing here?
                    Q = 5,
                    class = FALSE)
    })
    
    changepoints <-
      data.table(cbind(cp1 = shift(changepoints, fill = 1),
                       cp2 = changepoints))
    
    # Loops through the changepoints identified above and determines the
    # variance and number of points <= 10 N between the changepoint pairs.
    # changepoint pairs with any force values <= 10 N are thrown out as we can
    # assume they're capturing part of the jump cycle or stepping
    # onto the force plates.
    # From there, we keep the pair with the lowest variance between them as
    # the likely candidate for quiet standing.
    weigh_start_time <-
      changepoints[, map2_df.(cp1, cp2, function(x, y) {
        trial_data[seq_len(nrow(trial_data)) %between% c(x, y),
                   .(weight_start = first(time),
                     variance = var(total_force),
                     below_threshold = sum(total_force <= 10))]
      })] %>% 
      filter.(below_threshold == 0) %>% 
      tidytable::dt(order(variance),
                    first(weight_start))
    
    trial_data <-
      trial_data[time >= weigh_start_time]
    
    # Minimum force prior to takeoff peak force is used in the jump
    # identification process a little later.
    minimum_force_time <-
      trial_data[time <= peak_force_time
      ][which.min(total_force), time]
    
    return(
      list(
        trial_data = trial_data,
        sampling_rate = sampling_rate,
        minimum_force_time = minimum_force_time,
        peak_force_time = peak_force_time,
        flight_threshold = flight_statistics$threshold,
        takeoff_time = takeoff_time,
        landing_time = landing_time,
        landing_peak_force_time = landing_peak_force_time
      )
    )
  })
}

process_jump <- function(.df, weight_override = NULL, quiet_standing_length,
                         jump_type = "auto", start_method = "5SD - BW",
                         inverse_check = TRUE) {
  # If the primary plot isn't brushed, weight_override is NULL. If it is, this
  # constrains the trial start to x[1] of the brush. That is, the beginning of
  # quiet standing is set to the start of your brush location on the
  # primary plot.
  # If you do brush the primary plot, minimum force time is also updated just
  # in case.
  if (!is.null(weight_override)) {
    trial_data <-
      .df$trial_data[time >= weight_override[1]]
    
    minimum_force_time <-
      trial_data[time <= .df$peak_force_time
      ][which.min(total_force), time]
  } else {
    trial_data <- .df$trial_data
    
    minimum_force_time <- .df$minimum_force_time
  }
  
  sampling_rate <- .df$sampling_rate
  peak_force_time <- .df$peak_force_time
  flight_threshold <- .df$flight_threshold
  takeoff_time <- .df$takeoff_time
  landing_time <- .df$landing_time
  landing_peak_force_time <- .df$landing_peak_force_time
  
  quiet_standing_end_time <-
    trial_data[, first(time) + quiet_standing_length]
  
  body_weight <- trial_data[time <= quiet_standing_end_time, mean(total_force)]
  body_weight_sd <- trial_data[time <= quiet_standing_end_time, sd(total_force)]
  body_mass <- body_weight / 9.81
  fp1_weight <- trial_data[time <= quiet_standing_end_time, mean(fp1)]
  fp2_weight <- trial_data[time <= quiet_standing_end_time, mean(fp2)]
  
  minimum_force <-
    trial_data[time == minimum_force_time, total_force]
  
  # This if-else has always worked for me, but if you're dealing with
  # particularly light athletes, it may not work. If jumps are being
  # misidentified, I would suggest changing the jump type input from auto
  # to whatever the jump type you're analyzing is.
  if (jump_type == "auto") {
    if (body_weight - minimum_force > 250)
      jump_type <- "cmj"
    else
      jump_type <- "sj"
  } else {
    jump_type <- jump_type
  }
  
  # SJ and CMJ start identification essentially work in inverse of one another.
  # SJs look for the first point > body weight + 5SD, while CMJs look for
  # the first point < body weight - 5SD. 
  # Importantly, these points aren't the "true" jump start times, depending
  # on the research you're reading. Pre-jump movement and mechanical changes
  # that aren't caught by the plates occur prior to reaching the force threshold,
  # so the actual jump start may occur a hair before passing the threshold.
  if (jump_type == "sj") {
    initiation_threshold <- body_weight + body_weight_sd * 5
    inverse_threshold <- body_weight - body_weight_sd * 5
    
    jump_threshold_time <-
      trial_data[time <= peak_force_time
      ][total_force <= initiation_threshold,
        last(time)]
    
    # This process WAS NOT clear in the previous versions of the app, so I
    # added a selectInput where you can toggle the below inverse_check loop.
    # If the toggle is set to yes/true (the default), the below check happens:
    # Once the threshold time is identified, the app looks back an additional
    # 100 ms to determine if the "inverse threshold" was exceeded. If it was,
    # the app continues searching backward for the first point that matches
    # the selected jump start_method.
    if (inverse_check) {
      inverse_exceeded <-
        trial_data[time %between% c(jump_threshold_time - 0.1,
                                    jump_threshold_time),
                   min(total_force) < inverse_threshold]
      
      if (inverse_exceeded) {
        pre_movement_minimum_time <-
          trial_data[time %between% c(jump_threshold_time - 0.1,
                                      jump_threshold_time)
          ][which.min(total_force), time]
        
        # With 5SD - BW, it continues searching until it finds a point >=
        # body weight
        if (start_method == "5SD - BW") {
          jump_start_time <-
            trial_data[time <= pre_movement_minimum_time
            ][total_force >= body_weight, last(time)]
        } else {
          # Otherwise, it searches backward until it finds the first point
          # >= the inverse threshold and steps back an additional 30 ms.
          jump_start_time <-
            trial_data[time <= pre_movement_minimum_time
            ][total_force >= inverse_threshold,
              last(time) - 0.03]
        }
      } else {
        # If the inverse threshold isn't passed, the app searches back to find
        # the first point <= body weight OR
        if (start_method == "5SD - BW") {
          jump_start_time <-
            trial_data[time <= jump_threshold_time
            ][total_force <= body_weight, last(time)]
        } else {
          # Just steps back 30ms. These are generally close to one another,
          # outside weird edge-case jumps.
          jump_start_time <-
            jump_threshold_time - 0.03
        }
      }
    } else {
      # If you toggle off inverse_check, none of the inverse_check code
      # happens and the app proceeds like there wasn't an inverse_exceeded
      # event.
      if (start_method == "5SD - BW") {
        jump_start_time <-
          trial_data[time <= jump_threshold_time
          ][total_force <= body_weight, last(time)]
      } else {
        jump_start_time <-
          jump_threshold_time - 0.03
      }
    }
  } else {
    # Like I said above, CMJ start detection is literally the inverse of SJ
    # start detection, so the below code won't be commented.
    initiation_threshold <- body_weight - body_weight_sd * 5
    inverse_threshold <- body_weight + body_weight_sd * 5
    
    jump_threshold_time <-
      trial_data[time <= minimum_force_time
      ][total_force >= initiation_threshold,
        last(time)]
    
    if (inverse_check) {
      inverse_exceeded <-
        trial_data[time %between% c(jump_threshold_time - 0.1,
                                    jump_threshold_time),
                   max(total_force) > inverse_threshold]
      
      if (inverse_exceeded) {
        pre_movement_max_time <-
          trial_data[time %between% c(jump_threshold_time - 0.1,
                                      jump_threshold_time)
          ][which.max(total_force), time]
        
        if (start_method == "5SD - BW") {
          jump_start_time <-
            trial_data[time <= pre_movement_max_time
            ][total_force <= body_weight, last(time)]
        } else {
          jump_start_time <-
            trial_data[time <= pre_movement_max_time
            ][total_force <= inverse_threshold,
              last(time) - 0.03]
        }
      } else {
        if (start_method == "5SD - BW") {
          jump_start_time <-
            trial_data[time <= jump_threshold_time
            ][total_force >= body_weight, last(time)]
        } else {
          jump_start_time <-
            jump_threshold_time - 0.03
        }
      }
    } else {
      if (start_method == "5SD - BW") {
        jump_start_time <-
          trial_data[time <= jump_threshold_time
          ][total_force >= body_weight, last(time)]
      } else {
        jump_start_time <-
          jump_threshold_time - 0.03
      }
    }
  }
  
  # Jump metric calculations ----
  jump_data <-
    trial_data[time %between% c(jump_start_time, takeoff_time)] %>% 
    mutate.(
      impulse = cumtrapz(time, total_force - body_weight),
      fp1_impulse = cumtrapz(time, fp1 - fp1_weight),
      fp2_impulse = cumtrapz(time, fp2 - fp2_weight),
      velocity = impulse / !!body_mass,
      power = total_force * velocity
    )
  
  flight_time <- landing_time - takeoff_time
  net_impulse <- jump_data[, last(impulse)]
  jump_height_ft <- 0.5 * 9.81 * (flight_time / 2) ^ 2
  takeoff_velocity <- jump_data[, last(velocity)]
  jump_height_ni <- takeoff_velocity ^ 2 / (2 * 9.81)
  peak_force <- jump_data[, max(total_force)]
  peak_velocity <- jump_data[, max(velocity)]
  peak_power <- jump_data[, max(power)]
  force_peak_power <- jump_data[which.max(power), total_force]
  velocity_peak_power <- jump_data[which.max(power), velocity]
  time_to_peak_force <-
    jump_data[which.max(total_force), time] - jump_data[, first(time)]
  # Avg RFD is just onset to peak force. Added some additional RFD variables
  # for CMJs below.
  avg_rfd <- (peak_force - jump_data[, first(total_force)]) / time_to_peak_force
  contact_time <- jump_data[, last(time) - first(time)]
  rsi_modified <- jump_height_ni / contact_time
  
  # All the unilateral stuff uses %SI, with the calculation being
  # diff / sum * 100. Values > 0 favor the left, while values < 0 favor the right.
  fp1_net_impulse <- jump_data[, last(fp1_impulse)]
  fp2_net_impulse <- jump_data[, last(fp2_impulse)]
  net_impulse_symmetry_index <- 
    (fp1_net_impulse - fp2_net_impulse) / 
    (fp1_net_impulse + fp2_net_impulse) * 100
  fp1_peak_force <- jump_data[, max(fp1)]
  fp2_peak_force <- jump_data[, max(fp2)]
  peak_force_symmetry_index <-
    (fp1_peak_force - fp2_peak_force) /
    (fp1_peak_force + fp2_peak_force) * 100
  fp1_ttpf <-
    jump_data[which.max(fp1), time] - jump_data[, first(time)]
  fp2_ttpf <-
    jump_data[which.max(fp2), time] - jump_data[, first(time)]
  ttpf_symmetry_index <-
    (fp1_ttpf - fp2_ttpf) / (fp1_ttpf + fp2_ttpf) * 100
  fp1_avg_rfd <-
    (fp1_peak_force - jump_data[, first(fp1)]) / fp1_ttpf
  fp2_avg_rfd <-
    (fp2_peak_force - jump_data[, first(fp2)]) / fp2_ttpf
  avg_rfd_symmetry_index <-
    (fp1_avg_rfd - fp2_avg_rfd) / (fp1_avg_rfd + fp2_avg_rfd) * 100
  
  summary_data <-
    data.table(body_mass,
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
               rsi_modified,
               fp1_net_impulse,
               fp2_net_impulse,
               net_impulse_symmetry_index,
               fp1_peak_force,
               fp2_peak_force,
               peak_force_symmetry_index,
               fp1_ttpf,
               fp2_ttpf,
               ttpf_symmetry_index,
               fp1_avg_rfd,
               fp2_avg_rfd,
               avg_rfd_symmetry_index)
  
  # CMJs require some additional special processing. We need to identify
  # some additional events before performing further calculations.
  if (jump_type == "cmj") {
    unweight_end_time <-
      jump_data[time %between% c(minimum_force_time, peak_force_time)
      ][total_force <= jump_data[, first(total_force)], last(time)]
    
    braking_end_time <-
      jump_data[impulse < 0, last(time)]
    
    unweight_duration <-
      unweight_end_time - jump_data[, first(time)]
    
    braking_duration <-
      braking_end_time - jump_data[time > unweight_end_time, first(time)]
    
    propulsive_duration <-
      jump_data[, last(time)] - jump_data[time > braking_end_time, first(time)]
    
    zero_velo_time <-
      jump_data[velocity <= 0, last(time)]
    
    force_zero_velo <-
      jump_data[time == zero_velo_time, total_force]
    
    braking_variables <-
      jump_data[time %between% c(unweight_end_time, braking_end_time),
                .(peak_braking_force = max(total_force),
                  avg_braking_force = mean(total_force),
                  min_braking_velocity = min(velocity),
                  avg_braking_velocity = mean(velocity),
                  peak_braking_power = min(power),
                  avg_braking_power = mean(power))
      ][, braking_work := avg_braking_power * braking_duration]
    
    propulsive_variables <-
      jump_data[time > braking_end_time,
                .(peak_propulsive_force = max(total_force),
                  avg_propulsive_force = mean(total_force),
                  peak_propulsive_velocity = max(velocity),
                  avg_propulsive_velocity = mean(velocity),
                  peak_propulsive_power = max(power),
                  avg_propulsive_power = mean(power))
      ][, propulsive_work := avg_propulsive_power * propulsive_duration]
    
    # Depending on who/what jump analysis software provider you talk to,
    # there are different definitions of braking/eccentric/etc. The above
    # calculations are the "classical" versions, where the CMJ is broken
    # into unweighting, braking, and propulsion. The below calculations,
    # instead, break the "eccentric" portion of the jump into two different
    # categories: braking and deceleration.
    # Braking, in this context, is from minimum_force_time -> braking_end_time
    cmj_braking_rfd <-
      jump_data[time %between% c(minimum_force_time, braking_end_time),
                .(braking_rfd = (last(total_force) - first(total_force)) /
                    (last(time) - first(time)),
                  fp1_braking_rfd = (last(fp1) - first(fp1)) /
                    (last(time) - first(time)),
                  fp2_braking_rfd = (last(fp2) - first(fp2)) /
                    (last(time) - first(time)))
      ][, braking_rfd_symmetry_index :=
          (fp1_braking_rfd - fp2_braking_rfd) /
          (fp1_braking_rfd + fp2_braking_rfd) * 100]
    
    min_velo_time <-
      jump_data[time %between% c(minimum_force_time, takeoff_time)
      ][which.min(velocity), time]
    
    # Deceleration, on the other hand, extends from minimum velocity ->
    # the end of braking.
    cmj_decel_rfd <-
      jump_data[time %between% c(min_velo_time, braking_end_time),
                .(decel_rfd = (last(total_force) - first(total_force)) /
                    (last(time) - first(time)),
                  fp1_decel_rfd = (last(fp1) - first(fp1)) /
                    (last(time) - first(time)),
                  fp2_decel_rfd = (last(fp2) - first(fp2)) /
                    (last(time) - first(time)))
      ][, decel_rfd_symmetry_index :=
          (fp1_decel_rfd - fp2_decel_rfd) /
          (fp1_decel_rfd + fp2_decel_rfd) * 100]
    
    summary_data <-
      cbind(summary_data,
            unweight_duration,
            braking_duration,
            propulsive_duration,
            braking_variables,
            force_zero_velo,
            propulsive_variables,
            cmj_braking_rfd,
            cmj_decel_rfd,
            unweight_end_time,
            braking_end_time)
  }
  
  # plot_data spans quiet standing -> landing + 0.5s
  # subplot_data is only jump_start -> takeoff. It's also used for returning
  # the force-time curve data.
  # The metric_table is what will be saved to the summary csv.
  return(
    list(
      plot_data = trial_data,
      subplot_data = jump_data,
      plot_annotations = data.table(body_mass, body_weight, body_weight_sd,
                                    jump_type, initiation_threshold,
                                    flight_threshold, jump_start_time,
                                    jump_threshold_time, peak_force_time,
                                    takeoff_time, landing_time,
                                    landing_peak_force_time),
      metric_table = summary_data,
      sampling_rate = sampling_rate,
      jump_type = jump_type
    )
  )
}

# All the plotting nonsense that originally existed in the app.R has been
# moved here and turned into a function.
build_primary_secondary_plot <- function(.df = NULL, analyzed_data = NULL) {
  if (!is.null(.df)) {
    # This is for plotting the unprocessed trial data, so it's much
    # simpler than the else{} below.
    # We attach an event listener for plot brushing.
    plot <-
      .df$trial_data %>% 
      plot_ly(x = ~ time,
              y = ~ total_force,
              source = "plot_brush") %>% 
      add_lines(name = "Total Force") %>% 
      layout(xaxis = list(title = "Time",
                          fixedrange = TRUE),
             yaxis = list(title = "Force (N)",
                          fixedrange = TRUE),
             dragmode = "select") %>% 
      event_register(event = "plotly_brushed") %>% 
      config(displayModeBar = FALSE)
  } else {
    # This is for plotting the processed data...quite a bit more in-depth.
    ann <- analyzed_data$plot_annotations
    
    plot <-
      analyzed_data$plot_data %>% 
      plot_ly(x = ~ time) %>% 
      add_lines(y = ~ fp1,
                name = "Left") %>% 
      add_lines(y = ~ fp2,
                name = "Right") %>% 
      add_lines(y = ~ total_force,
                name = "Total Force") %>% 
      add_annotations(x = 0,
                      y = 1,
                      xref = "paper",
                      yref = "paper",
                      showarrow = FALSE,
                      text = paste0(
                        "Body Mass (kg): ", round(ann$body_mass, 2),
                        "<br>Body Weight (N): ", round(ann$body_weight, 2),
                        "<br>Body Weight SD (N): ", round(ann$body_weight_sd, 2),
                        "<br>Jump Type: ", toupper(ann$jump_type),
                        "<br>Initiation Threshold (N): ",
                        round(ann$initiation_threshold, 2),
                        "<br>Flight Threshold (N): ",
                        round(ann$flight_threshold, 2)
                      )) %>% 
      layout(
        shapes = list(
          vline(ann$jump_start_time),
          vline(ann$jump_threshold_time),
          vline(ann$peak_force_time),
          vline(ann$takeoff_time),
          vline(ann$landing_time),
          vline(ann$landing_peak_force_time),
          hline(x0 = analyzed_data$plot_data[, first(time)],
                x1 = ann$jump_threshold_time,
                y = ann$initiation_threshold),
          hline(x0 = ann$takeoff_time,
                x1 = ann$landing_time,
                y = ann$flight_threshold),
          # Plots a rectangle around the weight +/- 5SD identified as quiet
          # standing. This helps visually identify if the quiet standing
          # specification is correct.
          list(
            type = "rect",
            fillcolor = "blue",
            line = list(color = "blue"),
            opacity = 0.3,
            x0 = analyzed_data$plot_data[, first(time)],
            x1 = ann$jump_threshold_time,
            xref = "x",
            y0 = ann$body_weight - ann$body_weight_sd * 5,
            y1 = ann$body_weight + ann$body_weight_sd * 5,
            yref = "y"
          )
        ),
        xaxis = list(title = "Time"),
        yaxis = list(title = "Force (N)")
      ) %>% 
      config(displayModeBar = FALSE)
  }
  
  return(plot)
}

# Used for creating the plots in the "Quick View" tab in the far right box.
# SJs require less processing than CMJs.
build_sj_subplots <- function(.df, variable) {
  
  # I was way too deep into coding this when I realized I had total_force as
  # one of the variables. To avoid creating an additional dependency on
  # {stringr}, we leverage a little base R here to convert total_force,
  # velocity, and power to Total Force, Velocity, and Power.
  axis_title <-
    gsub("_", " ", variable) %>% 
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", perl = TRUE, x = .)
  
  plot <-
    plot_ly(.df$subplot_data,
            x = ~ time,
            y = ~ get(variable)) %>% 
    add_lines(name = variable,
              showlegend = FALSE) %>% 
    layout(yaxis = list(title = axis_title))
  
  return(plot)
}

build_cmj_subplots <- function(.df, variable) {
  axis_title <-
    gsub("_", " ", variable) %>% 
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", perl = TRUE, x = .)
  
  plot_data <-
    .df$subplot_data
  
  unweight_end_time <-
    .df$metric_table$unweight_end_time
  
  braking_end_time <-
    .df$metric_table$braking_end_time
  
  # The first version of the app painstakingly went through and added
  # fills for each phase of the CMJ's force-time plot. I wizened up and decided
  # to convert the total_force plot to net force instead so all plots could
  # leverage the fill = "tozeroy" argument.
  if (variable == "total_force") {
    plot_data <-
      plot_data %>% 
      mutate.(total_force = total_force - first(total_force))
  }
  
  plot <-
    plot_ly(type = "scatter",
            mode = "lines") %>% 
    # The unweighting phase is filled red.
    add_trace(data = plot_data[time <= unweight_end_time],
              x = ~ time,
              y = ~ get(variable),
              fill = "tozeroy",
              color = I("red"),
              showlegend = FALSE) %>% 
    # The braking phase (not the modified one) is orange.
    add_trace(data = plot_data[time > unweight_end_time &
                                 time <= braking_end_time],
              x = ~ time,
              y = ~ get(variable),
              fill = "tozeroy",
              color = I("orange"),
              showlegend = FALSE) %>% 
    # The propulsive phase is filled blue.
    add_trace(data = plot_data[time > braking_end_time],
              x = ~ time,
              y = ~ get(variable),
              fill = "tozeroy",
              color = I("blue"),
              showlegend = FALSE) %>% 
    layout(yaxis = list(title = axis_title))
  
  return(plot)
}

build_metric_plots <- function(.df) {
  if (.df$jump_type == "sj") {
    plot <-
      subplot(build_sj_subplots(.df, "total_force"),
              build_sj_subplots(.df, "velocity"),
              build_sj_subplots(.df, "power"),
              nrows = 3,
              titleY = TRUE)
  } else {
    plot <-
      subplot(build_cmj_subplots(.df, "total_force"),
              build_cmj_subplots(.df, "velocity"),
              build_cmj_subplots(.df, "power"),
              nrows = 3,
              titleY = TRUE)
  }
  
  return(
    plot %>% 
      config(displayModeBar = FALSE)
  )
}

build_metric_table <- function(.df) {
  # With how I've coded this function, overwriting the data that are in memory
  # _shouldn't_ be a problem like it was in a previous version of the app,
  # but I'd rather not take chances, so we'll take a shallow copy of the
  # metric_table prior to operating on it.
  table_data <-
    copy(.df$metric_table)
  
  jump_type <-
    .df$plot_annotations$jump_type
  
  if (jump_type == "sj")
    headers <- sj_table_headers
  else {
    table_data <-
      table_data %>% 
      select.(-c(unweight_end_time, braking_end_time))
    
    headers <- cmj_table_headers
  }
  
  setnames(table_data, headers)
  
  # Pivots the entire table to long format.
  long_table <-
    table_data %>% 
    pivot_longer.(names_to = "Variable", values_to = "Value")
  
  # Ironically, we drop the column names, but that's neither here nor there.
  output_table <-
    kable(long_table, digits = 3, col.names = NULL) %>% 
    kable_styling(bootstrap_options = "striped") %>% 
    pack_rows("Bilateral Variables",
              start_row = 1,
              end_row = 15,
              label_row_css = "background-color: #004687; color: #fff;") %>% 
    pack_rows("Unilateral Variables",
              start_row = 16,
              end_row = 27,
              label_row_css = "background-color: #004687; color: #fff;")
  
  if (jump_type == "cmj") {
    output_table <-
      output_table %>% 
      pack_rows("CMJ-Specific",
                start_row = 28,
                end_row = 53,
                label_row_css = "background-color: #004687; color: #fff;")
  }
  
  output_table <-
    output_table %>% 
    scroll_box(width = "100%",
               height = "750px")
  
  return(output_table)
}

# One of two new additions to the app! This saves the raw force-time data
# for a given trial to a csv. A previous iteration saved the data in wide
# format (each column represents a point in the trial), but importing
# variable-length curves is an absolute nightmare. So, instead, I've modified
# the function to return the data in long format. That is, each row represents
# a data point. To ensure it's possible to perform interpolation and all that
# good stuff on the data, some identifying information is also attached to
# the data prior to export.
save_raw_curve <- function(.df, date, name, 
                           trial_number, bar_load) {
  
  raw_data <-
    .df$subplot_data %>% 
    mutate.(date = date,
            name = name,
            jump_type = .df$plot_annotations$jump_type,
            trial_number = trial_number,
            bar_load = bar_load) %>% 
    select.(date:bar_load, time:total_force) %>% 
    rename.(left = fp1,
            right = fp2)
  
  if (.df$jump_type == "sj") {
    # SJs are basic
    raw_data <-
      raw_data %>% 
      mutate.(phase = "Propulsion",
              time = time - first(time))
  } else {
    # We have three possible phases for the CMJ in this iteration
    raw_data <-
      raw_data %>% 
      mutate.(phase = case_when.(
        time <= .df$metric_table$unweight_end_time ~ "Unweighting",
        time <= .df$metric_table$braking_end_time ~ "Braking",
        TRUE ~ "Propulsion"))
  }
  
  if (!dir.exists("Analyses"))
    dir.create("Analyses")
  
  save_file <-
    file.path("Analyses",
              paste(date,
                    "Raw Force-Time Data.csv"))
  
  if (!file.exists(save_file)) {
    fwrite(raw_data, save_file)
  } else {
    fwrite(raw_data, save_file, append = TRUE)
  }
}

parse_curve_file <- function(file_location) {
  data <-
    fread(file = file_location,
          header = TRUE)
  
  # group_split. is an especially handy function in separating repetitive
  # data based on grouping conditions. As a side-effect, it also creates a list,
  # which makes operating on each list element a cake walk.
  curve_list <-
    data %>% 
    group_split.(date, name, jump_type, trial_number, bar_load,
                 .named = TRUE)
  
  return(curve_list)
}

# I AM NOT COMPLETELY SURE ON THE PIECEWISE IMPLEMENTATION BELOW
# Also, this function is unbelievably ugly...at any rate, interpolating
# just from phase to phase within either of the piecewise approaches
# (e.g. only unweighting, only braking, only propulsion) produced some weird
# artifacts at the phase change points. Maybe it's supposed to look a little
# weird, I dunno. Regardless, to keep the interpolated curve nearer in
# appearance to its raw counterpart, I begin the interpolation process
# for braking and propulsion with the final point from the previous phase and
# increase the length.out by 1. I immediately remove the first interpolated
# value, however, to avoid duplicating the value in the returned vector. This
# also has the side effect of making the resultant vector the correct length.
# 
# I could definitely use some help in deciding if this is the correct
# approach. If not, it's pretty easy to change the code back to its original
# form. 
interpolate_data <- function(curve_list, trial, jump_type,
                             method = c("linear", "piecewise_linear"),
                             combine_phases = FALSE,
                             phase_lengths = list(Curve = NULL,
                                                  Unweighting = NULL,
                                                  Braking = NULL,
                                                  Stretching = NULL,
                                                  Propulsion = NULL)) {
  trial_data <-
    curve_list[[trial]]
  
  if (jump_type != trial_data[, first(jump_type)])
    return(showNotification("Selected jump type doesn't match data", 
                            duration = 5,
                            type = "error"))
  
  test_date <- trial_data[, first(date)]
  name <- trial_data[, first(name)]
  trial_number <- trial_data[, first(trial_number)]
  bar_load <- trial_data[, first(bar_load)]
  
  # Linear interpolation is very straightforward. WYSIWYG in terms of your
  # desired length.
  if (jump_type == "sj" | method == "linear") {
    # I had a user email me about phase labels being dropped in linear
    # interpolation, which was a huge oversight on my part. I didn't want to add
    # an extra if-else here to delineate between SJ and CMJ interpolation, so
    # they use the same method of adding labels back to the data.
    # Importantly, I have the CMJ relabeller set to use the nearest label
    # in the raw data based on index location. That is, if unweighting is from
    # indices 1-175 and the interpolation returns an index of 175.4 that is
    # rounded down to 175, the label will be "Unweighting." If it's 175.51,
    # however, the label will be returned as "Braking."
    interpolated_data <-
      trial_data %>% 
      summarize.(across.(left:total_force,
                         ~ approx(time, .x,
                                  n = phase_lengths[["Curve"]])$y),
                 index = round(approx(time, n = phase_lengths[["Curve"]])$x)) %>% 
      mutate.(phase = trial_data[index, phase]) %>% 
      select.(-index)
  } else {
    # Here begin the piecewise shenanigans. First up is if the user wants to
    # combine the unweighting and braking phases into a "Stretching" phase
    if (combine_phases) {
      interpolated_data <-
        bind_rows.(
          trial_data %>% 
            filter.(phase %in% c("Unweighting", "Braking")) %>% 
            summarize.(across.(left:total_force,
                               ~ approx(time, .x,
                                        n = phase_lengths[["Stretching"]])$y)) %>% 
            mutate.(phase = "Stretching"),
          trial_data %>% 
            # Here's what I mentioned above about moving back to the final
            # data point from the previous phase...
            filter.(row_number.() >= which.max(phase == "Propulsion") - 1) %>% 
            summarize.(across.(left:total_force,
                               ~ approx(time, .x,
                                        # And increasing the interpolation length
                                        # by 1, only to immediately remove the
                                        # first point from the resultant vector
                                        # via the [-1]
                                        n = as.numeric(phase_lengths[["Propulsion"]]) + 1)$y[-1])) %>% 
            mutate.(phase = "Propulsion")
        )
    } else {
      # This used to be such a pretty three-line piece of code before I
      # decided to use the add-one-remove-one approach. Unweighting has to be
      # treated differently from Braking and Propulsion, hence the one final
      # if-else pair.
      interpolated_data <-
        map_df.(c("Unweighting", "Braking", "Propulsion"), function(x) {
          if (x == "Unweighting") {
            trial_data %>% 
              filter.(phase == x) %>% 
              summarize.(across.(left:total_force,
                                 ~ approx(time, .x,
                                          n = phase_lengths[[x]])$y)) %>% 
              mutate.(phase = x)
          } else {
            trial_data %>% 
              # This follows the same add-one-remove-one approach as before.
              filter.(row_number.() %between% c(which.max(phase == x) - 1,
                                                last(which(phase == x)))) %>% 
              summarize.(across.(left:total_force,
                                 ~ approx(time, .x,
                                          n = as.numeric(phase_lengths[[x]]) + 1)$y[-1])) %>% 
              mutate.(phase = x)
          }
        })
    }
  }
  
  interpolated_data <-
    interpolated_data %>% 
    mutate.(date = test_date,
            name = name,
            jump_type = jump_type,
            method = ifelse.(jump_type == "sj", "linear", method),
            trial_number = trial_number,
            bar_load = bar_load,
            index = seq_along(total_force),
            .before = left)
  
  return(interpolated_data)
}

build_raw_interp_plot <- function(.df, plot_title) {
  .df %>% 
    plot_ly(x = ~ seq_along(total_force)) %>% 
    add_lines(y = ~ left, name = "Left") %>% 
    add_lines(y = ~ right, name = "Right") %>% 
    add_lines(y = ~ total_force, name = "Total Force") %>% 
    layout(
      title = plot_title,
      xaxis = list(title = "Index"),
      yaxis = list(title = "Force (N)")
    ) %>% 
    config(displayModeBar = FALSE)
}

save_interpolated_curve <- function(interpolated_data, analysis_date) {
  if (!dir.exists("Analyses"))
    dir.create("Analyses")
  
  save_file <-
    file.path("Analyses",
              paste(analysis_date,
                    "Interpolated Force-Time Data.csv"))
  
  if (!file.exists(save_file)) {
    fwrite(interpolated_data, save_file)
  } else {
    fwrite(interpolated_data, save_file, append = TRUE)
  }
}

# Saves general summary statistics about a given trial.
save_summary_data <- function(.df, date, name,
                              trial_number, bar_load) {
  
  summary_data <-
    .df$metric_table %>% 
    select.(-any_of(c("unweight_end_time", "braking_end_time"))) %>% 
    mutate.(date = !!date,
            name = !!name,
            jump_type = .df$plot_annotations$jump_type,
            trial_number = !!trial_number,
            bar_load = !!bar_load,
            .before = body_mass)
  
  if (!dir.exists("Analyses"))
    dir.create("Analyses")
  
  save_file <-
    file.path("Analyses",
              paste(date,
                    "Vertical Jump Analysis.csv"))
  
  if (!file.exists(save_file)) {
    fwrite(transpose(list(save_headers)), save_file)
  }
  
  fwrite(summary_data, save_file, append = TRUE)
}