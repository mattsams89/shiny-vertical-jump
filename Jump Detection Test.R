library(data.table)
library(zoo)
library(signal)
library(changepoint)
library(purrr)

filter_type <- "no_filter"
sampling_frequency <- 1000

# Update to cut down on calculation time; calculate all column sums ahead of time instead of waiting until trial selection
test_data <- fread(file.choose(),
                   skip = 1,
                   header = TRUE,
                   na.strings = c("", "NA"),
                   stringsAsFactors = FALSE)
test_data <- test_data[, grepl("Normal", names(test_data)), 
                       with = FALSE]

trials <- ncol(test_data) / 2

total_data <- lapply(1:trials, function(x){
  fp1 <- test_data[, (x * 2 - 1), with = FALSE] * 1 + 0 # Replace
  fp2 <- test_data[, (x * 2), with = FALSE] * 1 + 0 # Replace
  
  trial_data <- data.table(fp1, fp2)[, setnames(.SD, c("fp1", "fp2"))
                                     ][, na.omit(.SD)]
  
  if(filter_type != "no_filter"){
    if(filter_type == "butt_filter"){
      filter <- butter(2, 10 / (0.5 * sampling_frequency), "low")
      
      trial_data <- trial_data[, lapply(.SD, function(x) filtfilt(filter, x))
                               ][, total_force := fp1 + fp2]
    }
    else{
      trial_data <- trial_data[, lapply(.SD, function(x) rollapplyr(x, 10, mean, partial = TRUE))
                               ][, total_force := fp1 + fp2]
    }
  }
  else{
    trial_data[, total_force := fp1 + fp2]
  }
  
  mean_changepoints <- cpt.mean(trial_data[, total_force], method = "BinSeg", minseglen = round(200 * (sampling_frequency / 1000)), Q = 5, class = FALSE)
  mean_changepoints <- data.table(cbind(cp1 = c(0, mean_changepoints[-6]), cp2 = mean_changepoints))
  
  landing <- trial_data[, which.max(c(0, diff(total_force)))]
  closest <- which.min(abs(mean_changepoints$cp2 - landing))
  
  offset_range <- mean_changepoints[closest, c(cp1, cp2)]
  offset_length <- round(diff(offset_range) * 0.25)
  offset_range[1] <- offset_range[1] + offset_length
  offset_range[2] <- offset_range[2] - offset_length
  
  fp1_offset <- trial_data[1:nrow(trial_data) %between% c(offset_range[1], offset_range[2]), mean(fp1)]
  fp2_offset <- trial_data[1:nrow(trial_data) %between% c(offset_range[1], offset_range[2]), mean(fp2)]
  
  trial_data[, ":=" (fp1 = fp1 - fp1_offset,
                     fp2 = fp2 - fp2_offset)
             ][, total_force := fp1 + fp2]
  
  peak_force_index <- which.max(trial_data[1:offset_range[1], total_force])
  peak_landing_force_index <- which.max(trial_data[offset_range[2]:nrow(trial_data), total_force]) + offset_range[2] - 1
  
  if(trial_data[1:100, mean(total_force)] < 440){
    start_index <- detect_index(trial_data[, total_force], ~ .x >= 440)
    trial_data <- trial_data[start_index:nrow(trial_data)]
    
    peak_force_index <- peak_force_index - start_index
    peak_landing_force_index <- peak_landing_force_index - start_index
  }
  
  var_changepoints <- cpt.var(trial_data[1:peak_force_index, total_force], method = "BinSeg", minseglen = round(500 * (sampling_frequency / 1000)), Q = 5, class = FALSE)
  var_changepoints <- data.table(cbind(cp1 = c(0, var_changepoints[-6]), cp2 = var_changepoints))
  
  most_stable_var_changepoint <- trial_data[, list(lapply(1:6, function(x) trial_data[1:nrow(trial_data) %between% var_changepoints[x, .(cp1, cp2)],
                                                                                      sd(total_force)]))
                                            ][, which.min(V1)]
  
  trial_data <- trial_data[var_changepoints[most_stable_var_changepoint, cp1]:nrow(trial_data)]
})

