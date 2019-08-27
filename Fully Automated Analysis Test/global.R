library(shiny)
library(shinydashboard)
library(data.table)
library(zoo)
library(signal)
library(changepoint)
library(purrr)
library(pracma)
library(plotly)
library(knitr)
library(kableExtra)
library(shinyjs)

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

file_reader <- function(file_location) {
  
  data <- fread(file = file_location,
                skip = 1,
                header = TRUE,
                na.strings = c("", "NA"),
                stringsAsFactors = FALSE)
  
  data <- data[, grepl("Normal", 
                       names(data)), 
               with = FALSE]
  
  return(data)
}

data_manipulation <- function(uploaded_data, filter_type, sampling_frequency, fp1_slope, fp1_intercept, fp2_slope, fp2_intercept) {
  
  data <- uploaded_data
  
  trials <- ncol(data) / 2
  
  trial_list <- lapply(1:trials, function(x){
    fp1 <- data[, (x * 2 - 1), 
                     with = FALSE] * fp1_slope + fp1_intercept
    
    fp2 <- data[, (x * 2), 
                     with = FALSE] * fp2_slope + fp2_intercept
    
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
    
    if(trial_data[1:250, mean(total_force)] < 440){
      start_index <- detect_index(trial_data[, total_force], ~ .x >= 440)
      
      trial_data <- trial_data[start_index:nrow(trial_data)]
    }
    
    if(trial_data[trial_data[, tail(.I, 250)], mean(total_force)] < 440){
      end_index <- detect_index(trial_data[, total_force], ~ .x >= 440, .dir = "backward")
      
      trial_data <- trial_data[1:end_index]
    }
    
    suppressWarnings({
      mean_changepoints <- cpt.mean(trial_data[, total_force], 
                                    method = "BinSeg", 
                                    minseglen = round(200 * (sampling_frequency / 1000)), 
                                    Q = 5, 
                                    class = FALSE)
    })
    
    mean_changepoints <- data.table(cbind(cp1 = c(1, mean_changepoints[1:length(mean_changepoints) - 1]), 
                                          cp2 = mean_changepoints))
    
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
    
    peak_landing_force_index <- which.max(trial_data[offset_range[2]:nrow(trial_data), total_force]) + offset_range[2]
    
    suppressWarnings({
      var_changepoints <- cpt.var(trial_data[1:peak_force_index, total_force], 
                                  method = "BinSeg", 
                                  minseglen = round(500 * (sampling_frequency / 1000)), 
                                  Q = 5, 
                                  class = FALSE)
    })
    
    var_changepoints <- data.table(cbind(cp1 = c(1, var_changepoints[1:length(var_changepoints) - 1]), 
                                         cp2 = var_changepoints))
    
    most_stable_var_changepoint <- trial_data[, list(lapply(1:6, function(x) trial_data[1:nrow(trial_data) %between% var_changepoints[x, .(cp1, cp2)],
                                                                                        sd(total_force)]))
                                              ][, which.min(V1)]
    
    trial_data <- trial_data[var_changepoints[most_stable_var_changepoint, cp1]:nrow(trial_data)]
    
    peak_force_index <- peak_force_index - var_changepoints[most_stable_var_changepoint, cp1]
    offset_range <- offset_range - var_changepoints[most_stable_var_changepoint, cp1]
    peak_landing_force_index <- peak_landing_force_index - var_changepoints[most_stable_var_changepoint, cp1]
    
    trial_information <- list(trial_data = trial_data, 
                              peak_force_index = peak_force_index, 
                              offset_range = offset_range, 
                              peak_landing_force_index = peak_landing_force_index)
  })
  
  return(trial_list)
}

sj_table_headers <- c("Body Mass", "Flight Time", "Net Impulse", "Jump Height (FT)",
                      "Jump Height (NI)", "Takeoff Velocity", "Peak Force", "Peak Velocity",
                      "Peak Power", "Force @ Peak Power", "Velocity @ Peak Power",
                      "Time to Peak Force", "Avg RFD", "Contact Time", "RSI Modified",
                      "FP1 Net Impulse", "FP2 Net Impulse", "Net Impulse SI",
                      "FP1 Peak Force", "FP2 Peak Force", "Peak Force SI",
                      "FP1 Time to Peak Force", "FP2 Time to Peak Force",
                      "Time to Peak Force SI", "FP1 Avg RFD", "FP2 Avg RFD", "Avg RFD SI")

cmj_table_headers <- c(sj_table_headers, "Force @ Zero Velocity", "Unweighting Duration", "Braking Duration",
                       "Concentric Duration")