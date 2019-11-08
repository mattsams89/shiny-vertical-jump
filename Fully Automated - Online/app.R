source("global.R")

header <- dashboardHeader(title = "Vertical Jump Analysis",
                          titleWidth = 250)

sidebar <- dashboardSidebar(
  disable = TRUE
)

body <- dashboardBody(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML("
            .skin-blue .main-header .logo {
                background-color:#004687;
            }
            .skin-blue .main-header .logo:hover {
                background-color:#004687;
            }
            .skin-blue .main-header .navbar {
                background-color:#004687;
            }
            .skin-blue .main-header .navbar .sidebar-toggle:hover {
                background-color:#BD9B60;
            }
            .sidebar-toggle {
                display: none;
            }
            .shiny-split-layout > div {
                overflow: visible;
            }
          ")
    )
  ),
  
  box(
    width = 2,
    solidHeader = TRUE,
    selectInput(inputId = "upload_type",
                label = "How are the Data Arranged?",
                choices = c("Pasco",
                            "Multiple Trials - Wide",
                            "Multiple Trials - Long",
                            "Single Trial"),
                multiple = FALSE),
    fileInput(inputId = "file_upload",
              label = "Upload Data",
              multiple = FALSE,
              accept = c("*.txt", 
                         "*.csv")),
    numericInput(inputId = "selected_trial",
                 label = "Select Trial",
                 value = 1,
                 step = 1),
    dateInput(inputId = "test_date",
              label = "Selected Testing Date"),
    textInput(inputId = "athlete_name",
              label = "Enter Athlete Name"),
    splitLayout(
      selectInput(inputId = "filter_type",
                  label = "Apply Filter?",
                  choices = c("None" = "no_filter",
                              "Butterworth" = "butt_filter",
                              "Moving Avg" = "moving_avg")),
      selectInput(inputId = "jump_type",
                  label = "Select Jump Type",
                  choices = c("Automatic" = "auto",
                              "Squat Jump" = "sj",
                              "Countermovement Jump" = "cmj"))
    ),
    numericInput(inputId = "bar_load",
                 label = "Enter Bar Load (If Any)",
                 value = 0),
    splitLayout(
      numericInput(inputId = "sampling_frequency",
                   label = "Sampling Frequency",
                   value = 1000),
      numericInput(inputId = "standing_length",
                   label = "Quiet Standing",
                   value = 1,
                   min = 0.2,
                   max = 2,
                   step = 0.1)
    ),
    splitLayout(
      numericInput(inputId = "fp1_slope",
                   label = "FP1 Slope",
                   value = 1),
      numericInput(inputId = "fp1_intercept",
                   label = "FP1 Intercept",
                   value = 0)
    ),
    splitLayout(
      numericInput(inputId = "fp2_slope",
                   label = "FP2 Slope",
                   value = 1),
      numericInput(inputId = "fp2_intercept",
                   label = "FP2 Intercept",
                   value = 0)
    ),
    splitLayout(
      actionButton(inputId = "save_trial",
                   label = "Save Trial"),
      downloadButton(outputId = "download_data")
    )
  ),
  
  box(
    width = 7,
    solidHeader = TRUE,
    plotlyOutput(outputId = "main_plot"),
    plotlyOutput(outputId = "secondary_plot")
  ),
  
  tabBox(
    width = 3,
    tabPanel(title = "Quick View",
             plotlyOutput(outputId = "metric_plot",
                          height = "750px")),
    tabPanel(title = "Calculated Metrics",
             tableOutput(outputId = "metric_table"))
  )
)

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session){
  
  jump_information <- reactiveValues()
  
  # File upload observer; runs the file_parser function from global.R and sets the trial selector max value
  observeEvent(input$file_upload, {
    
    jump_information$data <- NULL
    
    jump_information$data <- file_parser(file_location = input$file_upload$datapath,
                                         upload_type = input$upload_type)
  })
  
  # Data manipulation / formatting prior to analysis
  observe({
    req(jump_information$data)
    
    jump_information$formatted_data <- data_manipulation(uploaded_data = jump_information$data,
                                                         upload_type = isolate(input$upload_type),
                                                         filter_type = input$filter_type,
                                                         sampling_frequency = input$sampling_frequency,
                                                         fp1_slope = input$fp1_slope,
                                                         fp1_intercept = input$fp1_intercept,
                                                         fp2_slope = input$fp2_slope,
                                                         fp2_intercept = input$fp2_intercept)
    
    if(isolate(input$upload_type) != "Single Trial")
      jump_information$trials <- length(jump_information$formatted_data)
    else
      jump_information$trials <- 1
    
    updateNumericInput(session,
                       inputId = "selected_trial",
                       value = 1,
                       max = jump_information$trials)
  })
  
  # Plotly allows plot brushing. This allows you to select a new range for x-axis values if you aren't happy with the automatic
  # selection for quiet standing. Start by initializing the brush as NULL, as the functions checking for brushing
  # specifically check for a non-NULL brush.
  jump_information$trial_brush <- NULL
  
  # We also need the capability to reset the brush alongside creating the brush, so...
  observe({
    req(jump_information$formatted_data)
    
    if(is.null(event_data(source = "plot_brush",
                          event = "plotly_brushed")$x))
      jump_information$trial_brush <- NULL
    else
      jump_information$trial_brush <- round(event_data(source = "plot_brush",
                                                       event = "plotly_brushed")$x)
  })
  
  # When selecting a new trial, we need to make sure the brush is destroyed. Otherwise, this can crash the
  # app if the brush values are outside the bounds of the new trial.
  observeEvent(input$selected_trial, {
    if(!is.null(jump_information$trial_brush))
      jump_information$trial_brush <- NULL
  })
  
  # Creates the primary plot. Brushing is allowed.
  output$main_plot <- renderPlotly({
    req(jump_information$formatted_data,
        input$selected_trial != 0,
        !is.na(input$selected_trial),
        !(input$selected_trial > jump_information$trials))
    
    if(isolate(input$upload_type) != "Single Trial"){
      trial <- input$selected_trial
      data <- jump_information$formatted_data[[trial]]$trial_data
    }
    else
      data <- jump_information$formatted_data$trial_data
    
    plot_ly(data,
            x = ~seq_along(total_force),
            source = "plot_brush") %>%
      add_lines(y = ~total_force,
                name = "Total Force") %>%
      layout(xaxis = list(title = "Index",
                          fixedrange = TRUE),
             yaxis = list(title = "Force",
                          fixedrange = TRUE),
             dragmode = "select")
  })
  
  # Remainder of event detection. Adjusts itself if there's any plot brushing.
  observe({
    req(jump_information$formatted_data,
        input$selected_trial != 0,
        !is.na(input$selected_trial),
        !(input$selected_trial > jump_information$trials))
    
    trial <- input$selected_trial
    
    if(isolate(input$upload_type) != "Single Trial"){
      trial <- input$selected_trial
      data <- jump_information$formatted_data[[trial]]$trial_data
      minimum_force_index <- jump_information$formatted_data[[trial]]$minimum_force_index
      peak_force_index <- jump_information$formatted_data[[trial]]$peak_force_index
      flight_threshold <- jump_information$formatted_data[[trial]]$flight_threshold
      takeoff_index <- jump_information$formatted_data[[trial]]$takeoff_index
      landing_index <- jump_information$formatted_data[[trial]]$landing_index
      peak_landing_force_index <- jump_information$formatted_data[[trial]]$peak_landing_force_index
    }
    else{
      data <- jump_information$formatted_data$trial_data
      minimum_force_index <- jump_information$formatted_data$minimum_force_index
      peak_force_index <- jump_information$formatted_data$peak_force_index
      flight_threshold <- jump_information$formatted_data$flight_threshold
      takeoff_index <- jump_information$formatted_data$takeoff_index
      landing_index <- jump_information$formatted_data$landing_index
      peak_landing_force_index <- jump_information$formatted_data$peak_landing_force_index
    }
    
    sampling_frequency <- input$sampling_frequency
    quiet_standing_length <- round(input$standing_length * sampling_frequency)
    
    req(quiet_standing_length != 0,
        !is.na(quiet_standing_length))
    
    if(!is.null(jump_information$trial_brush)){
      
      brush_offset <- jump_information$trial_brush[1] - 1
      
      data <- data[1:nrow(data) %between% jump_information$trial_brush]
      minimum_force_index <- minimum_force_index - brush_offset
      peak_force_index <- peak_force_index - brush_offset
      takeoff_index <- takeoff_index - brush_offset
      landing_index <- landing_index - brush_offset
      peak_landing_force_index <- peak_landing_force_index - brush_offset
    }
    
    body_weight <- data[1:quiet_standing_length, mean(total_force)]
    body_weight_sd <- data[1:quiet_standing_length, sd(total_force)]
    body_mass <- body_weight / 9.81
    fp1_weight <- data[1:quiet_standing_length, mean(fp1)]
    fp2_weight <- data[1:quiet_standing_length, mean(fp2)]
    
    minimum_force <- data[minimum_force_index, total_force]
    
    if(input$jump_type == "auto")
      if(body_weight - minimum_force > 250)
        jump_type <- "cmj"
    else
      jump_type <- "sj"
    else
      jump_type <- input$jump_type
    
    if(jump_type == "sj"){
      initiation_threshold <- body_weight + body_weight_sd * 5
      
      jump_start_index <- detect_index(data[1:peak_force_index, total_force],
                                       ~ .x <= initiation_threshold,
                                       .dir = "backward") - round(29 * (sampling_frequency / 1000))
    }
    else{
      initiation_threshold <- body_weight - body_weight_sd * 5
      
      jump_start_index <- detect_index(data[1:minimum_force_index, total_force],
                                       ~ .x >= initiation_threshold,
                                       .dir = "backward") - round(29 * (sampling_frequency / 1000))
    }
    
    jump_information$analysis_data <- list(analysis_force_data = data,
                                           quiet_standing_length = quiet_standing_length,
                                           body_weight = body_weight,
                                           body_weight_sd = body_weight_sd,
                                           body_mass = body_mass,
                                           fp1_weight = fp1_weight,
                                           fp2_weight = fp2_weight,
                                           jump_type = jump_type,
                                           sampling_frequency = sampling_frequency,
                                           initiation_threshold = initiation_threshold,
                                           jump_start_index = jump_start_index,
                                           minimum_force_index = minimum_force_index,
                                           peak_force_index = peak_force_index,
                                           flight_threshold = flight_threshold,
                                           takeoff_index = takeoff_index,
                                           landing_index = landing_index,
                                           peak_landing_force_index = peak_landing_force_index)
  })
  
  output$secondary_plot <- renderPlotly({
    req(jump_information$analysis_data)
    
    data <- jump_information$analysis_data$analysis_force_data
    jump_type <- jump_information$analysis_data$jump_type
    body_weight <- jump_information$analysis_data$body_weight
    body_weight_sd <- jump_information$analysis_data$body_weight_sd
    body_mass <- jump_information$analysis_data$body_mass
    initiation_threshold <- jump_information$analysis_data$initiation_threshold
    jump_start <- jump_information$analysis_data$jump_start_index
    peak_force <- jump_information$analysis_data$peak_force_index
    flight_threshold <- jump_information$analysis_data$flight_threshold
    takeoff <- jump_information$analysis_data$takeoff_index
    landing <- jump_information$analysis_data$landing_index
    peak_landing_force <- jump_information$analysis_data$peak_landing_force_index
    
    plot_ly(data,
            x = ~seq_along(total_force)) %>%
      add_lines(y = ~fp1,
                name = "FP1") %>%
      add_lines(y = ~fp2,
                name = "FP2") %>%
      add_lines(y = ~total_force,
                name = "Total Force") %>%
      add_annotations(x = 0,
                      y = 1,
                      text = paste0(
                        "Body Mass (kg): ", round(body_mass, 2),
                        "<br>Body Weight (N): ", round(body_weight, 2),
                        "<br>Body Weight SD (N): ", round(body_weight_sd, 2),
                        "<br>Jump Type: ", toupper(jump_type),
                        "<br>Initiation Threshold (N): ", round(initiation_threshold, 2),
                        "<br>Flight Threshold (N): ", round(flight_threshold, 2)
                        ),
                      xref = "paper",
                      yref = "paper",
                      showarrow = FALSE) %>%
      layout(shapes = list(vline(jump_start),
                           vline(peak_force),
                           vline(takeoff),
                           vline(landing),
                           vline(peak_landing_force),
                           list(type = "rect",
                                fillcolor = "blue",
                                line = list(color = "blue"),
                                opacity = 0.3,
                                x0 = 1,
                                x1 = jump_start,
                                xref = "x",
                                y0 = body_weight - body_weight_sd * 5,
                                y1 = body_weight + body_weight_sd * 5),
                           list(type = "line",
                                line = list(color = "red"),
                                x0 = 1,
                                x1 = jump_start,
                                xref = "x",
                                y0 = initiation_threshold,
                                y1 = initiation_threshold,
                                yref = "y"),
                           list(type = "line",
                                line = list(color = "red"),
                                x0 = takeoff,
                                x1 = landing,
                                xref = "x",
                                y0 = flight_threshold,
                                y1 = flight_threshold,
                                yref = "y")),
             xaxis = list(title = "Index"),
             yaxis = list(title = "Force")
      )
  })
  
  observe({
    req(jump_information$analysis_data)
    
    data <- jump_information$analysis_data$analysis_force_data
    jump_type <- jump_information$analysis_data$jump_type
    body_weight <- jump_information$analysis_data$body_weight
    body_mass <- jump_information$analysis_data$body_mass
    fp1_weight <- jump_information$analysis_data$fp1_weight
    fp2_weight <- jump_information$analysis_data$fp2_weight
    jump_start_index <- jump_information$analysis_data$jump_start_index
    takeoff_index <- jump_information$analysis_data$takeoff_index
    landing_index <- jump_information$analysis_data$landing_index
    peak_landing_force_index <- jump_information$analysis_data$peak_landing_force_index
    sampling_frequency <- jump_information$analysis_data$sampling_frequency
    
    jump_data <- data[jump_start_index:takeoff_index]
    
    jump_data[, ":=" (net_impulse = cumtrapz(1:nrow(jump_data), total_force - body_weight) / sampling_frequency,
                      fp1_net_impulse = cumtrapz(1:nrow(jump_data), fp1 - fp1_weight) / sampling_frequency,
                      fp2_net_impulse = cumtrapz(1:nrow(jump_data), fp2 - fp2_weight) / sampling_frequency)
              ][, velocity := net_impulse / body_mass
                ][, power := total_force * velocity]
    
    peak_power_index <- jump_data[, which.max(power)]
    
    flight_time <- (landing_index - takeoff_index) / sampling_frequency
    net_impulse <- jump_data[, last(net_impulse)]
    jump_height_ft <- 0.5 * 9.81 * (flight_time / 2) ^ 2
    takeoff_velocity <- jump_data[, last(velocity)]
    jump_height_ni <- takeoff_velocity ^ 2 / (2 * 9.81)
    peak_force <- jump_data[, max(total_force)]
    peak_velocity <- jump_data[, max(velocity)]
    peak_power <- jump_data[, max(power)]
    force_peak_power <- jump_data[peak_power_index, total_force]
    velocity_peak_power <- jump_data[peak_power_index, velocity]
    time_to_peak_force <- jump_data[, which.max(total_force)] / sampling_frequency
    avg_rfd <- (peak_force - jump_data[, first(total_force)]) / time_to_peak_force
    contact_time <- nrow(jump_data) / sampling_frequency
    rsi_modified <- jump_height_ni / contact_time
    
    metric_table <- data.table(date = input$test_date,
                               athlete = input$athlete_name,
                               jump_type,
                               trial = input$selected_trial,
                               bar_load = input$bar_load,
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
                               rsi_modified)
    
    fp1_net_impulse <- jump_data[, last(fp1_net_impulse)]
    fp2_net_impulse <- jump_data[, last(fp2_net_impulse)]
    net_impulse_symmetry_index <- (fp1_net_impulse - fp2_net_impulse) / mean(c(fp1_net_impulse, fp2_net_impulse)) * 100
    fp1_peak_force <- jump_data[, max(fp1)]
    fp2_peak_force <- jump_data[, max(fp2)]
    peak_force_symmetry_index <- (fp1_peak_force - fp2_peak_force) / mean(c(fp1_peak_force, fp2_peak_force)) * 100
    fp1_peak_force_index <- jump_data[, which.max(fp1)]
    fp2_peak_force_index <- jump_data[, which.max(fp2)]
    fp1_time_to_peak_force <- fp1_peak_force_index / sampling_frequency
    fp2_time_to_peak_force <- fp2_peak_force_index / sampling_frequency
    ttpf_symmetry_index <- (fp1_time_to_peak_force - fp2_time_to_peak_force) / mean(c(fp1_time_to_peak_force, fp2_time_to_peak_force)) * 100
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
    
    if(jump_type == "cmj"){
      
      peak_force_index <- jump_data[, which.max(total_force)]
      
      minimum_force_index <- jump_data[1:peak_force_index, which.min(total_force)]
      
      unweight_end_index <- detect_index(jump_data[minimum_force_index:peak_force_index, total_force],
                                         ~ .x >= jump_data[, first(total_force)]) + minimum_force_index - 2
      
      braking_end_index <- detect_index(jump_data[(unweight_end_index + 1):nrow(jump_data), net_impulse],
                                        ~ .x >= 0) + unweight_end_index - 1
      
      unweight_duration <- unweight_end_index / sampling_frequency
      braking_duration <- (braking_end_index - unweight_end_index) / sampling_frequency
      concentric_duration <- (nrow(jump_data) - braking_end_index) / sampling_frequency
      
      zero_velo_index <- detect_index(jump_data[, velocity],
                                      ~ .x <= 0,
                                      .dir = "backward") + 1
      
      force_zero_velo <- jump_data[zero_velo_index,
                                   total_force]
      
      metric_table <- cbind(metric_table,
                            unweight_duration,
                            braking_duration,
                            concentric_duration,
                            force_zero_velo)
    }
    
    jump_information$metric_data <- list(plot_data = jump_data,
                                         metric_table = metric_table,
                                         sampling_frequency = sampling_frequency)
  })
  
  output$metric_plot <- renderPlotly({
    req(jump_information$metric_data)
    
    jump_type <- jump_information$metric_data$metric_table$jump_type
    plot_data <- jump_information$metric_data$plot_data
    sampling_frequency <- jump_information$metric_data$sampling_frequency
    unweight_end_index <- round(jump_information$metric_data$metric_table$unweight_duration * sampling_frequency)
    braking_end_index <- round(jump_information$metric_data$metric_table$braking_duration * sampling_frequency) + unweight_end_index
    
    if(jump_type == "sj"){
      force_plot <- plot_ly(plot_data,
                            x = ~1:nrow(plot_data)) %>%
        add_lines(y = ~total_force,
                  name = "Force",
                  showlegend = FALSE) %>%
        add_annotations(text = "<b>Force</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
      
      velocity_plot <- plot_ly(plot_data,
                               x = ~1:nrow(plot_data)) %>%
        add_lines(y = ~velocity,
                  name = "Velocity",
                  showlegend = FALSE) %>%
        add_annotations(text = "<b>Velocity</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
      
      power_plot <- plot_ly(plot_data,
                            x = ~1:nrow(plot_data)) %>%
        add_lines(y = ~power,
                  name = "Power",
                  showlegend = FALSE) %>%
        add_annotations(text = "<b>Power</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
    }
    else{
      force_plot <- plot_ly(type = "scatter",
                            mode = "lines",
                            name = "Force") %>%
        add_trace(data = plot_data,
                  x = ~1:unweight_end_index,
                  y = ~first(total_force),
                  color = I("black"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[1:unweight_end_index],
                  x = ~seq_along(total_force),
                  y = ~total_force,
                  fill = "tonexty",
                  color = I("red"),
                  name = "Unweighting") %>%
        add_trace(data = plot_data,
                  x = ~unweight_end_index:braking_end_index,
                  y = ~first(total_force),
                  color = I("black"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(unweight_end_index + 1):braking_end_index],
                  x = ~((unweight_end_index + 1):braking_end_index),
                  y = ~total_force,
                  fill = "tonexty",
                  color = I("orange"),
                  name = "Braking") %>%
        add_trace(data = plot_data,
                  x = ~braking_end_index:nrow(plot_data),
                  y = ~first(total_force),
                  color = I("black"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(braking_end_index + 1):nrow(plot_data)],
                  x = ~((braking_end_index + 1):nrow(plot_data)),
                  y = ~total_force,
                  fill = "tonexty",
                  color = I("blue"),
                  name = "Propulsive") %>%
        layout(xaxis = list(title = "Index"),
               yaxis = list(title = "Force"),
               legend = list(orientation = "h")) %>%
        add_annotations(text = "<b>Force</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
      
      velocity_plot <- plot_ly(type = "scatter",
                               mode = "lines",
                               name = "Velocity") %>%
        add_trace(data = plot_data[1:unweight_end_index],
                  x = ~seq_along(velocity),
                  y = ~velocity,
                  fill = "tozeroy",
                  color = I("red"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(unweight_end_index + 1):braking_end_index],
                  x = ~((unweight_end_index + 1):braking_end_index),
                  y = ~velocity,
                  fill = "tozeroy",
                  color = I("orange"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(braking_end_index + 1):nrow(plot_data)],
                  x = ~((braking_end_index + 1):nrow(plot_data)),
                  y = ~velocity,
                  fill = "tozeroy",
                  color = I("blue"),
                  showlegend = FALSE) %>%
        layout(xaxis = list(title = "Index"),
               yaxis = list(title = "Velocity"),
               shapes = list(type = "line",
                             line = list(color = "black"),
                             x0 = 0,
                             x1 = 1,
                             xref = "paper",
                             y0 = 0,
                             y1 = 0,
                             yref = "y")) %>%
        add_annotations(text = "<b>Velocity</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
      
      power_plot <- plot_ly(type = "scatter",
                            mode = "lines",
                            name = "Power") %>%
        add_trace(data = plot_data[1:unweight_end_index],
                  x = ~seq_along(power),
                  y = ~power,
                  fill = "tozeroy",
                  color = I("red"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(unweight_end_index + 1):braking_end_index],
                  x = ~((unweight_end_index + 1):braking_end_index),
                  y = ~power,
                  fill = "tozeroy",
                  color = I("orange"),
                  showlegend = FALSE) %>%
        add_trace(data = plot_data[(braking_end_index + 1):nrow(plot_data)],
                  x = ~((braking_end_index + 1):nrow(plot_data)),
                  y = ~power,
                  fill = "tozeroy",
                  color = I("blue"),
                  showlegend = FALSE) %>%
        layout(xaxis = list(title = "Index"),
               yaxis = list(title = "Power"),
               shapes = list(type = "line",
                             line = list(color = "black"),
                             x0 = 0,
                             x1 = 1,
                             xref = "paper",
                             y0 = 0,
                             y1 = 0,
                             yref = "y")) %>%
        add_annotations(text = "<b>Power</b>",
                        x = 0,
                        y = 1,
                        xref = "paper",
                        yref = "paper",
                        xanchor = "middle",
                        yanchor = "top",
                        showarrow = FALSE)
    }
    
    subplot(force_plot,
            velocity_plot,
            power_plot,
            nrows = 3,
            shareX = TRUE) %>%
      layout(xaxis = list(title = "Index"))
  })
  
  output$metric_table <- function(){
    req(jump_information$metric_data)
    
    table_data <- jump_information$metric_data$metric_table
    jump_type <- table_data[, jump_type]
    
    if(jump_type == "cmj")
      new_names <- cmj_table_headers
    else
      new_names <- sj_table_headers
    
    table_data <- table_data[, ":=" (date = NULL,
                                     athlete = NULL,
                                     jump_type = NULL,
                                     trial = NULL,
                                     bar_load = NULL)]
    setnames(table_data, new_names)
    
    long_table <- melt(table_data,
                       measure.vars = 1:ncol(table_data),
                       variable.name = "Variable",
                       value.name = "Value")
    
    output_table <- kable(long_table,
                          digits = 3,
                          col.names = NULL) %>%
      kable_styling(bootstrap_options = "striped") %>%
      pack_rows("Bilateral Variables", 
                1, 
                15, 
                label_row_css = "background-color: #004687; color: #fff;") %>%
      pack_rows("Unilateral Variables", 
                16, 
                27, 
                label_row_css = "background-color: #004687; color: #fff;")
    
    if(jump_type == "cmj"){
      output_table <- output_table %>%
        pack_rows("Phase Variables", 
                  28, 
                  31, 
                  label_row_css = "background-color: #004687; color: #fff;")
    }
    
    output_table %>%
      scroll_box(width = "100%",
                 height = "750px")
  }
  
  observeEvent(input$save_trial, {
    req(jump_information$metric_data)
    
    save_function(data_to_write = jump_information$metric_data$metric_table)
    
    showNotification(ui = "Trial Saved Successfully!", 
                     duration = 2, 
                     type = "message")
  })
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste(Sys.Date(),
            "Vertical Jump Analysis.csv")
    },
    content = function(file){
      file.copy(
        from = file.path("Analyses",
                         paste(Sys.Date(),
                               "Vertical Jump Analysis.csv")),
        to = file
      )
    },
    contentType = "text/csv"
  )
}

shinyApp(ui, server)