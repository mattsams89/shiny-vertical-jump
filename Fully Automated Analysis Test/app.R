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
          ")
    )
  ),
  
  box(
    width = 2,
    solidHeader = TRUE,
    fileInput(inputId = "file_upload",
              label = "Upload data",
              multiple = FALSE,
              accept = c(".txt", ".csv")),
    numericInput(inputId = "selected_trial",
                 label = "Select trial",
                 value = 1,
                 step = 1),
    dateInput(inputId = "test_date",
              label = "Select testing date"),
    textInput(inputId = "athlete_name",
              label = "Enter athlete name"),
    selectInput(inputId = "jump_type",
                label = "Select jump type",
                choices = c("Automatic" = "auto",
                            "Squat Jump" = "sj",
                            "Countermovement Jump" = "cmj")),
    numericInput(inputId = "bar_load", 
                 label = "Enter bar load (if any)", 
                 value = 0),
    splitLayout(numericInput(inputId = "sampling_frequency",
                             label = "Samling frequency", 
                             value = 1000),
                numericInput(inputId = "standing_length",
                             label = "Quiet standing",
                             value = 0.5,
                             min = 0.2,
                             max = 1.0,
                             step = 0.1)),
    splitLayout(
      numericInput(inputId = "fp1_slope", 
                   label = "FP1 slope", 
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
    selectInput(inputId = "filter_type",
                label = "Apply filter?",
                choices = c("None" = "no_filter",
                            "Butterworth" = "butt_filter",
                            "Moving Avg" = "moving_avg")),
    actionButton(inputId = "save_trial",
                 label = "Save trial")
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

server <- function(input, output, session) {
  
  jump_information <- reactiveValues()
  
  # File upload observer; formats data and updates trial input to max(trials)
  observe({
    req(input$file_upload)
    
    jump_information$data <- NULL
    
    jump_information$data <- file_reader(file_location = input$file_upload$datapath)
    
    trials <- ncol(jump_information$data) / 2
    
    updateNumericInput(session,
                       inputId = "selected_trial", 
                       value = 1, 
                       max = trials)
  })
  
  # Data formatter
  observe({
    req(jump_information$data)
    
    jump_information$formatted_data <- data_manipulation(uploaded_data = jump_information$data, 
                                                         filter_type = input$filter_type, 
                                                         sampling_frequency = input$sampling_frequency, 
                                                         fp1_slope = input$fp1_slope, 
                                                         fp1_intercept = input$fp1_intercept, 
                                                         fp2_slope = input$fp2_slope, 
                                                         fp2_intercept = input$fp2_intercept)
  })
  
  # Brush for primary plot; used to modify trial selection if you're unhappy with the automatic selection
  jump_information$trial_brush <- NULL
  
  observe({
    req(jump_information$formatted_data)
    
    if(is.null(event_data(source = "plot_brush", event = "plotly_brushed")$x))
      jump_information$trial_brush <- NULL
    else
      jump_information$trial_brush <- round(event_data(source = "plot_brush", event = "plotly_brushed")$x)
  })
  
  # Resets the plot brush when a new trial is selected
  observeEvent(input$selected_trial, {
    if(!is.null(jump_information$trial_brush))
      jump_information$trial_brush <- NULL
  })
  
  # Primary plot; brushing is allowed
  # Requires uploaded data and a selected trial to work
  output$main_plot <- renderPlotly({
    req(jump_information$formatted_data, input$selected_trial)
    
    trial <- input$selected_trial
    data <- jump_information$formatted_data[[trial]]$trial_data
    
    plot_ly(data,
            x = ~1:nrow(data),
            source = "plot_brush") %>%
      add_lines(y = ~fp1,
                name = "FP1") %>%
      add_lines(y = ~fp2,
                name = "FP2") %>%
      add_lines(y = ~total_force,
                name = "Total Force") %>%
      layout(xaxis = list(title = "Index", 
                          fixedrange = TRUE),
             yaxis = list(title = "Force",
                          fixedrange = TRUE),
             dragmode = "select")
  })
  
  # Event detection; requires uploaded/formatted data and a selected trial
  observe({
    req(jump_information$formatted_data, input$selected_trial)
    
    trial <- input$selected_trial
    data <- jump_information$formatted_data[[trial]]$trial_data
    peak_force_index <- jump_information$formatted_data[[trial]]$peak_force_index
    offset_range <- jump_information$formatted_data[[trial]]$offset_range
    peak_landing_force_index <- jump_information$formatted_data[[trial]]$peak_landing_force_index
    sampling_frequency <- input$sampling_frequency
    quiet_standing_length <- input$standing_length * sampling_frequency
    
    threshold <- 10
    
    if(!is.null(jump_information$trial_brush)){
      data <- data[1:nrow(data) %between% jump_information$trial_brush]
      peak_force_index <- peak_force_index - jump_information$trial_brush[1] + 1
      offset_range <- offset_range - jump_information$trial_brush[1] + 1
      peak_landing_force_index <- peak_landing_force_index - jump_information$trial_brush[1] + 1
    }
    
    takeoff_index <- detect_index(data[peak_force_index:offset_range[1], total_force], ~ .x <= threshold) + peak_force_index - 2
    landing_index <- detect_index(data[offset_range[2]:peak_landing_force_index, total_force], ~ .x <= threshold, .dir = "backward") + offset_range[2]
    
    body_weight <- data[1:quiet_standing_length, mean(total_force)]
    body_weight_sd <- data[1:quiet_standing_length, sd(total_force)]
    body_mass <- body_weight / 9.81
    
    pre_peak_minimum_force <- data[quiet_standing_length:peak_force_index, min(total_force)]
    if(input$jump_type == "auto")
      if(body_weight - pre_peak_minimum_force > 250) # Arbitrarily chose 250 newtons as the delineation between SJ and CMJ; may need to be adjusted
        jump_type <- "cmj"
    else
      jump_type <- "sj"
    else
      jump_type <- input$jump_type
    
    if(jump_type == "sj"){
      initiation_threshold <- body_weight + body_weight_sd * 5
      jump_start_index <- detect_index(data[1:peak_force_index, total_force], 
                                       ~ .x <= initiation_threshold, .dir = "backward") + 1 - 30 * (sampling_frequency / 1000)
    }
    else{
      initiation_threshold <- body_weight - body_weight_sd * 5
      minimum_force_index <- which.min(data[1:peak_force_index, total_force])
      jump_start_index <- detect_index(data[1:minimum_force_index, total_force],
                                       ~ .x >= initiation_threshold, .dir = "backward") + 1 - 30 * (sampling_frequency / 1000)
    }
    
    analysis_force_data <- data
    
    jump_information$analysis_data <- list(analysis_force_data = analysis_force_data, 
                                           quiet_standing_length = quiet_standing_length,
                                           body_weight = body_weight, 
                                           body_weight_sd = body_weight_sd,
                                           body_mass = body_mass, 
                                           jump_type = jump_type, 
                                           sampling_frequency = sampling_frequency, 
                                           jump_start_index = jump_start_index, 
                                           peak_force_index = peak_force_index, 
                                           takeoff_index = takeoff_index, 
                                           landing_index = landing_index, 
                                           peak_landing_force_index = peak_landing_force_index)
  })
  
  output$secondary_plot <- renderPlotly({
    req(jump_information$analysis_data)
    
    data <- jump_information$analysis_data$analysis_force_data
    body_weight <- jump_information$analysis_data$body_weight
    body_weight_sd <- jump_information$analysis_data$body_weight_sd
    jump_start <- jump_information$analysis_data$jump_start_index
    peak_force <- jump_information$analysis_data$peak_force_index
    takeoff <- jump_information$analysis_data$takeoff_index
    landing <- jump_information$analysis_data$landing_index
    
    plot_ly(data,
            x = ~1:nrow(data)) %>%
      add_lines(y = ~fp1,
                name = "FP1") %>%
      add_lines(y = ~fp2,
                name = "FP2") %>%
      add_lines(y = ~total_force,
                name = "Total Force") %>%
      layout(shapes = list(vline(jump_start),
                           vline(peak_force),
                           vline(takeoff),
                           vline(landing),
                           list(type = "rect",
                                fillcolor = "blue",
                                line = list(color = "blue"),
                                opacity = 0.3,
                                x0 = 1,
                                x1 = jump_start,
                                xref = "x",
                                y0 = body_weight - body_weight_sd * 5,
                                y1 = body_weight + body_weight_sd * 5,
                                yref = "y")),
             xaxis = list(title = "Index"),
             yaxis = list(title = "Force"))
  })
  
  observe({
    req(jump_information$analysis_data)
    
    data <- jump_information$analysis_data$analysis_force_data
    jump_type <- jump_information$analysis_data$jump_type
    body_weight <- jump_information$analysis_data$body_weight
    body_mass <- jump_information$analysis_data$body_mass
    jump_start <- jump_information$analysis_data$jump_start_index
    takeoff <- jump_information$analysis_data$takeoff_index
    landing <- jump_information$analysis_data$landing_index
    sampling_frequency <- jump_information$analysis_data$sampling_frequency
    quiet_standing_length <- jump_information$analysis_data$quiet_standing_length
    
    fp1_weight <- data[1:quiet_standing_length, mean(fp1)]
    fp2_weight <- data[1:quiet_standing_length, mean(fp2)]
    
    jump_data <- data[jump_start:takeoff]
    
    jump_data[, ":=" (net_impulse = cumtrapz(1:nrow(jump_data), total_force - body_weight) / sampling_frequency,
                      fp1_net_impulse = cumtrapz(1:nrow(jump_data), fp1 - fp1_weight) / sampling_frequency,
                      fp2_net_impulse = cumtrapz(1:nrow(jump_data), fp2 - fp2_weight) / sampling_frequency)
              ][, velocity := net_impulse / body_mass
                ][, power := total_force * velocity]
    
    peak_power_index <- which.max(jump_data[, power])
    
    flight_time <- (landing - takeoff) / sampling_frequency
    net_impulse <- jump_data[, last(net_impulse)]
    jump_height_ft <- 0.5 * 9.81 * (flight_time / 2) ^ 2
    takeoff_velocity <- jump_data[, last(velocity)]
    jump_height_ni <- takeoff_velocity ^ 2 / (2 * 9.81)
    peak_force <- jump_data[, max(total_force)]
    peak_velocity <- jump_data[, max(velocity)]
    peak_power <- jump_data[, max(power)]
    force_peak_power <- jump_data[peak_power_index, total_force]
    velocity_peak_power <- jump_data[peak_power_index, velocity]
    time_to_peak_force <- which.max(jump_data[, total_force]) / sampling_frequency
    avg_rfd <- (peak_force - jump_data[, first(total_force)]) / time_to_peak_force
    contact_time <- nrow(jump_data) / sampling_frequency
    rsi_modified <- jump_height_ni / contact_time
    
    metric_table <- data.table(date = input$test_date, athlete = input$athlete_name, jump_type, trial = input$selected_trial,
                               bar_load = input$bar_load, body_mass, flight_time, net_impulse, jump_height_ft, jump_height_ni,
                               takeoff_velocity, peak_force, peak_velocity, peak_power,
                               force_peak_power, velocity_peak_power, time_to_peak_force,
                               avg_rfd, contact_time, rsi_modified)
    
    fp1_net_impulse <- jump_data[, last(fp1_net_impulse)]
    fp2_net_impulse <- jump_data[, last(fp2_net_impulse)]
    net_impulse_symmetry_index <- (fp1_net_impulse - fp2_net_impulse) / mean(c(fp1_net_impulse, fp2_net_impulse)) * 100
    fp1_peak_force <- jump_data[, max(fp1)]
    fp2_peak_force <- jump_data[, max(fp2)]
    peak_force_symmetry_index <- (fp1_peak_force - fp2_peak_force) / mean(c(fp1_peak_force, fp2_peak_force)) * 100
    fp1_peak_force_index <- which.max(jump_data[, fp1])
    fp2_peak_force_index <- which.max(jump_data[, fp2])
    fp1_time_to_peak_force <- fp1_peak_force_index / sampling_frequency
    fp2_time_to_peak_force <- fp2_peak_force_index / sampling_frequency
    time_to_peak_force_symmetry_index <- (fp1_time_to_peak_force - fp2_time_to_peak_force) / mean(c(fp1_time_to_peak_force, fp2_time_to_peak_force)) * 100
    fp1_avg_rfd <- (fp1_peak_force - jump_data[, first(fp1)]) / fp1_time_to_peak_force
    fp2_avg_rfd <- (fp2_peak_force - jump_data[, first(fp2)]) / fp2_time_to_peak_force
    avg_rfd_symmetry_index <- (fp1_avg_rfd - fp2_avg_rfd) / mean(c(fp1_avg_rfd, fp2_avg_rfd)) * 100
    
    metric_table <- cbind(metric_table, fp1_net_impulse, fp2_net_impulse, 
                          net_impulse_symmetry_index, fp1_peak_force, fp2_peak_force,
                          peak_force_symmetry_index, fp1_time_to_peak_force,
                          fp2_time_to_peak_force, time_to_peak_force_symmetry_index,
                          fp1_avg_rfd, fp2_avg_rfd, avg_rfd_symmetry_index)
    
    if(jump_type == "cmj"){
      peak_force_index <- which.max(jump_data[, total_force])
      minimum_force_index <- which.min(jump_data[1:peak_force_index, total_force])
      
      unweight_end_index <- detect_index(jump_data[minimum_force_index:peak_force_index, total_force],
                                         ~ .x >= jump_data[, first(total_force)], .dir = "forward") + minimum_force_index - 2
      braking_end_index <- detect_index(jump_data[(unweight_end_index + 1):nrow(jump_data), net_impulse],
                                        ~ .x >= 0, .dir = "forward") + unweight_end_index - 1
      
      unweight_duration <- unweight_end_index / sampling_frequency
      braking_duration <- (braking_end_index - unweight_end_index) / sampling_frequency
      concentric_duration <- (nrow(jump_data) - braking_end_index) / sampling_frequency
      
      zero_velo_index <- detect_index(jump_data[, velocity],
                                      ~ .x <= 0, .dir = "backward")
      
      force_zero_velo <- jump_data[zero_velo_index, total_force]
      
      metric_table <- cbind(metric_table, unweight_duration, braking_duration, concentric_duration, force_zero_velo)
    }
    
    jump_information$metric_data <- list(plot_data = jump_data,
                                         metric_table = metric_table,
                                         sampling_frequency = sampling_frequency)
  })
  
  output$metric_plot <- renderPlotly({
    req(jump_information$metric_data)
    
    plot_data <- jump_information$metric_data$plot_data
    
    force_plot <- plot_ly(plot_data,
                          x = ~1:nrow(plot_data)) %>%
      add_lines(y = ~total_force,
                name = "Force")
    
    velocity_plot <- plot_ly(plot_data,
                             x = ~1:nrow(plot_data)) %>%
      add_lines(y = ~velocity,
                name = "Velocity")
    
    power_plot <- plot_ly(plot_data,
                          x = ~1:nrow(plot_data)) %>%
      add_lines(y = ~power,
                name = "Power")
    
    subplot(force_plot, velocity_plot, power_plot, nrows = 3, shareX = TRUE) %>%
      layout(xaxis = list(title = "Index"))
  })
  
  output$metric_table <- function() {
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
    
    long_table <- melt(table_data, measure.vars = 1:ncol(table_data), variable.name = "Variable", value.name = "Value")
    
    output_table <- kable(long_table, digits = 3, col.names = NULL) %>%
      kable_styling(bootstrap_options = "striped") %>%
      pack_rows("Bilateral Variables", 1, 15, label_row_css = "background-color: #004687; color: #fff;") %>%
      pack_rows("Unilateral Variables", 16, 27, label_row_css = "background-color: #004687; color: #fff;")
    
    if(jump_type == "cmj"){
      output_table <- output_table %>%
        pack_rows("Phase-Related Variables", 28, 31, label_row_css = "background-color: #004687; color: #fff;")
    }
    
    output_table %>%
      scroll_box(width = "100%",
                 height = "750px")
  }
}

shinyApp(ui, server)