source("global.R")

header <-
  dashboardHeader(title = "Vertical Jump Analysis",
                  titleWidth = 250)

sidebar <-
  dashboardSidebar(
    width = "50px",
    sidebarMenu(
      menuItem(
        text = NULL,
        tabName = "analysis",
        icon = icon("chart-line")
      ),
      menuItem(
        text = NULL,
        tabName = "interpolation",
        icon = icon("wave-square")
      )
    )
  )

body <-
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(
        HTML(
          "
            .skin-blue .main-header .logo {
                background-color:#004687;
            }
            .skin-blue .main-header .logo:hover {
                background-color:#004687;
            }
            .skin-blue .main-header .navbar {
                background-color:#004687;
            }
            .sidebar-toggle {
                display: none;
            }
            .shiny-split-layout > div {
                overflow: visible;
            }
          "
        )
      )
    ),
    tabItems(
      tabItem(
        tabName = "analysis",
        fluidPage(
          box(
            width = 3,
            height = 835,
            solidHeader = TRUE,
            splitLayout(
              selectInput(inputId = "upload_type",
                          label = "File Type",
                          choices = c("Pasco",
                                      "Multiple Trials - Wide",
                                      "Multiple Trials - Long",
                                      "Single Trial"),
                          multiple = FALSE),
              dateInput(inputId = "test_date",
                        label = "Testing Date")
            ),
            splitLayout(
              fileInput(inputId = "file_upload",
                        label = "Upload Data",
                        multiple = FALSE,
                        accept = c("\\.txt", "\\.csv")),
              selectInput(inputId = "plate_layout",
                          label = "Plate Layout",
                          choices = c("Left-Right" = "lr",
                                      "Right-Left" = "rl"))
            ),
            textInput(inputId = "athlete_name",
                      label = "Enter Athlete Name"),
            splitLayout(
              numericInput(inputId = "bar_load",
                           label = "Bar Load",
                           value = 0),
              numericInput(inputId = "selected_trial",
                           label = "Select Trial",
                           value = 1,
                           step = 1)
            ),
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
            splitLayout(
              selectInput(inputId = "start_index",
                          label = "Jump Start Location",
                          choices = c("5SD - BW",
                                      "5SD - 30ms"),
                          multiple = FALSE),
              selectInput(inputId = "inverse_check",
                          label = "Check Inverse Threshold?",
                          choices = c("Yes" = TRUE,
                                      "No" = FALSE),
                          multiple = FALSE)
            ),
            splitLayout(
              numericInput(inputId = "sampling_frequency",
                           label = "Sampling Frequency",
                           value = NA),
              numericInput(inputId = "standing_length",
                           label = "Quiet Standing",
                           value = 1,
                           min = 0.2,
                           max = 2,
                           step = 0.1)
            ),
            splitLayout(
              numericInput(inputId = "fp1_slope",
                           label = "Left Slope",
                           value = 1),
              numericInput(inputId = "fp1_intercept",
                           label = "Left Intercept",
                           value = 0)
            ),
            splitLayout(
              numericInput(inputId = "fp2_slope",
                           label = "Right Slope",
                           value = 1),
              numericInput(inputId = "fp2_intercept",
                           label = "Right Intercept",
                           value = 0)
            ),
            splitLayout(
              actionButton(inputId = "run_analysis",
                           label = "Analyze Data"),
              actionButton(inputId = "save_trial",
                           label = "Save Summary"),
              actionButton(inputId = "save_raw",
                           label = "Save Raw Curve"),
              style = "margin-top: 25px;"
            )
          ),
          
          box(
            width = 6,
            height = 835,
            solidHeader = TRUE,
            plotlyOutput(outputId = "main_plot"),
            plotlyOutput(outputId = "secondary_plot")
          ),
          
          tabBox(
            width = 3,
            height = 835,
            tabPanel(title = "Quick View",
                     plotlyOutput(outputId = "metric_plot",
                                  height = "750px")),
            tabPanel(title = "Calculated Metrics",
                     tableOutput(outputId = "metric_table"))
          )
        )
      ),
      tabItem(
        tabName = "interpolation",
        fluidPage(
          box(
            width = 3,
            # height = 500,
            solidHeader = TRUE,
            fileInput(inputId = "curve_upload",
                      label = "Upload Raw Curves",
                      multiple = FALSE,
                      accept = c("\\.txt", "\\.csv")),
            selectInput(inputId = "interp_jump_type",
                        label = "Select Jump Type",
                        choices = c("CMJ" = "cmj",
                                    "SJ" = "sj"),
                        multiple = FALSE),
            selectizeInput(inputId = "interp_trial",
                           label = "Select Trial",
                           choices = "",
                           options = list(
                             placeholder = "Required",
                             onInitialize = I("function() { this.setValue(''); }")
                           )),
            selectInput(inputId = "interp_method",
                        label = "Interpolation Method",
                        choices = c("Linear" = "linear",
                                    "Piecewise Linear" = "piecewise_linear"),
                        multiple = FALSE),
            selectInput(inputId = "interp_combine_phases",
                        label = "Combine Stretching Phases?",
                        choices = c("Yes" = TRUE,
                                    "No" = FALSE),
                        multiple = FALSE,
                        selected = FALSE),
            uiOutput(outputId = "interp_settings"),
            actionButton(inputId = "interp_save",
                         label = "Save Interpolated Data",
                         icon = icon("save"))
          ),
          box(
            width = 9,
            solidHeader = TRUE,
            splitLayout(
              plotlyOutput(outputId = "raw_curve_plot"),
              plotlyOutput(outputId = "interp_curve_plot")
            )
          )
        )
      )
    )
  )

ui <- dashboardPage(header, sidebar, body)

server <- function(input, output, session) {
  jump_information <- reactiveValues()
  
  # Analysis tab code ----
  
  observeEvent(input$file_upload, {
    jump_information$data <- NULL
    
    jump_information$data <-
      parse_file(file_location = input$file_upload$datapath,
                 upload_type = input$upload_type)
    
    updateNumericInput(session,
                       inputId = "selected_trial",
                       value = 1)
  })
  
  observeEvent(input$run_analysis, {
    req(jump_information$data,
        !is.na(input$sampling_frequency))
    
    jump_information$trial_list <-
      create_trial_list(.df = jump_information$data,
                        upload_type = isolate(input$upload_type),
                        filter_type = input$filter_type,
                        sampling_rate = input$sampling_frequency,
                        plate_layout = input$plate_layout,
                        fp1_slope = input$fp1_slope,
                        fp1_intercept = input$fp1_intercept,
                        fp2_slope = input$fp2_slope,
                        fp2_intercept = input$fp2_intercept)
    
    jump_information$processed_list <-
      detect_events(trial_list = jump_information$trial_list,
                    sampling_rate = input$sampling_frequency)
    
    jump_information$trials <-
      length(jump_information$processed_list)
    
    updateNumericInput(session,
                       inputId = "selected_trial",
                       value = 1,
                       max = jump_information$trials)
  })
  
  jump_information$trial_brush <- NULL
  
  observe({
    req(jump_information$processed_list)
    
    if (is.null(event_data(source = "plot_brush",
                           event = "plotly_brushed")$x))
      jump_information$trial_brush <- NULL
    else
      jump_information$trial_brush <-
        round(event_data(source = "plot_brush",
                         event = "plotly_brushed")$x, 3)
  })
  
  observeEvent(input$selected_trial, {
    if (!is.null(jump_information$trial_brush))
      jump_information$trial_brush <- NULL
  })
  
  output$main_plot <-
    renderPlotly({
      req(jump_information$processed_list,
          input$selected_trial != 0,
          !is.na(input$selected_trial),
          !(input$selected_trial > jump_information$trials))
      
      trial <- input$selected_trial
      
      build_primary_secondary_plot(
        .df = jump_information$processed_list[[trial]]
      )
    })
  
  observeEvent(input$inverse_check, {
    inverse_check <- input$inverse_check
    start_method <- input$start_index
    
    # !inverse_check wasn't working for some reason
    if (inverse_check == FALSE & start_method == "5SD - BW")
      updateSelectInput(session,
                        inputId = "start_index",
                        selected = "5SD - 30ms")
  })
  
  observe({
    req(jump_information$processed_list,
        input$selected_trial != 0,
        !is.na(input$selected_trial),
        !(input$selected_trial > jump_information$trials),
        input$standing_length != 0)
    
    trial <- input$selected_trial
    
    jump_information$processed_jump <-
      process_jump(
        .df = jump_information$processed_list[[trial]],
        weight_override = jump_information$trial_brush,
        quiet_standing_length = input$standing_length,
        jump_type = input$jump_type,
        start_method = input$start_index,
        inverse_check = input$inverse_check
      )
  })
  
  output$secondary_plot <-
    renderPlotly({
      req(jump_information$processed_jump)
      
      build_primary_secondary_plot(
        analyzed_data = jump_information$processed_jump
      )
    })
  
  output$metric_plot <-
    renderPlotly({
      req(jump_information$processed_jump)
      
      build_metric_plots(jump_information$processed_jump)
    })
  
  output$metric_table <- function() {
    req(jump_information$processed_jump)
    
    build_metric_table(jump_information$processed_jump)
  }
  
  observeEvent(input$save_trial, {
    req(jump_information$processed_jump)
    
    save_summary_data(
      .df = jump_information$processed_jump,
      date = input$test_date,
      name = input$athlete_name,
      trial_number = input$selected_trial,
      bar_load = input$bar_load
    )
    
    showNotification("Trial summary data saved", duration = 5)
  })
  
  observeEvent(input$save_raw, {
    req(jump_information$processed_jump)
    
    save_raw_curve(
      .df = jump_information$processed_jump,
      date = input$test_date,
      name = input$athlete_name,
      trial_number = input$selected_trial,
      bar_load = input$bar_load
    )
    
    showNotification("Raw curve data saved", duration = 5)
  })
  
  # Interpolation tab code ----
  
  output$interp_settings <-
    renderUI({
      interpolation_ui(jump_type = input$interp_jump_type,
                       method = input$interp_method,
                       combine_phases = input$interp_combine_phases)
    })
  
  observe({
    input$interp_jump_type
    input$interp_method
    input$interp_combine_phases
    
    reset("curve_length")
    reset("unweighting_length")
    reset("braking_length")
    reset("stretching_length")
    reset("propulsion_length")
  })
  
  observeEvent(input$curve_upload, {
    jump_information$raw_curves <- NULL
    
    jump_information$raw_curves <-
      parse_curve_file(file_location = input$curve_upload$datapath)
    
    updateSelectizeInput(session,
                         inputId = "interp_trial",
                         choices = names(jump_information$raw_curves))
  })
  
  output$raw_curve_plot <-
    renderPlotly({
      req(jump_information$raw_curves,
          input$interp_trial != "")
      
      build_raw_interp_plot(jump_information$raw_curves[[input$interp_trial]],
                            "Raw Data")
    })
  
  observe({
    req(jump_information$raw_curves,
        input$interp_trial != "")
    
    jump_type <- input$interp_jump_type
    
    if (jump_type == "sj" | input$interp_method == "linear") {
      req(input$curve_length != "")
      
      jump_information$interpolated_data <-
        interpolate_data(curve_list = jump_information$raw_curves,
                         trial = input$interp_trial,
                         jump_type = jump_type,
                         method = "linear",
                         combine_phases = FALSE,
                         phase_lengths = list(Curve = input$curve_length))
    } else {
      if (input$interp_combine_phases) {
        req(input$stretching_length != "",
            input$propulsion_length != "")
        
        jump_information$interpolated_data <-
          interpolate_data(curve_list = jump_information$raw_curves,
                           trial = input$interp_trial,
                           jump_type = jump_type,
                           method = "piecewise_linear",
                           combine_phases = TRUE,
                           phase_lengths = list(
                             Stretching = input$stretching_length,
                             Propulsion = input$propulsion_length))
      } else {
        req(input$unweighting_length != "",
            input$braking_length != "",
            input$propulsion_length != "")
        
        jump_information$interpolated_data <-
          interpolate_data(curve_list = jump_information$raw_curves,
                           trial = input$interp_trial,
                           jump_type = jump_type,
                           method = "piecewise_linear",
                           combine_phases = FALSE,
                           phase_lengths = list(
                             Unweighting = input$unweighting_length,
                             Braking = input$braking_length,
                             Propulsion = input$propulsion_length
                           ))
      }
    }
  })
  
  output$interp_curve_plot <-
    renderPlotly({
      req(nrow(jump_information$interpolated_data) > 0)
      
      build_raw_interp_plot(jump_information$interpolated_data,
                            "Interpolated Data")
    })
  
  observeEvent(input$interp_save, {
    req(nrow(jump_information$interpolated_data) > 0)
    
    save_interpolated_curve(jump_information$interpolated_data,
                            as.character(Sys.Date()))
    
    showNotification("Interpolated data saved", duration = 5)
  })
}

shinyApp(ui, server)