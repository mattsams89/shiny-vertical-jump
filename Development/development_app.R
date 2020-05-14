source("development_global.R")

# We start with the layout of the app. Layout elements are contained in the header, sidebar, and body; these are auto-translated to HTML/CSS via Shiny.
# The majority of elements will remain uncommented, as their uses are relatively straightforward.
header <- dashboardHeader(title = "Vertical Jump Analysis",
                          titleWidth = 250)

sidebar <- dashboardSidebar(
  disable = TRUE
)

# The only exception to what I said above is here. If you've never seen HTML, the below is used to change the layout and color of the app a bit.
# If you'd like to change the colors to your team/school colors, look up the hex codes via your organization's color palette standards and replace
# 004687 with the respective values. If you'd like to change the color of "Vertical Jump Analysis", too, add a line below background-color:#004687;
# with color:#XXXXXX; based on whatever color you'd like to change it to.
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
    height = 835,
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
    splitLayout(
      numericInput(inputId = "bar_load",
                   label = "Enter Bar Load",
                   value = 0),
      selectInput(inputId = "start_index",
                  label = "Jump Start Location",
                  choices = c("5SD - BW",
                              "5SD - 30ms"),
                  multiple = FALSE)
    ),
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
    actionButton(inputId = "save_trial",
                 label = "Save Trial")
  ),
  
  box(
    width = 7,
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
    
    # You'll notice input$upload_type wrapped in isolate() in several parts of the below code. This is because we're dealing with observe(), not observeEvent()
    # like above. Observe() responds to any changes in reactive inputs. E.g. new data upload, changing the filter type, etc. Since upload_type doesn't change once a given
    # file is uploaded, though, we need to isolate() it to only have it checked when other inputs change.
    if(isolate(input$upload_type) != "Multiple Trials - Long"){
      jump_information$formatted_data <- wide_data_manipulation(data = jump_information$data,
                                                                filter_type = input$filter_type,
                                                                sampling_frequency = input$sampling_frequency,
                                                                fp1_slope = input$fp1_slope,
                                                                fp1_intercept = input$fp1_intercept,
                                                                fp2_slope = input$fp2_slope,
                                                                fp2_intercept = input$fp2_intercept)
    }
    else{
      jump_information$formatted_data <- long_data_manipulation(data = jump_information$data,
                                                                filter_type = input$filter_type,
                                                                sampling_frequency = input$sampling_frequency,
                                                                fp1_slope = input$fp1_slope,
                                                                fp1_intercept = input$fp1_intercept,
                                                                fp2_slope = input$fp2_slope,
                                                                fp2_intercept = input$fp2_intercept
      )
    }
    
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
      data <- jump_information$formatted_data[[trial]]$data
    }
    else
      data <- jump_information$formatted_data$data
    
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
  
  observe({
    req(jump_information$formatted_data,
        input$selected_trial != 0,
        !is.na(input$selected_trial),
        !(input$selected_trial > jump_information$trials))
    
    trial <- input$selected_trial
    
    if(isolate(input$upload_type) != "Single Trial")
      data_list <- jump_information$formatted_data[[trial]]
    else
      data_list <- jump_information$formatted_data
    
    sampling_frequency <- input$sampling_frequency
    
    quiet_standing_length <- round(input$standing_length * sampling_frequency)
    
    req(quiet_standing_length != 0,
        !is.na(quiet_standing_length))
    
    jump_information$analysis_list <- single_jump_analysis(data_list = data_list,
                                                           sampling_frequency = sampling_frequency,
                                                           quiet_standing_length = quiet_standing_length,
                                                           offset_brush = jump_information$trial_brush,
                                                           jump_type = input$jump_type,
                                                           start_definition = input$start_index,
                                                           session = session,
                                                           date = input$test_date,
                                                           athlete = input$athlete_name,
                                                           trial = input$selected_trial,
                                                           bar_load = input$bar_load)
  })
  
  output$secondary_plot <- renderPlotly({
    req(jump_information$analysis_list)
    
    secondary_plot(analysis_list = jump_information$analysis_list$descriptive_list)
  })
  
  output$metric_plot <- renderPlotly({
    req(jump_information$analysis_list)

    jump_type <- jump_information$analysis_list$descriptive_list$jump_type
    data <- jump_information$analysis_list$subplot_data

    if(jump_type == "sj"){
      force_plot <- sj_subplot_design(data,
                                      variable = "total_force") %>%
        layout(yaxis = list(title = "<b>Force</b>"))

      velocity_plot <- sj_subplot_design(data,
                                         variable = "velocity") %>%
        layout(yaxis = list(title = "<b>Velocity</b>"))

      power_plot <- sj_subplot_design(data,
                                      variable = "power") %>%
        layout(yaxis = list(title = "<b>Power</b>"))
    }
    else{
      sampling_frequency <- input$sampling_frequency
      unweight_end_index <- round(jump_information$analysis_list$metric_table$unweight_duration * sampling_frequency)
      braking_end_index <- round(jump_information$analysis_list$metric_table$braking_duration * sampling_frequency) + unweight_end_index

      force_plot <- cmj_subplot_design(data,
                                       variable = "total_force",
                                       unweight_end_index,
                                       braking_end_index)

      velocity_plot <- cmj_subplot_design(data,
                                          variable = "velocity",
                                          unweight_end_index,
                                          braking_end_index) %>%
        layout(yaxis = list(title = "<b>Velocity</b>"))

      power_plot <- cmj_subplot_design(data,
                                       variable = "power",
                                       unweight_end_index,
                                       braking_end_index) %>%
        layout(yaxis = list(title = "<b>Power</b>"))
    }

    subplot(force_plot,
            velocity_plot,
            power_plot,
            nrows = 3,
            shareX = TRUE,
            shareY = FALSE,
            titleY = TRUE) %>%
      layout(xaxis = list(title = "Index"))
  })
  
  output$metric_table <- function(){
    req(jump_information$analysis_list)
    
    metric_table_design(data = jump_information$analysis_list$metric_table)
  }
  
  observeEvent(input$save_trial, {
    req(jump_information$analysis_list)
    
    save_function(data_to_write = jump_information$analysis_list$metric_table)
    
    showNotification(ui = "Trial Saved Successfully!",
                     duration = 2,
                     type = "message")
  })
}

shinyApp(ui, server)