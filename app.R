#This Shiny application performs analysis of squat jump or countermovement jump data collected on PASCO force plates. Calculated variables
#are saved to 2 - 3 files based on the vertical jump type.

#UI setup begins at line 37
#Server code begins at line 534
#Data handling, processing, and filtering occur from lines 550 - 749
#Analysis begins at line 893
#Variable calculations begin at line 1076

#Importantly, no alterations of the PASCO .csv export are needed prior to analyzing the data. The reactive functions in the server
#portion of the code parse the data into trials. You can then select the trial of interest from the "Select trial" area on the left sidebar.

#Packages you'll need for analysis are listed below

#shiny: underlying framework for interface
#data.table: reads and parses uploaded data
#dplyr: formats the data and removes non-essential data
#signal: creates and applies Butterworth filter if selected
#ggplot2: plots data
#MESS: calculates impulse via auc function
#shinythemes: makes Shiny pretty
#TTR: used for simple moving average filter if selected
#purrr: used to quickly find index values satisfying search criteria (e.g. force < threshold); MUCH faster than original

#A future version of this script will allow for sampling frequencies other than 1000 Hz. The script was written
#over a two-day period, so I wasn't thinking clearly on how to account for other sampling frequencies
#e.g. determining the number of points to step back for the jump start, etc.
#I also have no data sampled below 1000 Hz, so anyone who can share some data at other frequencies (250, 300, 500, etc.)
#would be much appreciated.

#This describes the maximum allowable file size; currently set to 40 mB
options(shiny.maxRequestSize = 40 * 1024 ^ 2)

#Application begins here
shiny::shinyApp(
  #This section creates the UI
  ui = shiny::fluidPage(
    #This sets a more visually appealing theme
    theme = shinythemes::shinytheme('journal'),
    
    #This creates the sidebar
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        #File upload dialog
        shiny::fileInput('jump.data', label = 'Choose file to analyze'),
        
        #Date selection
        shiny::dateInput('jump.date', label = 'Select testing date'),
        
        #Athlete name
        shiny::textInput('athlete', label = 'Enter athlete name'),
        
        #Vertical jump type
        shiny::selectInput(
          'jump.type',
          label = 'Select jump type',
          choices = list('Squat Jump' = 'SJ',
                         'Countermovement Jump' = 'CMJ')
        ),
        
        #FP1 calibration slope
        shiny::numericInput('fp1.slope', label = 'FP1 slope', value = 1),
        
        #FP1 calibration intercept
        shiny::numericInput('fp1.intercept', label = 'FP1 intercept', value = 0),
        
        #FP2 calibration slope
        shiny::numericInput('fp2.slope', label = 'FP2 slope', value = 1),
        
        #FP2 calibration intercept
        shiny::numericInput('fp2.intercept', label = 'FP2 intercept', value = 0),
        
        #Filter type
        shiny::selectInput(
          'filter.type',
          label = 'Select filter to apply',
          choices = list(
            'None' = 'no.filter',
            'Butterworth' = 'butterworth',
            'Moving Average' = 'moving.avg'
          )
        ),
        
        #Select trial to analyze; max trials supported = 25; you can adjust this max in the code below
        shiny::numericInput(
          'trial.number',
          label = 'Select trial',
          value = 1,
          min = 1,
          max = 25
        ),
        
        #Select the bar load; you can manually add other loads below
        shiny::selectInput('bar.load', label = 'Select bar load',
                           choices = list(0, 11, 20)),
        
        #Sets sidebar width
        width = 2
      ),
      
      #This creates the main panel
      shiny::mainPanel(
        shiny::tabsetPanel(
          id = 'tabs',
          shiny::tabPanel(
            'Start',
            
            #Suppresses error messages when nothing has been selected
            shiny::tags$style(
              type = 'text/css',
              '.shiny-output-error { visibility: hidden; }',
              '.shiny-output-error:before { visibility: hidden; }'
            ),
            
            #This plots the selected trial; this plot is "brushable"
            #meaning you can click and drag across parts of the force-time curve
            shiny::fluidRow(shiny::column(
              12,
              shiny::plotOutput(
                'data.plot',
                height = 325,
                dblclick = 'data.plot_dblclick',
                brush = shiny::brushOpts(id = 'data.plot_brush',
                                         resetOnNew = T)
              )
            )),
            
            #If you click the auto analysis button, this will plot the resultant best estimate of the trial
            #This can help you decide if you want to manually select the jump area
            shiny::fluidRow(shiny::column(
              12, shiny::plotOutput('auto.plot', height = 325)
            )),
            
            shiny::fluidRow(
              style = 'margin-bottom: 20px;',
              
              #You can brush the plot and press the corresponding button
              #to add the start and end times of the selected area to the respective boxes.
              #Setting the offset period is the only requisite if the first 1000 data points
              #of the trial involve the athlete standing quietly on the force plates.
              #Otherwise, you can manually set the weighing phase
              
              #Sets the beginning and end of the system mass
              shiny::column(4, shiny::actionButton('system.mass', label = 'Set system mass')),
              
              #Sets the beginning and end for the force offset
              shiny::column(
                4,
                shiny::actionButton('force.offset', label = 'Set offset area')
              ),
              
              #Performs analysis
              #Analysis will only happen if you have a valid force offset area selected
              shiny::column(
                4,
                shiny::actionButton('auto.analysis', label = 'Perform analysis')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::numericInput(
                  'mass.start',
                  label = 'System mass start',
                  value = '',
                  step = 1
                )
              ),
              
              shiny::column(
                4,
                shiny::numericInput(
                  'os.start',
                  label = 'Offset start',
                  value = '',
                  step = 1
                )
              ),
              
              shiny::column(
                4,
                shiny::actionButton('clear.fields', label = 'Clear fields'),
                style = 'margin-top: 25px;'
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::numericInput(
                  'mass.end',
                  label = 'System mass end',
                  value = '',
                  step = 1
                )
              ),
              
              shiny::column(
                4,
                shiny::numericInput(
                  'os.end',
                  label = 'Offset end',
                  value = '',
                  step = 1
                )
              )
            )
          ),
          
          #Plots of bilateral data and bilateral variables are contained on this tab
          shiny::tabPanel(
            'Bilateral Analysis',
            
            #This prevents Shiny from throwing errors prior to plotting your data
            shiny::tags$style(
              type = 'text/css',
              '.shiny-output-error { visibility: hidden; }',
              '.shiny-output-error:before { visibility: hidden; }'
            ),
            
            #These plots correspond to their respective variable
            shiny::fluidRow(
              shiny::column(3, shiny::plotOutput('force.plot')),
              
              shiny::column(3, shiny::plotOutput('velocity.plot')),
              
              shiny::column(3, shiny::plotOutput('power.plot')),
              
              shiny::column(3, shiny::plotOutput('displacement.plot'))
            ),
            
            #A number of common vertical jump variables are included below
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Jump height-FT (cm):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('jh.ft')
              ),
              
              shiny::column(
                4,
                shiny::p('Jump height-NI (cm):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('jh.ni')
              ),
              
              shiny::column(
                4,
                shiny::p('Jump height-TV (cm):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('jh.tv')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Flight time (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('flight.time')
              ),
              
              shiny::column(
                4,
                shiny::p('Net impulse (N*s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('net.impulse')
              ),
              
              shiny::column(
                4,
                shiny::p('Takeoff velocity (m/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('takeoff.velocity')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Peak force (N):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('peak.force')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak velocity (m/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('peak.velocity')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak power (W):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('peak.power')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Peak landing force (N):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('plf')
              ),
              
              shiny::column(
                4,
                shiny::p('Force @ peak power (N):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fpp')
              ),
              
              shiny::column(
                4,
                shiny::p('Velocity @ peak power (m/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('vpp')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Time to peak force (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('ttpf')
              ),
              
              shiny::column(
                4,
                shiny::p('Average RFD (N/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('avg.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('Landing RFD (N/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('landing.rfd')
              )
            )
          ),
          
          #A plot displaying both the left and right force-time curves is contained on this tab
          #Additionally, net impulse, peak force, peak landing force, and average RFD
          #asymmetry are displayed on this tab.
          shiny::tabPanel(
            'Unilateral Analysis',
            
            #Suppresses error messages when nothing has been selected
            shiny::tags$style(
              type = 'text/css',
              '.shiny-output-error { visibility: hidden; }',
              '.shiny-output-error:before { visibility: hidden; }'
            ),
            
            #Plots both limbs' force-time curves; this plot is not brushable
            shiny::plotOutput('unilateral.plot', height = 400),
            
            #Values for respective limbs are below
            #Asymmetry is calculated as the %SI (diff / mean * 100)
            #A negative %SI favors the right leg, whereas a positive %SI favors the left leg
            #Be sure you always collect force plate A/1 as the left leg and force plate B/2 as the right leg, or
            #you will get some very confusing values
            shiny::fluidRow(
              shiny::column(4, shiny::tags$u(shiny::h2('Left leg'))),
              
              shiny::column(4, shiny::tags$u(shiny::h2('Right leg'))),
              
              shiny::column(4, shiny::tags$u(shiny::h2('% symmetry index')))
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Net impulse:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp1.net.impulse')
              ),
              
              shiny::column(
                4,
                shiny::p('Net impulse:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp2.net.impulse')
              ),
              
              shiny::column(
                4,
                shiny::p('Net impulse:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('net.impulse.asymmetry')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Peak force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp1.peak.force')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp2.peak.force')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('peak.force.asymmetry')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('RFD:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp1.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('RFD:', style = 'color:#888888'),
                shiny::verbatimTextOutput('fp2.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('RFD:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('rfd.asymmetry')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Peak landing force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp1.plf')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak landing force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp2.plf')
              ),
              
              shiny::column(
                4,
                shiny::p('Peak landing force:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('plf.asymmetry')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Landing RFD:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp1.landing.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('Landing RFD:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('fp2.landing.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('Landing RFD:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('landing.rfd.asymmetry')
              )
            )
          ),
          
          #This tab will only display values in the event that you are analyzing a CMJ
          #Plots break the CMJ into its constituent phases: weighing, unweighting, braking, and propulsion
          #Additionally, several shape-related variables are included here
          shiny::tabPanel(
            'Phasic Analysis',
            
            #Suppresses error messages when nothing has been selected
            shiny::tags$style(
              type = 'text/css',
              '.shiny-output-error { visibility: hidden; }',
              '.shiny-output-error:before { visibility: hidden; }'
            ),
            
            shiny::fluidRow(
              shiny::column(3, shiny::plotOutput('weigh.plot', height = 400)),
              
              shiny::column(3, shiny::plotOutput('unweight.plot', height = 400)),
              
              shiny::column(3, shiny::plotOutput('braking.plot', height = 400)),
              
              shiny::column(3, shiny::plotOutput('propulsion.plot', height = 400))
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Unweighting duration (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('unweight.duration')
              ),
              
              shiny::column(
                4,
                shiny::p('Braking duration (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('braking.duration')
              ),
              
              shiny::column(
                4,
                shiny::p('Propulsion duration (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('propulsion.duration')
              )
            ),
            
            shiny::fluidRow(
              shiny::column(
                4,
                shiny::p('Total duration (s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('total.duration')
              ),
              
              shiny::column(
                4,
                shiny::p('Braking RFD (N/s):', style = 'color:#888888;'),
                shiny::verbatimTextOutput('braking.rfd')
              ),
              
              shiny::column(
                4,
                shiny::p('FT-CT Ratio:', style = 'color:#888888;'),
                shiny::verbatimTextOutput('ft.ct.ratio')
              )
            )
          )
        )
      )
    )
  ),
  
  #The server code begins below
  server = function(input, output, session) {
    #Reads the CSV file
    #DO NOT change the Capstone output file prior to upload
    read.data <- shiny::reactive({
      #Uploaded data
      my.data <- input$jump.data
      
      #Checks for upload; if no data exist, nothing happens
      #If new data are uploaded, re-runs all following processes
      if (is.null(my.data))
        return(NULL)
      
      #Reads the data
      #Skips the first row of the Capstone output
      #Sometimes Capstone will collect uneven samples; na.strings converts blanks to NA
      #data.table = f saves the data as a data frame instead of a data.table
      data.table::fread(
        my.data$datapath,
        header = T,
        na.strings = c('', 'NA'),
        stringsAsFactors = F,
        skip = 1,
        data.table = F
      )
    })
    
    #This does some data cleaning to get everything ready to allow you to select individual trials
    data.format <- shiny::reactive({
      #Checks for data upload
      if (is.null(read.data()))
        return(NULL)
      
      #Pulls data from previous function
      jump.data <- read.data()
      
      #Assigns new names to all columns as R doesn't like duplicate names
      names(jump.data) <- make.names(names(jump.data), unique = T)
      
      #Only retains normal force data
      #This removes the time columns, individual force beams, calculated fields, etc.
      jump.data <-
        dplyr::select(jump.data, dplyr::starts_with('Normal'))
    })
    
    #Once the data have been cleaned in the above step,
    #this finds the total number of trials contained in the file
    trial.count <- shiny::reactive({
      #Checks for uploaded and cleaned data
      if (is.null(read.data()) | is.null(data.format()))
        return(NULL)
      
      #Counts the total number of columns to determine the number of trials
      trial.number <- length(data.format()[1, ]) / 2
    })
    
    #This updates the trial selector on the side panel to only allow selections between
    #1 and the maximum number of trials found in the above step
    shiny::observe({
      shiny::updateNumericInput(session,
                                'trial.number',
                                value = 1,
                                max = trial.count())
    })
    
    #Changing the trial selector to a new trial will re-run this function
    #Altering the calibration values will re-run this function
    trial.select <- shiny::reactive({
      #Checks for uploaded and cleaned data
      if (is.null(read.data()) | is.null(data.format()))
        return(NULL)
      
      #Pulls cleaned data
      jump.data <- data.format()
      
      #Checks for which trial was selected in the side panel
      selected.trial <- input$trial.number
      
      #Determines the column number in jump.data for fp1 for the chosen trial
      fp1 <- selected.trial * 2 - 1
      
      #Determines the column number in jump.data for fp2 for the chosen trial
      fp2 <- selected.trial * 2
      
      #Selects the two columns and removes any rows where NA are present
      #This step is here in case one plate sampled longer than the other
      jump.data <- na.omit(jump.data[, fp1:fp2])
      
      #This applies your calibration values to fp1's data
      trial.fp1 <-
        jump.data[, 1] * input$fp1.slope + input$fp1.intercept
      
      #This applies your calibration values to fp2's data
      trial.fp2 <-
        jump.data[, 2] * input$fp2.slope + input$fp2.intercept
      
      #This creates a data frame containing your data
      jump.data <- data.frame(fp1 = trial.fp1, fp2 = trial.fp2)
    })
    
    #This filters your data from the above step
    #This function is reactive, so changing the filter type will re-run this function
    trial.format <- shiny::reactive({
      #Checks for uploaded and cleaned data
      if (is.null(read.data()) | is.null(data.format()))
        return(NULL)
      
      #Checks for the selected filter type
      jump.filter <- input$filter.type
      
      #Pulls the data from the trial.select function
      jump.data <- trial.select()
      
      #Conditional based on the type of filter selected
      #If jump.filter is not 'None', this nested if-else pair
      #checks for which filter you selected
      if (jump.filter != 'no.filter') {
        #If it's a Butterworth filter
        if (jump.filter == 'butterworth') {
          #This creates a 2nd order low-pass Butterworth filter with a cutoff frequency
          #of 10 Hz
          #You can change the 2 and 10 below to change the filter order and cutoff
          #frequency, respectively
          data.filter <- signal::butter(2, 10 / (0.5 * 1000), 'low')
          
          #Isolates fp1 for filtering
          fp1 <- jump.data[, 'fp1']
          
          #Filters fp1
          fp1 <- signal::filter(data.filter, fp1)
          
          #The first 200 data points are removed from the filtered force-time data
          #Because the Butterworth filter is an ARMA filter,
          #the initial filtered data will always be 0. Further, the data increase to their
          #true value over the first 200 or so data points.
          #This is similar in idea to losing the first few data points when creating a moving
          #average.
          fp1 <- fp1[-(1:200)]
          
          #Isolates fp2 for filtering
          fp2 <- jump.data[, 'fp2']
          
          #Filters fp2
          fp2 <- signal::filter(data.filter, fp2)
          
          #The first 200 data points are removed from the filtered force-time data
          #Because the Butterworth filter is an ARMA filter,
          #the initial filtered data will always be 0. Further, the data increase to their
          #true value over the first 200 or so data points.
          #This is similar in idea to losing the first few data points when creating a rolling
          #average.
          fp2 <- fp2[-(1:200)]
          
          #Creates a data frame containing the filtered data plus a time column in ms
          #At present, only a 1000 Hz sampling frequency is acceptable
          trial.data <-
            data.frame(time = 1:(length(fp1)),
                       fp1 = fp1,
                       fp2 = fp2)
          
          #Creates a total.force column by summing fp1 and fp2
          trial.data$total.force <- rowSums(trial.data[, 2:3])
        }
        
        #If you've selected moving average
        else{
          #Isolates fp1 for filtering
          fp1 <- jump.data[, 'fp1']
          
          #Applies a 10 point moving average to fp1
          #This can be adjusted by changing 10 to another value
          #na.omit removes the lost data points from the start of the data
          fp1 <- na.omit(TTR::SMA(fp1, 10))
          
          #Isolates fp2 for filtering
          fp2 <- jump.data[, 'fp2']
          
          #Applies a 10 point moving average to fp2
          #This can be adjusted by changing 10 to another value
          #na.omit removes the lost data points from the start of the data
          fp2 <- na.omit(TTR::SMA(fp2, 10))
          
          #Creates a data frame containing the filtered data plus a time column
          #At present, only a 1000 Hz sampling frequency is acceptable
          trial.data <-
            data.frame(time = 1:(length(fp1)),
                       fp1 = fp1,
                       fp2 = fp2)
          
          #Creates a total.force column by summing fp1 and fp2
          trial.data$total.force <- rowSums(trial.data[, 2:3])
        }
      }
      
      #If you aren't applying a filter
      else{
        #fp1
        fp1 <- jump.data[, 'fp1']
        
        #fp2
        fp2 <- jump.data[, 'fp2']
        
        #Creates a data frame with your data plus a time column
        #At present, only a 1000 Hz sampling frequency is acceptable
        trial.data <-
          data.frame(time = 1:(length(fp1)),
                     fp1 = fp1,
                     fp2 = fp2)
        
        #Creates a total.force column by summing fp1 and fp2
        trial.data$total.force <- rowSums(trial.data[, 2:3])
      }
      
      #Calls the data frame from the previous if-else block
      #This is merely so future functions can use the data frame
      trial.data <- trial.data
    })
    
    #Plots the currently selected trial
    output$data.plot <- shiny::renderPlot({
      #Checks for the data frame created in trial.format
      if (is.null(trial.format()))
        return(NULL)
      
      #Plots the total.force column from the data frame created in trial.format
      #Coord_cartesian is what allows the plot to zoom by double clicking a brushed area
      #This maintains the data, whereas zooming would destroy the data and throw an error
      ggplot2::ggplot(trial.format(),
                      ggplot2::aes(x = time, y = total.force, group = 1)) + ggplot2::geom_line() +
        ggplot2::labs(title = 'Force-Time Data', x = 'Time (ms)', y = 'Force (N)') +
        ggplot2::coord_cartesian(xlim = zoom.range$x, ylim = zoom.range$y) +
        ggplot2::scale_y_continuous(breaks = round(seq(
          0, max(trial.format()$total.force), by = 100
        ), 0)) +
        ggplot2::theme(
          axis.line.x = ggplot2::element_line(),
          panel.grid = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line.y = ggplot2::element_line()
        )
    })
    
    #This sets the zoom range used in coord_cartesian above
    #This range is initially set to NULL so you see the entire trial
    zoom.range <- shiny::reactiveValues(x = NULL, y = NULL)
    
    #If you double click on the plot...
    shiny::observeEvent(input$data.plot_dblclick, {
      #This variable holds your selected x and y coordinates
      brush <- input$data.plot_brush
      
      #If you have selected an area on the plot...
      if (!is.null(brush)) {
        #Sets the xmin and xmax coordinates to what you've selected on the plot
        zoom.range$x <- c(brush$xmin, brush$xmax)
        
        #Sets the ymin and ymax coordinates to what you've selected on the plot
        zoom.range$y <- c(brush$ymin, brush$ymax)
      }
      
      #If you haven't brushed anything on the plot
      #or have removed your brush from the plot
      else{
        #Resets the x coordinates to NULL
        #This returns the plot to a non-zoomed condition
        zoom.range$x <- NULL
        
        #Resets the y coordinates to NULL
        #This returns the plot to a non-zoomed condition
        zoom.range$y <- NULL
      }
    })
    
    #Creates a vector to hold time coordinates for the weighing phase
    #This holds the time points for setting the system mass
    #This range is not required for analysis if the beginning of the trial
    #involves the athlete standing quietly
    mass.range <- shiny::reactiveValues(xmin = NULL, xmax = NULL)
    
    #Waits for you to press the system.mass button
    shiny::observeEvent(input$system.mass, {
      #Checks for brushing on the plot
      brush <- input$data.plot_brush
      
      #If you have brushed the plot, sets xmin and xmax for system.mass
      #Otherwise, resets these values to NULL
      if (!is.null(brush)) {
        mass.range$xmin <- as.integer(brush$xmin)
        
        mass.range$xmax <- as.integer(brush$xmax)
      }
      
      #If blank, sets xmin and xmax to NULL
      else{
        mass.range$xmin <- NULL
        
        mass.range$xmax <- NULL
      }
    })
    
    #These observes wait for you to press the Set System Mass button
    #They will pull the values from mass.range to populate the respective text boxes
    shiny::observe({
      shiny::updateNumericInput(session, 'mass.start', value = mass.range$xmin)
    })
    
    shiny::observe({
      shiny::updateNumericInput(session, 'mass.end', value = mass.range$xmax)
    })
    
    #Same as above; used to set offset range
    #This is the only range required for analysis
    os.range <- shiny::reactiveValues(xmin = NULL, xmax = NULL)
    
    #Waits for you to press the offset button
    shiny::observeEvent(input$force.offset, {
      #Checks for brushing
      brush <- input$data.plot_brush
      
      #If you have brushed the plot, sets xmin and xmax to respective time values
      #Otherwise, resets them to NULL
      if (!is.null(brush)) {
        os.range$xmin <- as.integer(brush$xmin)
        
        os.range$xmax <- as.integer(brush$xmax)
      }
      
      #Otherwise, resets to NULL
      else{
        os.range$xmin <- NULL
        
        os.range$xmax <- NULL
      }
    })
    
    #Once you press the offset button, this will pull the values from os.range
    #to populate their respective text boxes
    shiny::observe({
      shiny::updateNumericInput(session, 'os.start', value = os.range$xmin)
    })
    
    shiny::observe({
      shiny::updateNumericInput(session, 'os.end', value = os.range$xmax)
    })
    
    #If you press the Clear fields button, this will reset the offset and
    #system mass ranges set by their respective buttons
    shiny::observeEvent(input$clear.fields, {
      mass.range$xmin <- NULL
      mass.range$xmax <- NULL
      os.range$xmin <- NULL
      os.range$xmax <- NULL
      
      shiny::updateNumericInput(session, 'mass.start', value = '')
      shiny::updateNumericInput(session, 'mass.end', value = '')
      shiny::updateNumericInput(session, 'os.start', value = '')
      shiny::updateNumericInput(session, 'os.end', value = '')
      
    })
    
    #This performs the automatic analysis
    #You must have uploaded data and selected an offset area for this function to be carried out
    #System mass is an optional parameter in this case
    shiny::observeEvent(input$auto.analysis, {
      #Checks that data are uploaded and that you have selected an offset area
      if (is.null(trial.format()))
        return(NULL)
      
      #These identifiers are pulled from the sidebar
      #They will be saved to the output
      #Furthermore, jump.type determines the analysis function to run
      athlete <- input$athlete
      jump.date <- input$jump.date
      trial.number <- input$trial.number
      jump.type <- input$jump.type
      filter.type <- input$filter.type
      bar.load <- input$bar.load
      
      #This pulls the offset time values if you have selected an offset area
      if(!is.null(os.range$xmin)){
        os.start <- input$os.start
        os.end <- input$os.end
      }
      
      else{
        os.start <- 'NA'
        os.end <- 'NA'
      }
      
      
      #This pulls the system mass time values if you have selected a system mass area
      if (!is.null(mass.range$xmin)) {
        mass.start <- input$mass.start
        mass.end <- input$mass.end
      }
      
      #Pulls the formatted trial data and changes the names of the columns to objects
      #This made the coding aspect of this easier
      jump.data <- trial.format()
      
      time = 'time'
      fp1 = 'fp1'
      fp2 = 'fp2'
      total.force = 'total.force'
      
      #Sets the flight time threshold
      #This can be adjusted to your needs
      threshold <- 20
      
      #Finds the end of the trial; this is used to search parts of the force-time curve later
      #This could have been alternatively calculated as length(jump.data[, time])
      #Either provides the same result
      max.length <- max(jump.data[, time])
      
      #This portion of code runs if you've selected an offset area
      #Calculates the offset for each plate and for the overall force value
      if(!is.null(os.range$xmin)){
        fp1.offset <- mean(jump.data[os.start:os.end, fp1])
        fp2.offset <- mean(jump.data[os.start:os.end, fp2])
        total.offset <- mean(jump.data[os.start:os.end, total.force])
        
        #Offsets the data
        jump.data[, fp1] <- jump.data[, fp1] - fp1.offset
        jump.data[, fp2] <- jump.data[, fp2] - fp2.offset
        jump.data[, total.force] <-
          jump.data[, total.force] - total.offset
      }
      
      #Determines the system weight
      #This will be important for determining the threshold for jump initiation,
      #calculating velocity, and calculating impulse later on
      #How this is calculated depends on whether you selected a system mass area
      
      #If you have selected a system mass area, this portion of code runs
      if (!is.null(mass.range$xmin)) {
        fp1.system.weight <- mean(jump.data[mass.start:mass.end, fp1])
        fp2.system.weight <-
          mean(jump.data[mass.start:mass.end, fp2])
        total.system.weight <-
          mean(jump.data[mass.start:mass.end, total.force])
        
        #This determines the standard deviation during system weight calculation
        #Used to determine the threshold for the jump start
        system.weight.sd <-
          sd(jump.data[mass.start:mass.end, total.force])
      }
      
      #Otherwise, the first second of data is taken from the start of the trial
      else{
        fp1.system.weight <- mean(jump.data[1:1000, fp1])
        fp2.system.weight <- mean(jump.data[1:1000, fp2])
        total.system.weight <- mean(jump.data[1:1000, total.force])
        
        #This determines the standard deviation during system weight calculation
        #Used to determine the threshold for the jump start
        system.weight.sd <- sd(jump.data[1:1000, total.force])
      }
      
      #Sets the system mass
      fp1.system.mass <- fp1.system.weight / 9.81
      fp2.system.mass <- fp2.system.weight / 9.81
      total.system.mass <- total.system.weight / 9.81
      
      #Finds the takeoff and landing points from total.force; how this is determined depends 
      #on whether you set an offset area
      
      #If an offset area is selected
      if(!is.null(os.range$xmin)){
        
        #Searches backward from the start of the offset time value
        #1 is added to this value to correctly represent when the athlete is no longer on the force plate
        takeoff <-
          purrr::detect_index(jump.data[1:os.start, total.force], ~ .x > threshold, .right = T) + 1
        
        #Finds landing from total.force; searches forward from the end of the offset time value
        #1 is subtracted here to reflect the fact we're searching for the first point the athlete is "on" the force plate
        #Subtracting 1 pulls us back to the final point they're not on the plate
        landing <-
          purrr::detect_index(jump.data[os.end:max.length, total.force], ~ .x > threshold) + os.end - 1
      }
      
      #Otherwise, searches from beginning (or the end of the system mass area) to the first value 
      #below the threshold for takeoff
      #and backwards from the end of the file to find landing
      #Importantly, you must set these areas if the beginning or end of 
      #your trial contains points where the athlete is not on the plate
      else{
        
        #Determines if you've set a system mass area
        #If so, finds takeoff from the end of the system mass range
        if(!is.null(mass.range$xmin)){
          takeoff <-
            purrr::detect_index(jump.data[mass.end:max.length, total.force], ~ .x < threshold) + mass.end - 1
        }
        
        #Otherwise, it searches from the beginning of the trial
        else{
          takeoff <-
            purrr::detect_index(jump.data[1:max.length, total.force], ~ .x < threshold)
        }
        
        landing <-
          purrr::detect_index(jump.data[takeoff:max.length, total.force], ~ .x < threshold, .right = T) + takeoff
      }
      
      #Finds peak force prior to takeoff
      #Used for searching for jump initiation later on
      peak.force <- max(jump.data[1:takeoff, total.force])
      
      #Determines the time value associated with peak force
      peak.force.time <-
        purrr::detect_index(jump.data[1:takeoff, total.force], ~ .x == peak.force)
      
      #If you're analyzing an SJ
      if (jump.type == 'SJ') {
        #Threshold force value for SJ initiation
        #This is based on recommendations from Owen et al. (2014) and McMahon et al. (2018)
        sj.threshold <- total.system.weight + system.weight.sd * 5
        
        #Determines time at which jump begins
        #This searches the data in reverse from peak force to the beginning of the trial
        #for the first point that falls below the threshold calculated above
        #The window is moved back an additional 30 ms
        #based on Owen et al. (2014) and McMahon et al. (2018)
        #A future version will allow for sampling frequencies other than 1000 Hz
        #This will be implemented through multiplying the sampling frequency by 0.03 and
        #rounding to the nearest integer. This value will correspond to the appropriate number
        #of points to step back in the data
        j.start <- 
          purrr::detect_index(jump.data[1:peak.force.time, total.force], ~ .x < sj.threshold, .right = T) - 30
      }
      
      #If you're analyzing a CMJ, the threshold and search are a bit different
      #Also, inclusion of a system mass area is handled a bit differently in CMJ compared to SJ
      #due to how the threshold value is found. In SJ, the function searches backward from peak force
      #to the first point that falls below the threshold for jump initiation.
      #Conversely, CMJ searches backward from minimum force to the first point that is greater than the threshold
      #for jump initiation. If a trial is uploaded where the athlete is not on standing on the force plate
      #at the beginning, minimum force will occur during the plate being unloaded.
      #This caused the function to throw an error and crash. Now, when a system mass area is manually specified,
      #the search window for minimum force is constrained from mass.end to peak.force.time. This should fix
      #any crashes when you specify a system mass area in trials where the force plate is unloaded at the start.
      else{
        #Threshold force value for CMJ initiation
        #Based on reommendation from Owen et al. (2014) and McMahon et al. (2018)
        cmj.threshold <- total.system.weight - system.weight.sd * 5
        
        #Finds minimum force during the countermovement;
        #Used to search for jump initiation
        
        #How this is calculated will depend on whether you selected a system mass area previously
        #If you have selected a system mass range
        if (!is.null(mass.range$xmin)) {
          cmj.min.force <-
            min(jump.data[mass.end:peak.force.time, total.force])
          
          #This determines the time at which minimum force occurs during the CMJ
          #mass.end is added here to calculate the correct time value
          cmj.min.force.time <-
            purrr::detect_index(jump.data[mass.end:peak.force.time, total.force], ~ .x == cmj.min.force) + mass.end - 1
        }
        
        else{
          cmj.min.force <- min(jump.data[1:peak.force.time, total.force])
          
          #This determines the time at which minimum force occurs during the CMJ
          #No correction is needed here as long as the beginning of the trial involves
          #the athlete standing quietly on the force plate
          cmj.min.force.time <-
            purrr::detect_index(jump.data[1:peak.force.time, total.force], ~ .x == cmj.min.force)
        }
        
        #Determines time at which jump begins
        #This searches in reverse from minimum force to the beginning of the trial
        #for the first point greater than the threshold calculated above
        #The window is moved back an additional 30 ms
        #Based on recommendations from Owen et al. (2014) and McMahon et al. (2018)
        #A future version will allow for sampling frequencies other than 1000 Hz
        #This will be implemented through multiplying the sampling frequency by 0.03 and
        #rounding to the nearest integer. This value will correspond to the appropriate number
        #of points to step back in the data
        j.start <- 
          purrr::detect_index(jump.data[1:cmj.min.force.time, total.force], ~ .x > cmj.threshold, .right = T) - 30
      }
      
      #Variables of interest are calculated from here#
      #----------------------------------------------#
      #----------------------------------------------#
      #----------------------------------------------#
      
      #Begin by only retaining force-time, velocity-time, power-time, and displacement-time curves
      #This is time-saving over searching the entire force vector each time
      fp1.force.data <- jump.data[j.start:takeoff, fp1]
      
      fp2.force.data <- jump.data[j.start:takeoff, fp2]
      
      force.data <- jump.data[j.start:takeoff, total.force]
      
      #At sufficient sampling frequency, there doesn't seem to be a significant difference
      #between cumsum and using an additional package to integrate force data to obtain velocity
      velocity.data <-
        cumsum(((force.data) / total.system.mass - 9.81) * 0.001)
      
      #Calculates power from force and velocity data
      power.data <- force.data * velocity.data
      
      #Calculates displacement from velocity
      #Similar to velocity, there is little advantage to using an integration function over
      #cumsum at sufficient sampling frequency
      displacement.data <- (cumsum(velocity.data) * 0.001) * 100
      
      #Bilateral variables#
      #-------------------#
      #-------------------#
      #-------------------#
      
      #Used to determine ft:ct and jump height from flight time
      flight.time <- (landing - takeoff) / 1000
      
      #Jump height from flight time
      #*100 to convert to cm
      jh.ft <- 0.5 * 9.81 * (flight.time / 2) ^ 2 * 100
      
      #This function calculates net impulse for the jump
      #auc requires 5 things: a time column, your data,
      #where to start in the data, where to end in the data,
      #and the type of interpolation to use (linear or spline)
      #Because we're sampling at 1000 Hz, spline offers little advantage
      #over linear
      #System weight is removed from the force data to give us net impulse
      net.impulse <- (
        MESS::auc(
          seq_along(force.data),
          force.data - total.system.weight,
          from = 1,
          to = max(seq_along(force.data)),
          type = 'linear'
        )
      ) / 1000
      
      #Jump height from net impulse
      #*100 to convert to cm
      jh.ni <-
        (net.impulse / total.system.mass) ^ 2 / (2 * 9.81) * 100
      
      #Determines velocity at takeoff
      #Tail retains x number of data points from the end of a vector
      #So tail(,1) retains the last velocity data point prior to takeoff
      takeoff.velocity <- tail(velocity.data, 1)
      
      #Calculates jump height from takeoff velocity
      #This value is in cm
      jh.tv <- takeoff.velocity ^ 2 / 2 * 9.81
      
      #Finds peak force in the force.data vector
      peak.force <- max(force.data)
      
      #Finds peak velocity
      peak.velocity <- max(velocity.data)
      
      #Finds peak power
      peak.power <- max(power.data)
      
      #Finds the time at which peak power occurs
      #This is used to determine fpp and vpp
      peak.power.time <- 
        purrr::detect_index(power.data, ~ .x == peak.power)
      
      #Finds the peak force after the athlete lands
      plf <- max(jump.data[landing:max.length, total.force])
      
      #Force @ peak power
      #Because force.data is a vector, calling peak.power.time in brackets
      #returns the force value associated with that time point
      fpp <- force.data[peak.power.time]
      
      #Velocity @ peak power
      #Same idea as fpp
      vpp <- velocity.data[peak.power.time]
      
      #Determines the time required to reach peak force
      #This is used for the avg.rfd calculation shortly
      ttpf <- 
        purrr::detect_index(force.data, ~ .x == peak.force) / 1000
      
      #Determines the time required to reach peak landing force
      #This is used to calculated landing rfd
      ttplf <-
        (purrr::detect_index(jump.data[landing:max.length, total.force], ~ .x == plf) - 1) / 1000
      
      #The force at the start of the jump
      #Like tail, head() retains x number of points from the beginning of a vector
      #In this case, it retains the first
      initial.force <- head(force.data, 1)
      
      #Average rate of force development
      #Peak rfd is not currently implemented, as it would add
      #considerable computational time to the function
      #as loops are frowned upon typically in R
      avg.rfd <- (peak.force - initial.force) / ttpf
      
      #Landing rate of force development
      landing.rfd <- (plf - jump.data[landing, total.force]) / ttplf
      
      #This is the plot that appears underneath the data plot on the start tab
      #This plots the jump with some room on either side via coord_cartesian to give
      #a zoomed-in look at the data. Three vertical lines mark
      #the start of the jump, takeoff, and landing
      output$auto.plot <- shiny::renderPlot({
        ggplot2::ggplot(jump.data, ggplot2::aes(x = time, y = total.force)) + ggplot2::geom_line() +
          ggplot2::geom_vline(xintercept = j.start,
                              colour = 'red',
                              linetype = 'dotdash') +
          ggplot2::geom_vline(xintercept = takeoff,
                              colour = 'red',
                              linetype = 'dotdash') +
          ggplot2::geom_vline(xintercept = landing,
                              colour = 'red',
                              linetype = 'dotdash') +
          ggplot2::coord_cartesian(xlim = c(j.start - 200, landing + ttplf * 1000 + 100)) +
          ggplot2::labs(title = 'Force-Time Data', x = 'Time (ms)', y = 'Force (N)') +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #This is the first plot on the bilateral analysis tab
      #This displays the force-time curve from jump initiation to takeoff
      output$force.plot <- shiny::renderPlot({
        ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
          x = seq_along(force.data),
          y = force.data,
          group = 1
        )) +
          ggplot2::labs(title = 'Force Data', x = 'Time (ms)', y = 'Force (N)') +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #This displays the velocity-time curve from jump initiation to takeoff
      output$velocity.plot <- shiny::renderPlot({
        ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
          x = seq_along(velocity.data),
          y = velocity.data,
          group = 1
        )) +
          ggplot2::labs(title = 'Velocity Data', x = 'Time (ms)', y = 'Velocity (m/s)') +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #This displays the power-time curve from jump initiation to takeoff
      output$power.plot <- shiny::renderPlot({
        ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
          x = seq_along(power.data),
          y = power.data,
          group = 1
        )) +
          ggplot2::labs(title = 'Power Data', x = 'Time (ms)', y = 'Power (W)') +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #This displays the displacement-time curve from jump initiation to takeoff
      output$displacement.plot <- shiny::renderPlot({
        ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
          x = seq_along(displacement.data),
          y = displacement.data,
          group = 1
        )) +
          ggplot2::labs(title = 'Displacement Data', x = 'Time (ms)', y = 'Displacement (cm)') +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #The following outputs display values in their respective text boxes on the bilateral analysis tab
      #You can read through them if you're curious, but I won't be commenting each output individually
      #They are named in a straightforward manner that should hopefully avoid confusion
      output$jh.ft <- shiny::renderText({
        jh.ft
      })
      
      output$jh.ni <- shiny::renderText({
        jh.ni
      })
      
      output$jh.tv <- shiny::renderText({
        jh.tv
      })
      
      output$flight.time <- shiny::renderText({
        flight.time
      })
      
      output$net.impulse <- shiny::renderText({
        net.impulse
      })
      
      output$takeoff.velocity <- shiny::renderText({
        takeoff.velocity
      })
      
      output$peak.force <- shiny::renderText({
        peak.force
      })
      
      output$peak.velocity <- shiny::renderText({
        peak.velocity
      })
      
      output$peak.power <- shiny::renderText({
        peak.power
      })
      
      output$plf <- shiny::renderText({
        plf
      })
      
      output$fpp <- shiny::renderText({
        fpp
      })
      
      output$vpp <- shiny::renderText({
        vpp
      })
      
      output$ttpf <- shiny::renderText({
        ttpf
      })
      
      output$avg.rfd <- shiny::renderText({
        avg.rfd
      })
      
      output$landing.rfd <- shiny::renderText({
        landing.rfd
      })
      
      #Unilateral variables#
      #--------------------#
      #--------------------#
      #--------------------#
      
      #Net impulse is calculated for fp1 below
      #This is functionally identical to the bilateral impulse calculation earlier,
      #although a limitation exists in that the function uses the same system weight
      #window as the bilateral impulse calculation. If the athlete shifts their weight
      #prior to jump initiation, this could lead to some seriously wonky impulse values
      #One possible fix is to use the force value at jump initiaiton instead
      fp1.net.impulse <- (
        MESS::auc(
          seq_along(fp1.force.data),
          fp1.force.data - fp1.system.weight,
          from = 1,
          to = max(seq_along(fp1.force.data)),
          type = 'linear'
        )
      ) / 1000
      
      #See above comments
      fp2.net.impulse <- (
        MESS::auc(
          seq_along(fp2.force.data),
          fp2.force.data - fp2.system.weight,
          from = 1,
          to = max(seq_along(fp2.force.data)),
          type = 'linear'
        )
      ) / 1000
      
      #fp1 peak force
      fp1.peak.force <- max(fp1.force.data)
      
      #Time required to reach peak force on fp1
      #Used for fp1 avg RFD
      fp1.ttpf <- 
        purrr::detect_index(fp1.force.data, ~ .x == fp1.peak.force) / 1000
      
      #Determines the initial force at initiation of the jump
      #Also used for fp1 avg RFD
      fp1.initial.force <- head(fp1.force.data, 1)
      
      #Calculated from the above variables
      fp1.rfd <- (fp1.peak.force - fp1.initial.force) / fp1.ttpf
      
      #Peak landing force for fp1
      fp1.plf <- max(jump.data[landing:max.length, fp1])
      
      #Time requied to reach peak landing force on fp1
      #Importantly, this may not align with the bilateral time value
      #as the athlete may load the limbs significantly differently from one another
      #when landing
      fp1.ttplf <-
        (purrr::detect_index(jump.data[landing:max.length, fp1], ~ .x == fp1.plf) - 1) / 1000
      
      #Landing RFD for fp1 based on above variables
      fp1.landing.rfd <-
        (fp1.plf - jump.data[landing, fp1]) / fp1.ttplf
      
      #These variable calculations mirror those for fp1
      #fp2 peak force
      fp2.peak.force <- max(fp2.force.data)
      
      #Time required to reach fp2 peak force
      #Used for fp2 avg RFD
      fp2.ttpf <- 
        purrr::detect_index(fp2.force.data, ~ .x == fp2.peak.force) / 1000
      
      #fp2 force at initiation of the jump
      #Used for fp2 avg RFD
      fp2.initial.force <- head(fp2.force.data, 1)
      
      #fp2 avg RFD
      fp2.rfd <- (fp2.peak.force - fp2.initial.force) / fp2.ttpf
      
      #fp2 peak landing force
      #used for landing rfd
      fp2.plf <- max(jump.data[landing:max.length, fp2])
      
      #Time required to reach fp2 peak landing force
      fp2.ttplf <-
        (purrr::detect_index(jump.data[landing:max.length, fp2], ~ .x == fp2.plf) - 1) / 1000
      
      #Based on above variables
      fp2.landing.rfd <-
        (fp2.plf - jump.data[landing, fp2]) / fp2.ttplf
      
      #Asymmetry calculations are below
      #A positive value favors fp1, while a negative value favors fp2
      #Asymmetry between net impulse values
      net.impulse.asymmetry <-
        (fp1.net.impulse - fp2.net.impulse) / mean(c(fp1.net.impulse, fp2.net.impulse)) * 100
      
      #Peak force asymmetry
      peak.force.asymmetry <-
        (fp1.peak.force - fp2.peak.force) / mean(c(fp1.peak.force, fp2.peak.force)) * 100
      
      #RFD asymmetry
      rfd.asymmetry <-
        (fp1.rfd - fp2.rfd) / mean(c(fp1.rfd, fp2.rfd)) * 100
      
      #PLF asymmetry
      plf.asymmetry <-
        (fp1.plf - fp2.plf) / mean(c(fp1.plf, fp2.plf)) * 100
      
      #Landing rfd asymmetry
      landing.rfd.asymmetry <-
        (fp1.landing.rfd - fp2.landing.rfd) / mean(c(fp1.landing.rfd, fp2.landing.rfd)) * 100
      
      #Plots the data for the individual limbs with space before jump initiation
      #and after landing thanks to coord_cartesian call
      #Vertical lines denote jump initiation, takeoff, and landing
      output$unilateral.plot <- shiny::renderPlot({
        ggplot2::ggplot(jump.data, ggplot2::aes(x = time)) + ggplot2::geom_line(ggplot2::aes(y = fp1, colour = 'Left')) +
          ggplot2::geom_line(ggplot2::aes(y = fp2, colour = 'Right')) +
          ggplot2::geom_vline(xintercept = j.start, linetype = 'dotdash') +
          ggplot2::geom_vline(xintercept = takeoff, linetype = 'dotdash') +
          ggplot2::geom_vline(xintercept = landing, linetype = 'dotdash') +
          ggplot2::labs(
            title = 'Unilateral Force-Time Data',
            x = 'Time (ms)',
            y = 'Force (N)',
            colour = 'Leg'
          ) +
          ggplot2::coord_cartesian(xlim = c(j.start - 200, landing + ttplf * 1000 + 100)) +
          ggplot2::theme(
            axis.line.x = ggplot2::element_line(),
            panel.grid = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line.y = ggplot2::element_line()
          )
      })
      
      #The following outputs add text to their respective text boxes
      #They will not be individually commented
      #Similar to the bilateral variables, the names shold be self explanatory
      output$fp1.net.impulse <- shiny::renderText({
        fp1.net.impulse
      })
      
      output$fp2.net.impulse <- shiny::renderText({
        fp2.net.impulse
      })
      
      output$net.impulse.asymmetry <- shiny::renderText({
        net.impulse.asymmetry
      })
      
      output$fp1.peak.force <- shiny::renderText({
        fp1.peak.force
      })
      
      output$fp2.peak.force <- shiny::renderText({
        fp2.peak.force
      })
      
      output$peak.force.asymmetry <- shiny::renderText({
        peak.force.asymmetry
      })
      
      output$fp1.rfd <- shiny::renderText({
        fp1.rfd
      })
      
      output$fp2.rfd <- shiny::renderText({
        fp2.rfd
      })
      
      output$rfd.asymmetry <- shiny::renderText({
        rfd.asymmetry
      })
      
      output$fp1.plf <- shiny::renderText({
        fp1.plf
      })
      
      output$fp2.plf <- shiny::renderText({
        fp2.plf
      })
      
      output$plf.asymmetry <- shiny::renderText({
        plf.asymmetry
      })
      
      output$fp1.landing.rfd <- shiny::renderText({
        fp1.landing.rfd
      })
      
      output$fp2.landing.rfd <- shiny::renderText({
        fp2.landing.rfd
      })
      
      output$landing.rfd.asymmetry <- shiny::renderText({
        landing.rfd.asymmetry
      })
      
      #Phasic analysis; this is only carried out for CMJ#
      #-------------------------------------------------#
      #-------------------------------------------------#
      #-------------------------------------------------#
      
      #Checks if analyzing a CMJ
      if (jump.type == 'CMJ') {
        #Jump length
        #used for ft:ct
        #This is not divided by 1000 yet as it's used in a few other places
        total.duration <- length(force.data)
        
        #FT:CT
        ft.ct.ratio <- flight.time / (total.duration / 1000)
        
        #Used to determine unweghting phase length
        #The length is subtracted from peak.force.time as the function searches the data in reverse
        #from peak.force.time to j.start
        #The time point represents the final point at which force is below the force at the start of the
        #unweighting phase. This marks the demarcation between unweighting and braking
        unweight.end.time <- 
          purrr::detect_index(jump.data[j.start:peak.force.time, total.force], ~ .x < initial.force, .right = T) + j.start - 1
        
        #Finds the length of the unweighting phase
        #Subtracts j.start from unweight.end.time to get unweight.duration
        unweight.duration <- unweight.end.time - j.start
        
        #Calculates the absolute value of the unweighting phase impulse
        #This is done because the absolute value of the net impulse of the unweighting phase
        #corresponds to the net impulse of the braking phase
        unweight.impulse <- abs((
          MESS::auc(
            seq_along(force.data),
            force.data - total.system.weight,
            from = 1,
            to = unweight.duration,
            type = 'linear'
          )
        ) / 1000)
        
        #This variable is used in a loop that finds the end of the braking phase
        #by increasing the size of the net impulse calculation window
        #until braking.impulse exceeds the absolute value of the
        #unweighting impulse
        b = unweight.duration + 2
        
        repeat {
          braking.impulse <- (
            MESS::auc(
              seq_along(force.data),
              force.data - total.system.weight,
              from = unweight.duration + 1,
              to = b,
              type = 'linear'
            )
          ) / 1000
          
          #The loop continues until braking impulse exceeds unweighting impulse
          if (braking.impulse > unweight.impulse) {
            break
          }
          
          #Adds one to the variable b to increase the impulse calculation window
          #until braking impulse exceeds unweighting impulse
          else{
            b = b + 1
          }
        }
        
        #The corresponding time value to the end of the braking phase
        braking.end <- b
        
        #The length of the braking phase
        braking.duration <- (b - unweight.duration) / 1000
        
        #The force at the end of the braking phase
        #Used for braking RFD
        braking.end.force <- force.data[braking.end]
        
        #Calculated from the variables above
        braking.rfd <-
          (braking.end.force - force.data[(unweight.duration + 1)]) / braking.duration
        
        #Propulsion phase duration
        propulsion.duration <- (total.duration - braking.end) / 1000
        
        #Adds values to text boxes
        #These will not be individually commented, although
        #the names should be straightforward
        
        #This is divided by 1000 as this was not done when creating
        #the variable earlier
        output$unweight.duration <- shiny::renderText({
          unweight.duration / 1000
        })
        
        output$braking.duration <- shiny::renderText({
          braking.duration
        })
        
        output$propulsion.duration <- shiny::renderText({
          propulsion.duration
        })
        
        output$braking.rfd <- shiny::renderText({
          braking.rfd
        })
        
        output$total.duration <- shiny::renderText({
          total.duration / 1000
        })
        
        output$ft.ct.ratio <- shiny::renderText({
          ft.ct.ratio
        })
        
        #Plots the weighing phase
        #If a system mass area is manually selected, this code creates the plot
        output$weigh.plot <- shiny::renderPlot({
          if (!is.null(mass.range$xmin))
            ggplot2::ggplot(jump.data[mass.start:mass.end, ]) + ggplot2::geom_line(ggplot2::aes(
              x = time,
              y = total.force,
              group = 1
            )) +
            ggplot2::labs(title = 'Weigh-In',
                          x = 'Time (ms)',
                          y = 'Force (N)') +
            ggplot2::theme(
              axis.line.x = ggplot2::element_line(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line.y = ggplot2::element_line()
            ) +
            ggplot2::coord_cartesian(ylim = c(0, max(force.data) + 100)) +
            ggplot2::geom_hline(yintercept = initial.force, linetype = 'dotdash')
          
          #Otherwise, the first 1000 data points are pulled to create the plot
          else
            ggplot2::ggplot(jump.data[1:1000, ]) + ggplot2::geom_line(ggplot2::aes(
              x = time,
              y = total.force,
              group = 1
            )) +
            ggplot2::labs(title = 'Weigh-In',
                          x = 'Time (ms)',
                          y = 'Force (N)') +
            ggplot2::theme(
              axis.line.x = ggplot2::element_line(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line.y = ggplot2::element_line()
            ) +
            ggplot2::coord_cartesian(ylim = c(0, max(force.data) + 100)) +
            ggplot2::geom_hline(yintercept = initial.force, linetype = 'dotdash')
        })
        
        #Plots the unweighting phase
        output$unweight.plot <- shiny::renderPlot({
          ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
            x = seq_along(force.data[1:unweight.duration]),
            y = force.data[1:unweight.duration],
            group = 1
          )) +
            ggplot2::labs(title = 'Unweighting',
                          x = 'Time (ms)',
                          y = 'Force (N)') +
            ggplot2::theme(
              axis.line.x = ggplot2::element_line(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line.y = ggplot2::element_line()
            ) +
            ggplot2::coord_cartesian(ylim = c(0, max(force.data) + 100)) +
            ggplot2::geom_hline(yintercept = initial.force, linetype = 'dotdash')
        })
        
        #Plots the braking phase
        output$braking.plot <- shiny::renderPlot({
          ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
            x = seq_along(force.data[(unweight.duration + 1):braking.end]),
            y = force.data[(unweight.duration + 1):braking.end],
            group = 1
          )) +
            ggplot2::labs(title = 'Braking',
                          x = 'Time (ms)',
                          y = 'Force (N)') +
            ggplot2::theme(
              axis.line.x = ggplot2::element_line(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line.y = ggplot2::element_line()
            ) +
            ggplot2::coord_cartesian(ylim = c(0, max(force.data) + 100)) +
            ggplot2::geom_hline(yintercept = initial.force, linetype = 'dotdash')
        })
        
        #Plots the propulsion phase
        output$propulsion.plot <- shiny::renderPlot({
          ggplot2::ggplot() + ggplot2::geom_line(ggplot2::aes(
            x = seq_along(force.data[braking.end:total.duration]),
            y = force.data[braking.end:total.duration],
            group = 1
          )) +
            ggplot2::labs(title = 'Propulsion',
                          x = 'Time (ms)',
                          y = 'Force (N)') +
            ggplot2::theme(
              axis.line.x = ggplot2::element_line(),
              panel.grid = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line.y = ggplot2::element_line()
            ) +
            ggplot2::coord_cartesian(ylim = c(0, max(force.data) + 100)) +
            ggplot2::geom_hline(yintercept = initial.force, linetype = 'dotdash')
        })
      }
      
      #Name of the current directory (folder)
      #e.g. 'D:/Dropbox/R Work/Shiny Vertical Jump'
      current.directory <- getwd()
      
      #Name of the directory where we want to save the data
      save.directory <- 'Analyses'
      
      #Creates the full save directory path
      #e.g. 'D:/Dropbox/R Work/Shiny Vertical Jump/Analyses'
      directory <- file.path(current.directory, save.directory)
      
      #Checks if the directory above exists
      #If not, creates the new directory
      if(!dir.exists(directory))
        dir.create(directory)
      
      #Names of the 2 - 3 files to save the data to
      #The file names are based on the date you select in the sidebar
      bilateral.filename <- paste(jump.date, 'Bilateral Analysis.csv', sep = ' ')
      unilateral.filename <- paste(jump.date, 'Unilateral Analysis.csv', sep = ' ')
      
      #Creates the full file paths based on the directory and the file names
      bilateral.file <- file.path(directory, bilateral.filename)
      unilateral.file <- file.path(directory, unilateral.filename)
      
      #Checks if the bilateral analysis file exists
      #If not, creates the file with the necessary header
      if(!file.exists(bilateral.file)){
        file.create(bilateral.file)
        
        #Adds header to the bilateral analysis file
        write.table(matrix(c('jump.date','athlete','jump.type','trial','bar.load','jh.ft','jh.ni','jh.tv','ft','ni','tv',
                             'pf','pv','pp','plf','fpp','vpp','ttpf','avg.rfd','landing.rfd','os.start','os.end','j.start',
                             'takeoff','landing'), ncol = 25), file = bilateral.file, row.names = F, col.names = F,
                    sep = ',', append = F)
      }
      
      #Writes data to the bilateral analysis file
      write.table(matrix(c(as.character(jump.date), athlete, jump.type, trial.number, bar.load, jh.ft, jh.ni, jh.tv,
                           flight.time, net.impulse, takeoff.velocity, peak.force, peak.velocity, peak.power,
                           plf, fpp, vpp, ttpf, avg.rfd, landing.rfd, os.start, os.end, j.start, takeoff, landing),
                         ncol = 25), file = bilateral.file, row.names = F, col.names = F, sep = ',', append = T)
      
      #Checks if the unilateral analysis file exists
      #If not, creates the file with the necessary header
      if(!file.exists(unilateral.file)){
        file.create(unilateral.file)
        
        #Adds header to the unilateral analysis file
        write.table(matrix(c('jump.date','athlete','jump.type','trial','bar.load','plate','ni','pf','rfd','plf','landing.rfd'),
                           ncol = 11), file = unilateral.file, row.names = F, col.names = F, sep = ',', append = F)
      }
      
      #Writes data to the unilateral analysis file
      #This occurs three times: fp1, fp2, and asymmetry
      #Each write will include what the values represent--fp1, fp2, or asymmetry
      write.table(matrix(c(as.character(jump.date), athlete, jump.type, trial.number, bar.load, 'fp1', fp1.net.impulse,
                           fp1.peak.force, fp1.rfd, fp1.plf, fp1.landing.rfd), ncol = 11), file = unilateral.file,
                  row.names = F, col.names = F, sep = ',', append = T)
      
      write.table(matrix(c(as.character(jump.date), athlete, jump.type, trial.number, bar.load, 'fp2', fp2.net.impulse,
                           fp2.peak.force, fp2.rfd, fp2.plf, fp2.landing.rfd), ncol = 11), file = unilateral.file,
                  row.names = F, col.names = F, sep = ',', append = T)
      
      write.table(matrix(c(as.character(jump.date), athlete, jump.type, trial.number, bar.load, 'asymmetry', net.impulse.asymmetry,
                           peak.force.asymmetry, rfd.asymmetry, plf.asymmetry, landing.rfd.asymmetry), ncol = 11),
                  file = unilateral.file, row.names = F, col.names = F, sep = ',', append = T)
      
      #This filename is only created and saved to if you are analyzing a CMJ
      if(jump.type == 'CMJ'){
        phasic.filename <- paste(jump.date, 'Phasic Analysis.csv', sep = ' ')
        phasic.file <- file.path(directory, phasic.filename)
        
        #Checks if the file exists
        #If not, creates it and adds a header to the file
        if(!file.exists(phasic.file)){
          file.create(phasic.file)
          
          #Adds header to the phasic analysis file
          write.table(matrix(c('jump.date','athlete','trial','bar.load','unweight.duration','braking.duration','propulsion.duration',
                               'total.duration','braking.rfd','ft.ct.ratio'), ncol = 10), file = phasic.file,
                      row.names = F, col.names = F, sep = ',', append = F)
        }
        
        #Writes data to the phasic analysis file
        write.table(matrix(c(as.character(jump.date), athlete, trial.number, bar.load, unweight.duration / 1000, braking.duration,
                             propulsion.duration, total.duration / 1000, braking.rfd, ft.ct.ratio), ncol = 10),
                    file = phasic.file, row.names = F, col.names = F, sep = ',', append = T)
      }
    })

  }
)