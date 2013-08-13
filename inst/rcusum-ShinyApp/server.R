library(shiny)
library(rcusum)
require(gridExtra)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  datasetInput <- reactive({
    
    # input$file1 will be NULL initially. After the user selects and uploads a 
    # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
    # columns. The 'datapath' column will contain the local filenames where the 
    # data can be found.
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  })
  
  output$contents <- renderTable({
    datasetInput()
  })
  
  output$failure_indicator_chooser <- renderUI({
    ind <- names(datasetInput())
    if (is.null(ind) | length(ind) == 0) {
      return(tags$div(style="color:red", "No data uploaded!", tags$br(),"Please upload data."))
    } else {
      return(selectInput("failure_indicator", "Choose Failure Indicator Variable", ind))
    }
  })
  
  output$p0_variable_chooser <- renderUI({
    ind <- names(datasetInput())
    if (is.null(ind) | length(ind) == 0) {
      return(tags$div(style="color:red", "No data uploaded!", tags$br(),"Please upload data."))
    } else {
      return(selectInput("p0_variable", "Choose p0 variable", ind))
    }
  })
  
  output$by_chooser <- renderUI({
    ind <- names(datasetInput())
    if (is.null(ind) | length(ind) == 0) {
      ind = c()
    }
    
    return(selectInput("by_indicator", "Choose a stratification Variable", c("No stratification" ,ind)))
  })
  
  output$unadjusted <- renderPlot({
    df = datasetInput()
    failures = df[[input$failure_indicator]]
    by = NULL
    if (!is.null(input$by_indicator) & input$by_indicator != "No stratification") {
      by = as.factor(df[[input$by_indicator]])
    }
    
    alpha = as.numeric(input$alpha)
    beta = as.numeric(input$beta)
    p0 = as.numeric(input$p0)
    p1 = as.numeric(input$p1)
    
    p1 = cusum(failures, p0=p0, p1=p1, alpha=alpha, beta=beta, by=by, loglike_chart=TRUE)
    p2 = cusum.obs_minus_exp(failures,p0=p0, by=by)

    gridExtra::grid.arrange(p1, p2, nrow=1, as.table=TRUE)
  })
  
  output$risk_adjusted <- renderPlot({
    df = datasetInput()
    failures = df[[input$failure_indicator]]
    p0 = df[[input$p0_variable]]
    by = NULL
    if (!is.null(input$by_indicator) & input$by_indicator != "No stratification") {
      by = as.factor(df[[input$by_indicator]])
    }
    
    alpha = as.numeric(input$alpha)
    beta = as.numeric(input$beta)
    OR = as.numeric(input$OR)
    
    p1 = cusum.sprt(failures, p0, OR, alpha, beta, by)
    p2 = cusum.obs_minus_exp(failures, p0, by)
    
    gridExtra::grid.arrange(p1, p2, nrow=1, as.table=TRUE)
  })
})
