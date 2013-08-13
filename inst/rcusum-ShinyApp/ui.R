library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("rcusum - An app for conducting interactive cusum analysis"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    tags$h4("Data File Upload"),
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 'text/comma-separated-values,text/plain')),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 'Comma'),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 'Double Quote'),
    tags$hr(),
    tags$h4("General Cusum Settings"),
    uiOutput("failure_indicator_chooser"),
    uiOutput("by_chooser"),
    tags$br(),
    sliderInput("alpha", 
                "Alpha-Level: Type I error rate", 
                min = 0.001,
                max = 0.2, 
                value = 0.01,
                ticks=TRUE,
                step=0.002
                ),
    sliderInput("beta", 
                "Beta-Level: Type II error rate", 
                min = 0.001,
                max = 0.2, 
                value = 0.01,
                ticks=TRUE,
                step=0.002
    ),
    br(),
    HTML("Please report bugs, errors and comments/suggestions to <a href='mailto:meyer.alexander+github@gmail.com'>me</a>.")
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Data", tableOutput('contents')),
      tabPanel("Unadjusted Cusum Analysis",
               sliderInput("p0", 
                           "p0: The acceptable failure rate", 
                           min = 0.01,
                           max = 0.30, 
                           value = 0.1,
                           ticks=TRUE,
                           step=0.01
               ),
               sliderInput("p1", 
                           "p1: The unacceptable failure rate", 
                           min = 0.01,
                           max = 0.30, 
                           value = 0.2,
                           ticks=TRUE,
                           step=0.01
               ),
               tags$hr(),
               plotOutput("unadjusted")
               ),
      tabPanel("Risk-Adjusted Cusum Analysis",
               uiOutput("p0_variable_chooser"),
               sliderInput("OR", 
                           "OR: The unacceptable realtive increase of failure compared to estimated risk", 
                           min = 1.00,
                           max = 3.00, 
                           value = 1.5,
                           ticks=TRUE,
                           step=0.01
               ),
               plotOutput("risk_adjusted")
               ),
      tabPanel("Help", tags$a(href="https://github.com/meyera/rcusum", "Go to the package development website for help"))
    )
  )
))