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
                min = 0,
                max = 0.5, 
                value = 0.01,
                step=0.01
                ),
    sliderInput("beta", 
                "Beta-Level: Type II error rate", 
                min = 0,
                max = 0.5, 
                value = 0.01,
                step=0.01
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Data", textOutput("strContents"),tableOutput('contents')),
      tabPanel("Unadjusted Cusum Analysis",
               plotOutput("unadjusted")
               ),
      tabPanel("Risk-Adjusted Cusum Analysis",
               plotOutput("risk_adjusted")
               ),
      tabPanel("Help", tags$a(href="https://github.com/meyera/rcusum", "Go to the package development website for help"))
    )
  )
))