library(shiny)
library(markdown)
# Define UI ----
ui <-
  navbarPage(
    "Navigation",
    
    fileInput(
      "file1",
      "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
    ),
    
    
    navbarMenu(
      "Patient Access",
      
      tabPanel("Patient Wait Time"),
      tabPanel("Distance Travelled")
    ),
    
    tabPanel(
      "Social Prescription",
   
        hr(),
        verbatimTextOutput('out4'),
        selectInput('in4', 'Options', c(Choose = '', state.name),  multiple=TRUE, selectize =
                      TRUE)
      
    ),
    
    
      navbarMenu(
        "Sustianability",
        
        
        tabPanel("Costs"),
        tabPanel("Carbon Footprint")
      )
    )
    
    
    
    # Define server logic ----
    server <- function(input, output, session) {
      
    }
    
    
    # Run the app ----
    shinyApp(ui = ui, server = server
    )