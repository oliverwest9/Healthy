
library(shiny)


speciallity <- c('Lung Asthma', 'Lung COPD','Vaccination flu','Mental Health stress',
                 'Brain Dementia',' Muscular medium','Arthritis Osteo','Paeds General')

schemes <- c('initiative1','initiative2','initiative3','initiative4','initiative5',
             'initiative6','initiative7')
library(ggplot2)

# create a dataset for the dummy graph 
specie=c(rep("a" , 2) , rep("b" , 2))
condition=rep(c("Current", "With Social Prescription") )
value=c(790, 1000, 1100, 830)
data=data.frame(specie,condition,value)



# Define User Interface for the application

ui <- fluidPage(
  
  title = 'Social Prescription Impact',
  
  # format the grey side bar
  sidebarLayout(
    sidebarPanel(
      
      fileInput(
        "file1",
        "Choose CSV File",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
      ),
      
 
      selectizeInput(
        'e1', 'Select Specialities', choices = speciallity, multiple = TRUE
      ),
  
      selectizeInput(
        'e2', 'Select Initiatives', choices = schemes, multiple = TRUE,
        options = list(maxItems = 20)
      ),
      
      sliderInput("i", "Initial Appointments Handled by Nurses",
                  min = 0, max = 100,
                  value = 20, step = 10,
                  post  = "%",
                  animate = TRUE),
      
      sliderInput("fu", "Follow ups Handled by Nurses",
                  min = 0, max = 100,
                  value = 20, step = 10,
                  post  = "%",
                  animate = TRUE),
      sliderInput("year", "Years Projected", 0, 5, value = c(0, 1)),
      
      numericInput('growth', 'Growth Rate', 0.3,
                   min = 0, max = 1)
      
      
    ),
    # format the main area
    mainPanel(
      
      # Creates the tabs and places the plots in under the respective tabs
      tabsetPanel(type = "tabs",
                  tabPanel("Overall", plotOutput("plot1"), plotOutput("plot2") ),
                  tabPanel("Cost", plotOutput("plot3")),
                  tabPanel("Appointment Numbers", plotOutput("plot4")),
                  tabPanel("Sustainability", tableOutput("table"))
     
       )
  
  )
)
)





# Define server logic 
server <- function(input, output) {
  # Creates the plots and assigns them to the correct plot name used in the UI
  output$plot1 <- renderPlot({
    ggplot(data, aes(fill=condition, y=value, x=specie)) + 
      geom_bar(position="dodge", stat="identity") + labs(x = " ") +title("Cost")
  })
  
  output$plot2 <- renderPlot({
    hist(Nurse_Follow,
    main = 'Follow Up Appointment Numbers',
      xlab = '', ylab='Follow up Appointments')
  })
  
  output$plot3 <- renderPlot({
    ggplot(data, aes(fill=condition, y=value, x=specie)) + 
      geom_bar(position="dodge", stat="identity") + labs(x = " ") +title("Cost")
  })
  
  output$plot4 <- renderPlot({
    hist(Nurse_Follow,
         main = 'Follow Up Appointment Numbers',
         xlab = '', ylab='Follow up Appointments')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

