#Shiny
install.packages('shiny')
library(shiny)

#ui.R -> ui design: controls appearance of app
#server.R -> Contains the instructions computer needs to build the app. 
#             Background. 
#Both files must ahve these names and be in the same folder.

#Shiny function included within shinyServer: e.g.
#renderPlot
#renderText

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$text1 <- renderText({ 
    paste("You have selected", input$transfer)
  })
  
})
#working directory must be where folder is included
setwd("C:/Users/Student/Downloads")
#open html
runApp('neuralnets')