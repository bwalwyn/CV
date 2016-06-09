# server.R
library(neuralnet)
set.seed(123)
traininginput <-  as.data.frame(runif(50, min=0, max=100))
trainingoutput <- sqrt(traininginput)

#Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")

shinyServer(
  function(input, output) {
    output$plotNNs <- renderPlot({      
      plot(neuralnet(Output~Input,trainingdata, stepmax=100000, 
                rep=10,hidden=input$neurons, threshold=0.01),rep="best")
    })
  }
)
