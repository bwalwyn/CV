# ui.R

shinyUI(fluidPage(
  titlePanel("Create single-multiple neuron 
                            feed forward neural networks"),
  sidebarLayout(
      sidebarPanel(helpText("Your choice of neurons"),
          
                  numericInput("neurons", 
                              label = "Number of Neurons",
                              value = 2)),
      mainPanel(plotOutput("plotNNs"))
    )
)
)