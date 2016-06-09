#Note: Very much like html code

#---   USER INPUT
#1. Number of hidden layers
#2. Mumber of neurons i the hidden layer(s)
#3. Learning paramter
#4. Act. function
#5. Error function
#6. Number of iterations
#7. Number of repeats
#8. Input and output variables

#User input captured using widgets
#many types of widgets available for different types of input

shinyUI(fluidPage( #defined structure within fluidpage
                  titlePanel("Single Layer Neural Network"),
                  sidebarLayout(
                    sidebarPanel(  
                                fluidRow(column(3,
                                                fileInput("file", label = h3("File input")))),
                                fluidRow(column(3,
                                                numericInput("numin", label = h3("Number of input variables"), value = 1))),
                                fluidRow(column(3,
                                                numericInput("numout",label = h3("Number of output variables"), value = 1))),
                                fluidRow(column(3,
                                                radioButtons("transfer", label = h3("Transfer functions"),
                                                             choices = list("Step function (Sym)" = 1, "Step function" = 2,
                                                                            "Sigmoid function" = 3,"Tanh function" = 4), selected = 1))),
                                fluidRow(column(3,
                                                sliderInput("iteration", label = h3("Number of Iterations"),
                                                            min = 0, max = 100, value = 50))),
                                fluidRow(column(3,
                                                h3("Run NNs"), submitButton("Submit")))
                                ),
                    mainPanel(textOutput("text1"),
                              plotOutput("map"),
                              img(src="bigorb.png", height = 400, width = 400)
                              )
                    )
                  )
        )
