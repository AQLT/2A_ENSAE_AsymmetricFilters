#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    titlePanel("Properties of asymmetric filters"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "kernel",
                        label = "Select kernel",
                        choices = c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),
                        selected = "Henderson"
            ),
            selectInput(inputId = "endpoints",
                        label = "Select endpoint method",
                        choices = c("LC", "QL", "CQ", "CC", "DAF"),
                        selected = "LC"
            ),
            fluidRow(
                column(4,numericInput("degree", "degree", value = 3, step=1,  min = 1, max = 7)),
                column(4,numericInput("horizon", "horizon", value = 3, step=1,  min = 1, max = 20)),
                column(4,numericInput("ic", "ic", value = 4.5, 
                                      step=0.1, 
                                      min = 0, max = 20))
            ),
            uiOutput("q0"),
            width = 3.
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("General results",
                                 plotOutput("plotFilterProperties", height = "80vh")),
                        tabPanel("Fixed endpoints",
                                 tabsetPanel(type = "tabs",
                                   tabPanel("Coefficients",
                                            plotOutput("coefplot_endpoints", height = "75vh")),
                                   tabPanel("Gain",
                                            plotOutput("gainplot_endpoints", height = "75vh")),
                                   tabPanel("Phase",
                                            plotOutput("phaseplot_endpoints", height = "75vh"))
                                 )),
                        tabPanel("Fixed kernel",
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Coefficients",
                                                      plotOutput("coefplot_kernel", height = "75vh")),
                                             tabPanel("Gain",
                                                      plotOutput("gainplot_kernel", height = "75vh")),
                                             tabPanel("Phase",
                                                      plotOutput("phaseplot_kernel", height = "75vh"))
                                 )),
                        tabPanel("More",
                                 tabsetPanel(type = "tabs",
                                             tabPanel("Kernels",
                                                      includeHTML('kernels_def.html'))
                                 ))
                        )
                        
        )
    )
))