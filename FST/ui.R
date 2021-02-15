#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(plotly)
library(shinybusy)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    add_busy_spinner(spin = "fading-circle"),
    
    # Application title
    titlePanel("Properties of fst filters"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            fluidRow(
                column(4, numericInput("lags", "lags", value = 6, step=1,  min = 1, max = 30)),
                column(4, numericInput("leads", "leads", value = 0, step=1,  min = 1, max = 30)),
                column(4, numericInput("pdegree", "pdegree", value = 1, step=1, min = 0, max = 5))
            ),
            fluidRow(
                column(6, numericInput("smoothness.degree", "smoothness.degree", value = 3, step=1,  min = 1, max = 20)),
                column(6, textInput("timeliness.passband", label = "timeliness.passband", value = "pi/6"))
            ),
            switchInput(inputId = "timeliness.antiphase", value = TRUE,
                        label="timeliness.antiphase", size="normal",onStatus = "dreamrs"),
            numericInput("resolution", label = "Resolution", value = 100, min=50),
            actionButton("refresh", label = "Refresh"),
            selectInput(inputId = "color",
                        label = "Color",
                        choices = c("default", "fidelity.weight", "smoothness.weight", "timeliness.weight",
                                    "method"),
                        selected = "default"),
            fluidRow(
                column(6, switchInput(inputId = "lpp_filters", value = TRUE,
                                      label="Local polynomial filters", size="normal",onStatus = "dreamrs")),
                column(6, switchInput(inputId = "rkhs_filters", value = TRUE,
                                      label="RKHS filters", size="normal",onStatus = "dreamrs"))
                # column(6, switchInput(inputId = "normalizedAxis", value = FALSE,
                #                       label="Normalized axis", size="normal",onStatus = "dreamrs"))
            ),
            switchInput(inputId = "tlpp_filters", value = FALSE,
                        label="Timeliness local polynomial filters", size="normal",onStatus = "dreamrs")

            
            # sliderInput(inputId = "xlim",
            #             label = "x limits * pi for Phase/Gain functions",
            #             min = 0,
            #             max = 1,
            #             value = c(0,1/24)),
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel(HTML("Grun-Rehomme <em>et al.</em>"),
                                 plotlyOutput("plot_guguemos", height = "80vh")),
                        tabPanel("Wildi and McElroy",
                                 plotlyOutput("plot_wildi", height = "80vh")),
                        tabPanel("Both",
                                 plotlyOutput("plot_2graph", height = "80vh")))
            
        #     tabsetPanel(type = "tabs",
        #                 tabPanel("General results",
        #                          plotOutput("plotlpp_properties", height = "80vh")),
        #                 tabPanel("Fixed endpoints",
        #                          tabsetPanel(type = "tabs",
        #                            tabPanel("Coefficients",
        #                                     plotOutput("coefplot_endpoints", height = "75vh")),
        #                            tabPanel("Gain",
        #                                     plotOutput("gainplot_endpoints", height = "75vh")),
        #                            tabPanel("Phase",
        #                                     plotOutput("phaseplot_endpoints", height = "75vh"))
        #                          )),
        #                 tabPanel("Fixed kernel",
        #                          tabsetPanel(type = "tabs",
        #                                      tabPanel("Coefficients",
        #                                               plotOutput("coefplot_kernel", height = "75vh")),
        #                                      tabPanel("Gain",
        #                                               plotOutput("gainplot_kernel", height = "75vh")),
        #                                      tabPanel("Phase",
        #                                               plotOutput("phaseplot_kernel", height = "75vh"))
        #                          )),
        #                 tabPanel("More",
        #                          tabsetPanel(type = "tabs",
        #                                      tabPanel("Kernels",
        #                                               includeHTML('kernels_def.html'))
        #                          ))
        #                 )
        #                 
        )
    )
))
