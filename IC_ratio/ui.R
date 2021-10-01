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
    titlePanel("Properties of asymmetric filters"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput('kernel', 'Select kernel',
                        c("Henderson", "Uniform", "Biweight", "Trapezoidal", "Triweight", "Tricube", "Gaussian", "Triangular", "Parabolic"),
                        multiple=TRUE, selectize=TRUE,
                        selected = "Henderson"),
            selectInput(inputId = "endpoints",
                        label = "Select endpoint method",
                        choices = c("LC", "QL", "CQ", "DAF"),
                        multiple=TRUE, selectize=TRUE,
                        selected = "LC"
            ),
            numericInput("horizon", "horizon", value = 6, step=1,  min = 3, max = 12),
            actionButton("update",
                         label = "Update"),
            sliderInput("ic_r", label = h3("IC_ratio values"), min = 0.1, 
                        max = 30, value = c(0.1,30)),
            uiOutput("q0"),
            width = 3.
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Coefficients",
                                 plotlyOutput("plot_coef", height = "80vh")))
    )
)))
