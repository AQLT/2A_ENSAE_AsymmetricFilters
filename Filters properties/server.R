#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# devtools::install_github("AQLT/rjdfilters")
library(rjdfilters)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    filters_properties <- reactive({
        filterproperties(horizon = input$horizon,
                         degree = input$degree,
                         endpoints = input$endpoints,
                         kernel = input$kernel,
                         ic = input$ic)
    })
    
    # Plot
    output$coefs <- renderPlot({ 
        plot_coef(filters_properties(),
                  q = as.numeric(input$q),
                  legend = TRUE)
    })
    output$gain <- renderPlot({ 
        plot_gain(filters_properties(),
                  q = as.numeric(input$q))
    })
    output$phase <- renderPlot({ 
        plot_phase(filters_properties(),
                  q = as.numeric(input$q))
    })
    
    
    # UI
    output$q0 <- renderUI({
        choices <- seq(0, input$horizon)
        default_value <- input$horizon
        checkboxGroupInput(inputId = "q",
                           label = "Select q",
                           choices = choices,
                           selected = default_value,
                           inline = TRUE)
    })
    

})
