#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
        plot_coef(filters_properties()$filters.coef,
                  q = input$q)
    })
    output$gain <- renderPlot({ 
        plot_gain(filters_properties()$filters.gain,
                  q = input$q)
    })
    output$phase <- renderPlot({ 
        plot_phase(filters_properties()$asymmetricfilter.phase,
                  q = input$q)
    })
    
    # UI
    output$q0 <- renderUI({
        default_value <- input$horizon
        sliderInput("q", "q",
                    min = 0, max = input$horizon, step = 1,
                    value = default_value)
    })

})
