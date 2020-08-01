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
source("shinyFunctions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    r <- reactiveValues(allfilters_properties = NULL,
                        selectedFilter = filterproperties(3),
                        xlim = c(0,2*pi/12))
    # eventReactive({
    #     c(input$horizon, input$degree, input$ic)
    # }, {
    #     r$allfilters_properties <- all_filters(horizon = input$horizon,
    #                                         degree = input$degree,
    #                                         ic = input$ic)
    #     r$selectedFilter <- r$allfilters_properties[[input$endpoints]][[input$kernel]]
    # })
    filters_properties <- reactive({
        if(is.null(r$allfilters_properties)){
            r$selectedFilter
        }else{
            r$selectedFilter <- r$allfilters_properties[[input$endpoints]][[input$kernel]]
            r$selectedFilter  
        }
    })
    observeEvent({
        input$horizon
        input$degree
        input$ic
    },{
        r$allfilters_properties <- all_filters(horizon = input$horizon,
                                               degree = input$degree,
                                               ic = input$ic)
        r$selectedFilter <- r$allfilters_properties[[input$endpoints]][[input$kernel]]
    })
    
    observeEvent({
        input$xlim
    },{
        r$xlim <- c(eval(parse(text=input$xllim)),
                    eval(parse(text=input$xulim)))
    })

    
    # Plot
    output$plotFilterProperties <- renderPlot({ 
        layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
        par(mai = c(0.2, 0.3, 0.2, 0))
        plot_coef(filters_properties(),
                  q = as.numeric(input$q),
                  legend = TRUE, main = "Coefficients")
        par(mai = c(0.3, 0.5, 0.2, 0))
        plot_gain(filters_properties(),
                  q = as.numeric(input$q), main = "Gain",
                  xlim = r$xlim)
        par(mai = c(0.3, 0.5, 0.2, 0))
        plot_phase(filters_properties(),
                   q = as.numeric(input$q), main = "Phase",
                   xlim = r$xlim)
    })
    output$tableFilterProperties <- renderDataTable(
        diagnostic_table(r$allfilters_properties,
                         horizon = input$horizon),
        escape = c(-1)
    )

    output$coefplot_endpoints <- renderPlot({ 
        coef_plot_comp(filters_properties = r$allfilters_properties,
                       endpoints = input$endpoints,
                       kernel = input$kernel,
                       q = as.numeric(input$q), 
                       fixed = "endpoints")
    })
    output$phaseplot_endpoints <- renderPlot({ 
        gain_phase_plot_comp(filters_properties = r$allfilters_properties,
                             endpoints = input$endpoints,
                             kernel = input$kernel,
                             q = as.numeric(input$q), 
                             which = "phase",
                             fixed = "endpoints",
                             xlim = r$xlim)
    })
    output$gainplot_endpoints <- renderPlot({ 
        gain_phase_plot_comp(filters_properties = r$allfilters_properties,
                             endpoints = input$endpoints,
                             kernel = input$kernel,
                             q = as.numeric(input$q), 
                             which = "gain",
                             fixed = "endpoints",
                             xlim = r$xlim)
    })
    
    output$coefplot_kernel <- renderPlot({ 
        coef_plot_comp(filters_properties = r$allfilters_properties,
                       endpoints = input$endpoints,
                       kernel = input$kernel,
                       q = as.numeric(input$q), 
                       fixed = "kernel")
    })
    output$phaseplot_kernel <- renderPlot({ 
        gain_phase_plot_comp(filters_properties = r$allfilters_properties,
                             endpoints = input$endpoints,
                             kernel = input$kernel,
                             q = as.numeric(input$q), 
                             which = "phase",
                             fixed = "kernel",
                             xlim = r$xlim)
    })
    output$gainplot_kernel <- renderPlot({ 
        gain_phase_plot_comp(filters_properties = r$allfilters_properties,
                             endpoints = input$endpoints,
                             kernel = input$kernel,
                             q = as.numeric(input$q), 
                             which = "gain",
                             fixed = "kernel",
                             xlim = r$xlim)
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
