#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
# devtools::install_github("AQLT/rjdfilters")
source("shinyFunctions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    r <- reactiveValues(data = all_fst_res())
    observeEvent({
        input$refresh
    },{
        r$data <- tryCatch(all_fst_res(lags = input$lags, leads = input$leads, pdegree = input$pdegree,
                              smoothness.degree=input$smoothness.degree,
                              timeliness.passband = eval(parse(text = input$timeliness.passband)),
                              timeliness.antiphase = input$timeliness.antiphase,
                              resolution = input$resolution),
                           error = function(e){
                               NULL
                           })
    })
  
    output$plot_guguemos <- renderPlotly({
        plotly_guguemos(r$data, color = input$color, lpp_filters = input$lpp_filters)
    })
    output$plot_wildi <- renderPlotly({
        plotly_wildi(r$data, color = input$color, lpp_filters = input$lpp_filters)
    })
    
})
