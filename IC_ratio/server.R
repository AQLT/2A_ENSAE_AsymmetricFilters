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
source("functions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    r <- reactiveValues(data = extract_all(),
                        ylim = c(0, 30))

    observeEvent({
        input$update
    },{
        r$data <- extract_all(kernel = input$kernel,
                              h = input$horizon,
                              method = input$endpoints)
    })
    
    observeEvent({
        input$ic_r
    },{
        r$ylim <- input$ic_r
    })
    
    # Plot
    output$plot_coef <- renderPlotly({
        data <- r$data
        if(!is.null(data)){
            data <- data[(data$y>= r$ylim[1])&(data$y<= r$ylim[2]),]
            data <- data[data$q %in% as.numeric(input$q),]
            data$id <- as.factor(data$id)
            colors = RColorBrewer::brewer.pal(length(levels(data$id)),"Paired")[1:length(levels(data$id))]
            plot_ly(data,
                    x = ~x,
                    y = ~y,
                    z = ~z,
                    type="scatter3d", mode="markers",
                    color = ~id,
                    colors = colors
            ) %>%
                layout(scene = list(xaxis = list(title = ''),
                                    yaxis = list(title = 'I/C ratio'),
                                    zaxis = list(title = 'Coefficient'))) 
        }else{
            NULL
        }
        
    })
    
    # UI
    output$q0 <- renderUI({
        choices <- seq(0, input$horizon)
        default_value <- 0#input$horizon
        checkboxGroupInput(inputId = "q",
                           label = "Select q",
                           choices = choices,
                           selected = default_value,
                           inline = TRUE)
    })
    
})
