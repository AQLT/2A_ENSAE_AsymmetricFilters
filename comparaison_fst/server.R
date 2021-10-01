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
library(dplyr)
# rkhs <- readRDS("FST/rkhs.RDS")
#tlpp <- readRDS("FST/timeliness_lpp.RDS")


# devtools::install_github("AQLT/rjdfilters")
# source("shinyFunctions.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  r <- reactiveValues(data = readRDS(sprintf("comparison_fst_weights_h%s%.1f.RDS", 6, 3.5)))
  
  observeEvent({
    input$horizon
  },{
    r$data <- readRDS(sprintf("comparison_fst_weights_h%s.RDS", input$horizon))
  })
    
    output$plot <- renderPlotly({
      data_tri <- r$data %>% 
        filter(q %in% as.numeric(input$q),
               method %in% input$method,
               degree %in% as.numeric(input$degree))
      if(input$degrees){
        c("frf", "gain", "phase", "LC", "QL", "CQ", "DAF")
        data_tri <- data_tri %>% 
          filter(((method == "gain") & (degree >= 0)) |
                 ((method == "frf") & (degree >= 0)) |
                 ((method == "phase") & (degree >= 0)) |
                 ((method == "LC") & (degree >= 0)) |
                 ((method == "QL") & (degree >= 1)) |
                 ((method == "CQ") & (degree >= 2)) |
                 ((method == "DAF") & (degree >= 3))
                 )
      }
      data_tri$method <- recode(data_tri$method,
                                gain = "RKHS_TimelinessGain",
                                frf = "RKHS_FrequencyResponse",
                                phase = "RKHS_Timeliness")
      data_tri$label <- sprintf("%s - degree = %i - q = %i", data_tri$method,
                                data_tri$degree,
                                data_tri$q)
      tmp1 <<- data_tri
      if(input$intersection){
        # data_tri <- data_tri %>%
        #   group_by(smoothness.weight, fidelity.weight,
        #            timeliness.weight) %>%
        #   filter( n() > 1 ) %>%
        #   ungroup()
        # tmp <- data_tri[,c("smoothness.weight", "fidelity.weight",
        #                    "timeliness.weight")]
        # data_tri <- data_tri[ duplicated(tmp) |
        #                         duplicated(tmp, fromLast = TRUE), ]
        intersect_data <- Reduce(intersect, split(data_tri[,c("smoothness.weight", "fidelity.weight",
                                            "timeliness.weight")],
                                data_tri$label))
        data_tri <- data_tri[(data_tri$smoothness.weight %in% intersect_data$smoothness.weight) &
                       (data_tri$fidelity.weight %in% intersect_data$fidelity.weight) &
                       (data_tri$timeliness.weight %in% intersect_data$timeliness.weight),]
        
      }
      
      plot_ly(data_tri,
              x=~`smoothness.weight`,
              y=~`fidelity.weight`,
              z=~`timeliness.weight`,
              type="scatter3d", mode = "markers",
              color = ~as.factor(label)
      ) %>% 
        layout(
          scene = list(
            xaxis = list(range = c(0, 1), dtick = 0.2),
            yaxis = list(range = c(0, 1), dtick = 0.2),
            zaxis = list(range = c(0, 1), dtick = 0.2)
          )
        )
    })
    # UI
    output$q0 <- renderUI({
      choices <- seq(0, as.numeric(input$horizon)-1)
      default_value <- 0
      checkboxGroupInput(inputId = "q",
                         label = "Select q",
                         choices = choices,
                         selected = default_value,
                         inline = TRUE)
    })
    
})
