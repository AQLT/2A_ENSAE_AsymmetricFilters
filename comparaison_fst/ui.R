#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinybusy)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    # Application title
    add_busy_spinner(spin = "fading-circle"),
    titlePanel("When does the FST filters outperforms other methods?"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput("horizon", label = h3("Select horizon"), 
                        choices = list("13-terms" = 6, "23-termes" = 11), 
                        selected = 6),
            uiOutput("q0"),
            checkboxGroupInput("method", label = h3("methods"),
                               choices = list("RKHS frf" = "frf", "RKHS gain" = "gain",
                                              "RKHS phase" = "phase",
                                              "LC" = "LC", "QL" = "QL",
                                              "CQ" = "CQ", "DAF" = "DAF"),
                               selected = c("LC")),
            checkboxInput("degrees", label = "Simplify degree", value = TRUE),
            checkboxGroupInput("degree", label = h3("Selected degrees"),
                               choices = list("0" = 0, "1" = 1,
                                              "2" = 2, "3" = 3),
                               selected = 0:3),
            checkboxInput("intersection", label = "Keep intersection", value = FALSE),
            width = 3.
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotlyOutput("plot", height = "80vh")
        )
    )
))
