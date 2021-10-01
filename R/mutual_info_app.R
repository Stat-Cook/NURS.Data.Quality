#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
# source("../R/mutual_info.R")

# Define UI for application that draws a histogram
mutual.info.app <- function(data){
  #'
  #' @export
  ui <- fluidPage(
  
      # Application title
      titlePanel("Mutual Information"),
  
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
          sidebarPanel(
              sliderInput("highlight",
                          "Minimum mutual information",
                          min = 0,
                          max = 2,
                          value = 0.2, step=0.01)
          ),
  
          # Show a plot of the generated distribution
          mainPanel(
              DT::dataTableOutput("contents")
          )
      )
  )
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
      mi <- mutual_info(data)
      rounded <- round(mi, 2)
    
      output$contents <- DT::renderDataTable({

          datatable(rounded, options = list(dom = 't')) %>% 
              formatStyle(
                  colnames(rounded), 
                  backgroundColor=styleInterval(input$highlight, c("white", "rgb(255, 160, 160)"))
              )
      })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}
