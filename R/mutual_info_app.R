library(shiny)
library(DT)

# Define UI for application that draws a histogram
mutual.info.app <- function(data){
  #' Interactive app for tracking mutual info.
  #' 
  #' @param data The data set to calculate mutual info from
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
