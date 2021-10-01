library(tidyverse)
library(shiny)

summary.vector <- function(values, ...){
  #' @export
  args <- c(...)
  values <- unlist(values)
  len <- length(values)
  tab <- table(values)
  tablen <- length(tab)
  tabmax <- max(tab)
  c(
    unlist(args),
    `Missing ratio` = mean(values %in% MISSING_TYPES),
    `Classes ratio` = tablen / len,
    `Most common class` = tabmax / len
  )
}



data.quality.app <- function(data, group_var){
  #' @export
  grouped <- data %>% group_by({{ group_var }})
  col.options <- data %>% select(-{{ group_var }}) %>% colnames()
  
  ui <- fluidPage(
    
    titlePanel("Data Quality plots"),
    
    sidebarLayout(
      sidebarPanel(
          radioButtons("Col1", "Col1", col.options)
      ),
      mainPanel(
        plotOutput("output")
      )
    )
  )
  
  server <- function(input, output) {
    
    output$output <- renderPlot({
      
      Col <- input$Col1

      dq.data <- grouped %>% 
        select(Col) %>%  group_map(summary.vector) %>% 
        do.call(rbind, .)
      
      rownames(dq.data) <- dq.data[,1]
      
      dq.data[,-1] %>% reshape2::melt() %>% 
        ggplot(aes(x=Var1, y=value, color=Var2)) + geom_point(size=3) + 
        geom_line(linetype="dashed", size=1.0) + 
        ylim(0, 1) + xlab("") + ylab("Ratio")
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}


