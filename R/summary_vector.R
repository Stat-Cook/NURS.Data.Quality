library(tidyverse)
library(shiny)

summary.vector <- function(values, ...){
  #' Calculates data quality metrics for a vector.  
  #' 
  #' Calculates a series of quality metrics where values approaching 1 
  #' indicate data quality issues.   The three metrics are:
  #' `Missing Ratio`: rate of missing observations.
  #' `Average Class Size`: The number of classes / the number of observations
  #' `Most common ratio`: Level of data represeted by the modal class.
  #'
  #' @param values 
  #' 
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
    `Average Class size` = tablen / len,
    `Most common ratio` = tabmax / len
  )
}

summary.vector.2 <- function(values, ...){
  #' @export
  args <- c(...)
  values <- unlist(values)
  len <- length(values)
  tab <- table(values)
  tablen <- length(tab)
  tabmax <- max(tab)
  c(
    unlist(args),
    "Most common" = names(which.max(tab))
  )
}

data.quality.plot <- function(grouped, col, scale="linear"){
  dq.data <- grouped %>% 
    select(col) %>%  group_map(summary.vector) %>% 
    do.call(rbind, .)
  
  rownames(dq.data) <- dq.data[,1]
  
  melted <- dq.data[,-1] %>% reshape2::melt() %>% 
    mutate(value = as.numeric(value))
  
  p <- melted %>% ggplot(aes(x=Var1, y=value, color=Var2)) + 
    geom_point(size=3) + geom_line(linetype="dashed", size=1.0) + 
    ylim(0, 1) + xlab("") + ylab("Ratio") 
  
  if (tolower(scale) == "linear"){
    return(p)
  }
  else{
    min_ <- min(melted$value[melted$value > 0])
    max_ <- max(melted$value)
    
    limits <- c(0.001, 1)
    p <- p + scale_y_continuous(trans="log10", limits=limits)
    
    return(p)
  }
}


data.quality.function <- function(data, group_var, Col, scale="Linear"){
  grouped <- data %>% group_by(data[group_var])
  data.quality.plot(grouped, Col, scale)
}


data.quality.app <- function(data, group_var){
  #' R shiny app for visualizing data quality patterns
  #' 
  #' Visualizes data quality metrics for  a data set as a function of 
  #' a grouping variable.  The grouping variable can be set as required though 
  #' using a temporal or geographical factor is advised.
  #' 
  #' @param data Data frame of observations to check for DQ.
  #' @param group_var Variable to visualized data quality as a function of.
  #' 
  #' @export
  #'
  data.name <- substitute(data)
  
  grouped <- data %>% group_by(data[group_var])
  col.options <- data %>% select(-{{ group_var }}) %>% colnames()
  
  ui <- fluidPage(
    
    titlePanel("Data Quality plots"),
    
    sidebarLayout(
      sidebarPanel(
          actionButton("do.output", "Send plot function to clipboard"),
          radioButtons("Col1", "Col1", col.options),
          radioButtons("scale", "Scale function", c("Linear", "Log"))
      ),
      mainPanel(
        plotOutput("output"),
        tableOutput("table")
      )
    )
  )
  
  server <- function(input, output) {
    
    observeEvent(input$do.output, {
      text <- glue("{data.name} %>% data.quality.function(
                      '{group_var}', '{input$Col1}', '{input$scale}'
                   )")
      writeClipboard(text)
    })
    
    output$output <- renderPlot({
      
      Col <- input$Col1

      dq.data <- grouped %>% 
        select(Col) %>%  group_map(summary.vector) %>% 
        do.call(rbind, .)
      
      rownames(dq.data) <- dq.data[,1]
      
      melted <- dq.data[,-1] %>% reshape2::melt() %>% 
        mutate(value = as.numeric(value))
      
      p <- melted %>% ggplot(aes(x=Var1, y=value, color=Var2)) + 
        geom_point(size=3) + geom_line(linetype="dashed", size=1.0) + 
        ylim(0, 1) + xlab("") + ylab("Ratio") 
      
      if (input$scale == "Linear"){
        return(p)
      }
      else{
        min_ <- min(melted$value[melted$value > 0])
        max_ <- max(melted$value)
        
        limits <- c(0.001, 1)
        p <- p + scale_y_continuous(trans="log10", limits=limits)
          
        return(p)
      }
    })
    
    output$table <- renderTable({
      Col <- input$Col1
      
      grouped %>% 
        select(Col) %>%  group_map(summary.vector.2) %>% 
        do.call(rbind, .)
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}