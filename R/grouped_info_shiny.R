library(shiny)
library(tidyverse)
library(glue)

grouped.info <- function(data, group_var, Col1, Col2, scale="Linear"){
  #' Produces a plot of mutual information between two columns as a function 
  #' of a third variable, i.e. x = group_var, y = mutinfo(Col1, Col2).
  #' 
  #' @param data Data frame to be analyzed
  #' @param group_var The variable to group data by.  Acts as the x-axis in the plot.
  #' @param Col1 A column of data to use for mutual information.
  #' @param Col2 A column of data to use for mutual information
  #' @param scale Scale type for y-axis - choice of 'Linear' or 'Log' 
  #' 
  #' @return A ggplot object.
  #' 
  #' @export
  nbins <- sqrt(nrow(data))
  disc.df <- discrete.data(data, nbins=50)
  grouped <- disc.df %>% group_by(data[group_var])
  
  grouped.info.plot(grouped, Col1, Col2, scale)
}

grouped.info.plot <- function(grouped, Col1, Col2, scale="Linear"){
  #' @export
  info <- grouped %>% group_map(column_info_func(Col1, Col2)) %>%
    do.call(rbind, .)%>% round(5)
  
  info <- cbind(info[,1], info[,3] / info[,2], info[,3] / info[,5])
  rownames(info) <- info[,1]
  
  colnames(info)[2:3] <- c(
    glue("{Col2} on {Col1}"),
    glue("{Col1} on {Col2}")
  )
  p <- info[,2:3] %>% reshape2::melt() %>% 
    ggplot(aes(x=Var1, y=value, color=Var2)) + 
    xlab("") + ylab("Information") + 
    geom_line(size=2) + ylim(0, 1) +labs(color="")
  
  if (scale == "Log"){
    p <- p + scale_y_continuous(trans = "log10", limits= c(1e-2, 1))
  }
  
  p
}

grouped.info.app <- function(data, group_var, discrete_bins=50){
  #' Interactive app for tracking mutual info.
  #'
  #' @param data The data set to calculate mutual info from
  #' @param group_var The variable to group by and use as the x-axis in plots
  #' 
  #' @return Starts a shiny App where variable pairs can be selected and the 
  #' change in mutual information shown.
  #' 
  #' @example 
  #' grouped.info.plot(ChickWeight, Diet)
  #' 
  #' @importFrom magrittr "%>%"
  #' 
  #' @export

  data.name <- substitute(data)
  nbins <- sqrt(nrow(data))

  disc.df <- data %>% replace_na() %>% discrete.data(nbins=discrete_bins)
  active <- rstudioapi::getSourceEditorContext()$path
  
  disc.df$grouper <-  data[group_var]
  grouped <- disc.df %>% group_by(grouper)
  col.options <- data %>% select(-{{ group_var }}) %>% colnames()
  
  ui <- fluidPage(
    
    titlePanel("Mutual Information plots"),
    
    sidebarLayout(
      sidebarPanel(
        actionButton("do.output", "Send plot function to clipboard"),
        fluidRow(
          column(6, radioButtons("Col1", "Col1", col.options)),
          column(6, radioButtons("Col2", "Col2", col.options))
        ),
        radioButtons("scale", "Scale function", c("Linear", "Log"))
      ),
      
      mainPanel(
        plotOutput("output")
      )
    )
  )
  
  server <- function(input, output) {
    observeEvent(input$do.output, {
    
      text <- glue("{data.name} %>% grouped.info(
                      '{group_var}', '{input$Col1}', '{input$Col2}', '{input$scale}'
                   )")
      writeClipboard(text)
    })

        
    output$output <- renderPlot({
      Col1 <- input$Col1
      Col2 <- input$Col2  
      grouped.info.plot(grouped, Col1, Col2, input$scale)
    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}

# grouped.info.app(ChickWeight, "Diet")
# 
# 
# ChickWeight %>% grouped.info.function(
#   'Diet', 'Time', 'x1', 'Linear'
# )
