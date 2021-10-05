library(shiny)
library(tidyverse)
library(glue)

grouped.info.app <- function(data, group_var){
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
  #' @export

  nbins <- sqrt(nrow(data))
  disc.df <- discrete.data(data, nbins=50)
  
  grouped <- disc.df %>% group_by(data[group_var])
  col.options <- data %>% select(-{{ group_var }}) %>% colnames()
  
  ui <- fluidPage(
    
    titlePanel("Mutual Information plots"),
    
    sidebarLayout(
      sidebarPanel(
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


        
    output$output <- renderPlot({
      
      Col1 <- input$Col1
      Col2 <- input$Col2  
      
      info <- grouped %>% group_map(column_info_func(Col1, Col2)) %>%
        do.call(rbind, .)%>% round(5)
      # 
      # ref1 <- grouped %>% group_map(column_info_func(input$Col1, input$Col1)) %>% 
      #   do.call(rbind, .)
      # 
      # ref2 <- grouped %>% group_map(column_info_func(input$Col2, input$Col2)) %>% 
      #   do.call(rbind, .)
      
      # info <- cbind(info, info[,2] / ref1[,2], info[,2] / ref2[,2] )
      # rownames(info) <- info[,1]
      # colnames(info)[c(3,4)] <- c(
      #   glue("{Col2} on {Col1}"),
      #   glue("{Col1} on {Col2}")
      # )
      info <- cbind(info[,1], info[,3] / info[,2], info[,3] / info[,5])
      rownames(info) <- info[,1]
      
      colnames(info)[2:3] <- c(
        glue("{Col2} on {Col1}"),
        glue("{Col1} on {Col2}")
      )
      p <- info[,2:3] %>% reshape2::melt() %>% 
        ggplot(aes(x=Var1, y=value, color=Var2)) + 
        xlab("") + ylab("Information") + 
        geom_line(size=2) + ylim(0, 1)
      
      if (input$scale == "Log"){
        p <- p + scale_y_continuous(trans = "log10", limits= c(1e-2, 1))
      }
      
      p

    })
  }
  
  # Run the application 
  shinyApp(ui = ui, server = server)
}


# grouped.info.plot(ChickWeight, Diet)
# 
# ChickWeight$x1 <- rnorm(nrow(ChickWeight))
# ChickWeight$x2 <- as.numeric(ChickWeight$x1 + rnorm(nrow(ChickWeight), 0, 0.05) > 0)
# 
# grouped <- ChickWeight %>% group_by(Diet)#  %>%  grouped.info.plot()
# colnames(ChickWeight)
# Col1 <- "Time"
# Col2 <- "weight"
# info <- grouped %>% group_map(column_info_func(Col1, Col2)) %>% 
#   do.call(rbind, .)
# 
# ref1 <- grouped %>% group_map(column_info_func(Col1, Col1)) %>% 
#   do.call(rbind, .)
# 
# ref2 <- grouped %>% group_map(column_info_func(Col2, Col2)) %>% 
#   do.call(rbind, .)
# info <- cbind(info, info[,2] / ref1[,2], info[,2] / ref2[,2] )
# rownames(info) <- info[,1]
# colnames(info)[c(3,4)] <- c("A on B", "B on A")
# reshape2::melt(info[,c(3,4)]) %>% ggplot(aes(x=Var1, y=value, color=Var2)) + 
#   geom_line() + ylim(0, 1)


