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
source("../R/mutual_info.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mutual Information"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
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
    output$contents <- DT::renderDataTable(data.frame())
    
    output$contents <- DT::renderDataTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$file1)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        df <- read.csv(input$file1$datapath)
        mi <- mutual_info(df)
        rounded <- round(mi, 2)
        
        datatable(rounded, options = list(dom = 't')) %>% 
            formatStyle(
                colnames(rounded), 
                backgroundColor=styleInterval(input$highlight, c("white", "rgb(255, 160, 160)"))
            )
    }, rownames=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

# datatable(pass.df) %>% 
#     formatStyle(colnames(pass.df), backgroundColor=styleInterval(c(1, 2), c("white", "red", "green")))
# 
# ?styleInterval
# styleInterval(0, "red")