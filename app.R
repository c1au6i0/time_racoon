# Thank you Cheng
# https://github.com/rstudio/shiny-examples/tree/master/087-crandash

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggthemes)
library(readxl)
library(tidyr)
library(data.table)
library(tidyverse)
library(lubridate)
library(janitor)
library(scales)



source("tidy_data.R")

ui <- dashboardPage(
    dashboardHeader(title = "time Racoon"),
    dashboardSidebar(
        fileInput("data_in", "Import data, Laura",
        ),
        
        dateRangeInput("date_r", start = "2019/10/26", label = "Date range", format = "mm-dd-yyyy"),
        
        radioGroupButtons(
          inputId = "dwm",
          label = "Unit",
          choices = c("day", 
                      "week", 
                      "month")
        ),
        
        sidebarMenu(
            menuItem("plots", tabName = "plots", icon = icon("chart-bar")),
            menuItem("tables", tabName = "tables", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("plots",
                    fluidRow(
                    tabBox(
                            title = "Plots",
                            width = 12,
                            # The id lets us use input$tabset1 on the server to find the current tab
                            id = "tabset1",
                            tabPanel("Sleep", 
                                      plotOutput("summary")
                                     
                                     ),
                            tabPanel("Work", "Tab content 2")
                    
            ))),
            tabItem("tables",
                    HTML("<spam style=color:red;>I am working on this</spam>")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    source("plots_racoon.R")
  
    # reactive values -------------
    dat_rc <- reactiveVal(NULL) # imported data
    
    # get data ----
    observeEvent(
        eventExpr = {
            input$data_in
        },
        handlerExpr = {
            dat_long <- suppressWarnings(clean_data(dat = readxl::read_excel(input$data_in$datapath)))
            dat_rc(dat_long)
            browser()
        }
    )
    
    # plot sleep ----
    output$summary <- renderPlot({
      sleeptime_plot(dat = req(dat_rc()), dwm = input$dwm, date_r = input$date_r)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
