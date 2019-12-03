# Thank you Cheng
# https://github.com/rstudio/shiny-examples/tree/master/087-crandash

# libraries ------------
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
# library(ggiraph) # next implementation

source("o_functions/tidy_data.R")

# UI --------------------
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
            menuItem("plots", tabName = "plots", icon = icon("chart-bar"), startExpanded = FALSE,
                     menuSubItem("Time Sleeping", tabName = "time_sleeping"),
                     menuSubItem("Get-up and Bed Time", tabName = "get_up"),
                     menuSubItem("Time in Workplace", tabName = "work_time"),
                     menuSubItem("Time Commuting", tabName = "commuting_time"),
                     menuSubItem("Time Working at Home", tabName = "home_time"),
                     menuSubItem("Processing Time", tabName = "processing_time"),
                     menuSubItem("Time doing Tai Chi", tabName = "tai_chi"),
                     menuSubItem("Time Walking", tabName = "walking"),
                     menuSubItem("Time Swimming", tabName = "swimming")
                     ),
            menuItem("tables", tabName = "tables", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            # plots ------------
            tabItem("tables",
                    HTML("<spam style=color:red;>I am working on this too</spam>")
            ),
            tabItem("time_sleeping", 
                      plotOutput("sleep")
                     ),
            tabItem("get_up",
                     plotOutput("getup")
                     ),
            tabItem("work_time",
                     plotOutput("work_time")
                     ),
            tabItem("commuting_time",
                     plotOutput("commuting_time")
                     ),
            tabItem("home_time",
                     plotOutput("home_time")
            ),
            tabItem("processing_time",
                     plotOutput("processing_time")
            ),
            tabItem("tai_chi",
                     plotOutput("tai_chi")
            ),
            tabItem("walking",
                     plotOutput("walking")
            ),
            tabItem("swimming",
                     plotOutput("swimming")
            )
        )
    )
)


# SERVER ------------
server <- function(input, output) {
    
    source("o_functions/plots_racoon.R")
  
    # reactive values -------------
    dat_rc <- reactiveVal(NULL) # imported data
    dat_fr <- reactiveVal(NULL)
    
    # get data ----
    observeEvent(
        eventExpr = {
            input$data_in
        },
        handlerExpr = {
            dat_long <- suppressWarnings(clean_data(dat = readxl::read_excel(input$data_in$datapath)))
            dat_rc(dat_long)
        }
    )
    
dat_fr <- reactive({
          filter(req(dat_rc()), date_ins >= input$date_r[1],  date_ins <= input$date_r[2])
      }
    )
    
    # plot sleep ----
    output$sleep<- renderPlot({
      sleeptime_plot(dat = req(dat_fr()), dwm = input$dwm)
    })

    # plot get_up ----
    output$getup<- renderPlot({
      getup_plot(dat = req(dat_fr()), dwm = input$dwm)
    })

    # create duration plots dinamically
    lapply(dur_act, function(x){
      output[[x]]<- renderPlot({
        time_plot(dat = req(dat_fr()), measure = x, dwm = input$dwm)
    })})

}

# Run the application 
shinyApp(ui = ui, server = server)
