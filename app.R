# Thank you Cheng
# https://github.com/rstudio/shiny-examples/tree/master/087-crandash

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggthemes)
source("helper.r")

ui <- dashboardPage(
    dashboardHeader(title = "time Racoon"),
    dashboardSidebar(
        fileInput("data_in", "Import data, Laura",
        ),
        radioGroupButtons(
            inputId = "dwm",
            label = "Unit",
            choices = c("day", 
                        "week", 
                        "month")
        ),
        
        dateRangeInput("date_r", start = "2019/10/26", label = "Date range", format = "mm-dd-yyyy"),
        
        actionBttn(
            inputId = "calc",
            label = "Calculate",
            style = "unite", 
            color = "danger",
            size = "sm"
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
                            tabPanel("Sleep", "First tab content"),
                            tabPanel("Work", "Tab content 2")
                    
            ))),
            tabItem("tables",
                    numericInput("maxrows", "Rows to show", 25),
                    verbatimTextOutput("rawtable"),
                    downloadButton("downloadCsv", "Download as CSV")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    dat_rc <- reactiveVal()
    

    observeEvent(
        eventExpr = {
            input$data_in
        },
        handlerExpr = {
            dat_long <- clean_data(dat = readxl::read_excel(input$data_in$datapath))
            dat_rc(dat_long)
        }
    )
    
    observeEvent(
        eventExpr = {
            input$calc
        },
        handlerExpr = {
            browser()
            dat_long <- dat_rc() %>% 
                filter(date_ins >= input$date_r[1],  date_ins <= input$date_r[2])
            
           #--------------------------------------------    
           if (input$dwm == "day") {
  
             dat_long %>% 
               mutate(sleep = sleep/60) %>% 
               ggplot() +
                     geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
                     scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
                     scale_color_brewer(palette = "Dark2") +
                     geom_label(aes(min(date_ins), y = 9.5), label = "target") +
                     labs(x = NULL, y = "hours", color = NULL) +
                     geom_line(aes(date_ins, sleep)) +
                     geom_point(size = 3, aes(date_ins, sleep, color = day_ins)) 

             dat_long %>% 
               mutate(sleep = sleep/60) %>% 
               ggplot() +
                   geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
                   scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
                   scale_color_brewer(palette = "Dark2") +
                   geom_label(aes(min(date_ins), y = 9.5), label = "target") +
                   labs(x = "week staring", y = "hours", color = NULL) +
                   geom_jitter(size = 3, aes(week_year_ins, sleep, color = day_ins), width = 0.2) 
                     
             dat_long %>% 
               mutate(sleep = sleep/60) %>% 
               ggplot() +
               geom_hline(yintercept = 9, lty = 4, lwd = 0.5) +
               scale_y_continuous(limits = c(0,12), breaks = 1:12, labels = 1:12) +
               scale_color_brewer(palette = "Dark2") +
               geom_label(aes(min(date_ins), y = 9.5), label = "target") +
               labs(x = "week staring", y = "hours", color = NULL) +
               geom_jitter(size = 3, aes(month_year_ins, sleep, color = day_ins), width = 1) +
               scale_x_date(date_breaks = "1 month")
               
               
           }
            
            
            
        }
        
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
