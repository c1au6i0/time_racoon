# Thank you Cheng
# https://github.com/rstudio/shiny-examples/tree/master/087-crandash

library(shiny)
library(shinydashboard)
library(shinyWidgets)


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
        
        sidebarMenu(
            menuItem("plots", tabName = "Plots", icon = icon("chart-bar")),
            menuItem("tables", tabName = "Tables", icon = icon("table"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    fluidRow(
                        valueBoxOutput("rate"),
                        valueBoxOutput("count"),
                        valueBoxOutput("users")
                    ),
                    fluidRow(
                        box(
                            width = 6, status = "info", solidHeader = TRUE,
                            title = "Sleep",
                            # bubblesOutput("packagePlot", width = "100%", height = 600)
                        ),
                        box(
                            width = 6, status = "info",
                            title = "Something else",
                            tableOutput("packageTable")
                        )
                    )
            ),
            tabItem("rawdata",
                    numericInput("maxrows", "Rows to show", 25),
                    verbatimTextOutput("rawtable"),
                    downloadButton("downloadCsv", "Download as CSV")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(
        eventExpr = {
            input$data_in
        },
        handlerExpr = {

            dat <- readxl::read_excel(input$data_in$datapath)
            dat_long <- clean_data(dat = dat)
            
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
