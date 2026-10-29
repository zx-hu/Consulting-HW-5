#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
# 
# https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset

library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(plotly)

df <- read.csv("bike/hour.csv")
df$dteday <- as.Date(df$dteday, format = "%Y-%m-%d")

daily <- df |>
  group_by(dteday) |>
  summarise(
    cnt = sum(cnt),
    holiday = first(holiday),
    workingday = first(workingday),
    .groups = "drop"
  )

# Define UI for application that draws a histogram
ui <- fluidPage(
    themeSelector(),
    # theme = shinytheme("darkly")
    # Application title
    titlePanel("Bike Rental Analysis"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            helpText("Create demographic maps with information from the 2010 Census"),
            checkboxGroupInput("graphpoints",
                        "Add markers to graph",
                        choices = c(
                            "Holidays" = "holiday", 
                            "Work Days" = "workingday"
                        ),
                        selected = NULL 
            ),
            dateRangeInput("date",
                        "Date Range:",
                        start = min(daily$dteday),
                        end = max(daily$dteday),
                        min = min(daily$dteday),
                        max = max(daily$dteday)
            )
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                id = "main_tabs",
                tabPanel(
                    "Time Series",
                    plotlyOutput("time_plot")
                ),
                tabPanel(
                    "Scatterplot",
                    selectInput("x",
                        "X axis",
                        choices = c("temp", "hum", "windspeed")
                    ),
                    plotlyOutput("scatter_plot")
                )
                
            )
        )  
    ), 
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$time_plot <- renderPlotly({

        # Filter the data using the selected date range
        filtered_daily <- daily |>
            filter(
                dteday >= input$date[1],
                dteday <= input$date[2]
            )
        p <- ggplot(filtered_daily, aes(x = dteday, y = cnt)) +
            geom_line() +
            labs(
                title = "Daily Bike Rentals (2011–2012)",
                x = "Date", y = "Daily Rentals"
            ) +
            theme_minimal()

        # Add holiday markers when selected
        if ("holiday" %in% input$graphpoints) {
        p <- p +
            geom_point(
            data = filter(filtered_daily, holiday == 1),
            color = "#E69F00",
            size = 2.5
            )
        }

        # Add workday markers when selected
        if ("workingday" %in% input$graphpoints) {
        p <- p +
            geom_point(
            data = filter(filtered_daily, workingday == 1),
            color = "#009E73",
            size = 2
            )
        }

        p
    })

    output$scatter_plot <- renderPlotly({
        # Filter hourly data using the selected date range
        filtered_df <- df |>
            filter(
                dteday >= input$date[1],
                dteday <= input$date[2]
            )

        p <- ggplot(filtered_df, aes_string(x=input$x, y="cnt")) +
            geom_point(size = 2.5, alpha=0.5) +
            theme_minimal()

        # Add holiday markers only when selected
        if ("holiday" %in% input$graphpoints) {
            p <- p +
                geom_point(
                    data = filter(filtered_df, holiday == 1),
                    aes_string(x = input$x, y = "cnt"),
                    color = "#E69F00",
                    size = 2.5
                )
        }

        # Add working-day markers only when selected
        if ("workingday" %in% input$graphpoints) {
            p <- p +
                geom_point(
                    data = filter(filtered_df, workingday == 1),
                    aes_string(x = input$x, y = "cnt"),
                    color = "#009E73",
                    size = 2.5
                )
        }

        p
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
