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
library(bslib)
library(bsicons)

hourly <- read.csv("./bike/hour.csv")
hourly$dteday <- as.Date(hourly$dteday, format = "%Y-%m-%d")
hourly <- hourly |>
    mutate(date = as.POSIXct(dteday) + hr * 3600)

daily <- read.csv("./bike/day.csv")
daily$dteday <- as.Date(daily$dteday, format = "%Y-%m-%d")
daily <- daily |>
    mutate(date = as.POSIXct(dteday))

# Define UI 
ui <- navbarPage(
    title="Bike Rental Analysis",
    # themeSelector(),
    theme = shinytheme("cerulean"),

    # Home page with instructions
    tabPanel("Home",
        fluidPage(
            h2("How to use"),
            p("Select Time Subdivision: data is available per hour or by daily aggregate"),
            p("Select Date Range"),
            p("Add markers: see which data points are holidays or working days"),
            p("All weather variables are normalized"),
            h2("TODO/IDEAS (remove later)"),
            p("Rider Demographic: Casual vs Registered"),
            p("weathersit (not sure what to do with this)"),
            p("add a legend")
        )),
    tabPanel(
        "Analysis",
        sidebarLayout(
            # Sidebar for input
            sidebarPanel(
                helpText("Analyze bike usage in the Capital Bikeshare System from 2011-2012"),
                selectInput("time", "Time subdivision",
                            choices = c("Hourly" = "hourly", 
                                        "Daily" = "daily"),
                            selected = "daily"),
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
                ),
                actionButton("reset_date", "Reset to Full Range")
            ),
            # Main plots
            mainPanel(
                layout_columns(
                    col_widths = c(6, 6),
                    value_box(
                        title = "Total Rentals",
                        value = textOutput("total_count"),
                        showcase = bs_icon("bicycle"),
                        theme = "primary",
                        "Over Selected Date Range"
                    ),
                    value_box(
                        title = "Average Rentals",
                        value = textOutput("average_count"),
                        showcase = bs_icon("bicycle"),
                        theme = "primary",
                        "Over Selected Date Range & Subdivision"
                    )
                ),
                tabsetPanel(
                    id = "main_tabs",
                    tabPanel(
                        "Time Series",
                        plotlyOutput("time_plot")
                    ),
                    tabPanel(
                        "Scatterplot",
                        p("TODO: maybe add a color?"),
                        selectInput("x",
                            "X axis",
                            choices = c("Temperature" = "temp",
                                        "Feeling Temperature" = "atemp", 
                                        "Humidity" = "hum", 
                                        "Windspeed" = "windspeed")
                        ),
                        plotlyOutput("scatter_plot")
                    ),
                    tabPanel(
                        "Demand Patterns",
                        p("TODO: make hour only work if hourly is selected / month & season cant both be selected"),
                        p("also considering putting this on a different page"),
                        selectInput(
                            "heat_x",
                            "Select X-axis variable:",
                            choices = c("hr", "weekday", "mnth", "season"),
                            selected = "mnth"
                        ),

                        selectInput(
                            "heat_y",
                            "Select Y-axis variable:",
                            choices = c("weekday", "hr", "mnth", "season"),
                            selected = "weekday"
                        ),
                        plotlyOutput("heatmap")
                    )
                )
            )  
        ), 
    )
)

server <- function(input, output, session) {
    # refresh data after each input change
    filtered_data <- reactive({
        data <- if (input$time == "hourly") {
            hourly
        } else {
            daily
        }

        start_date <- as.POSIXct(input$date[1])
        end_date <- as.POSIXct(input$date[2] + 1)

        data |>
            filter(
                date >= start_date,
                date < end_date
            )
    })

    # KPI Cards
    output$total_count <- renderText({
        sum(filtered_data()$cnt)
    })
    output$average_count <- renderText({
        mean(filtered_data()$cnt)
    })

    # Time Series
    output$time_plot <- renderPlotly({
        p <- ggplot(filtered_data(), aes(x = date, y = cnt)) +
            geom_line() +
            labs(
                x = "Date",
                y = "Rentals"
            ) +
            theme_minimal()

        if (input$time == "daily") {
            p <- p + labs(title = "Daily Bike Rentals")
        } else {
            p <- p + labs(title = "Hourly Bike Rentals")
        }

        # Add holiday markers when selected
        if ("holiday" %in% input$graphpoints) {
        p <- p +
            geom_point(
            data = filter(filtered_data(), holiday == 1),
            color = "#E69F00",
            size = 2.5
            )
        }

        # Add workday markers when selected
        if ("workingday" %in% input$graphpoints) {
        p <- p +
            geom_point(
            data = filter(filtered_data(), workingday == 1),
            color = "#009E73",
            size = 2
            )
        }

        p
    })

    # Scatter plot
    output$scatter_plot <- renderPlotly({
        p <- ggplot(filtered_data(), aes(x = .data[[input$x]], y = cnt)) +
            geom_point(size = 2.5, alpha=0.5) +
            labs(
                x = input$x,
                y = "Bike Rentals"
            ) +
            theme_minimal()

        # Add holiday markers only when selected
        if ("holiday" %in% input$graphpoints) {
            p <- p +
                geom_point(
                    data = filter(filtered_data(), holiday == 1),
                    aes(x = .data[[input$x]], y = cnt),
                    color = "#E69F00",
                    size = 2.5
                )
        }

        # Add working-day markers only when selected
        if ("workingday" %in% input$graphpoints) {
            p <- p +
                geom_point(
                    data = filter(filtered_data(), workingday == 1),
                    aes(x = .data[[input$x]], y = cnt),
                    color = "#009E73",
                    size = 2
                )
        }

        p
    })

    # Demand Patterns
    output$heatmap <- renderPlotly({
        heat_data <- filtered_data() |>
            group_by(.data[[input$heat_x]], .data[[input$heat_y]]) |>
            summarise(
            avg_rentals = mean(cnt, na.rm = TRUE),
            .groups = "drop"
            )
        p <- ggplot(heat_data, aes(x=.data[[input$heat_x]], y=.data[[input$heat_y]], fill=avg_rentals)) +
            geom_tile() + 
            labs(
                title = "Average Bike Rentals Heat Map",
                x = input$heat_x,
                y = input$heat_y
            ) +
            theme_minimal()

        p
    })

    # Reset Date Button
    observeEvent(input$reset_date, {
    updateDateRangeInput(
        session,
        "date",
        start = min(daily$dteday),
        end = max(daily$dteday)
    )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
