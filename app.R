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
            p("Times Series and Scatterplot use daily data, Demand Patterns is subdivided into hourly data"),
            p("Add markers: see which data points are holidays or working days"),
            p("All weather variables are normalized"),
            h2("TODO/IDEAS (remove later)"),
            p("weathersit (not sure what to do with this)"),
            p("		- 1: Clear, Few clouds, Partly cloudy, Partly cloudy
                    - 2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
                    - 3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
                    - 4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog"),
            p("instead of workingday maybe do the inverse and calculate weekends so its less noise on the plot? idk its just a lot of dots it feels messy"),
            p("could add a smoothing slider (like 1d -> 4 wks) for the time series if thats a cool factor?")
        )),
    tabPanel(
        "Analysis",
        sidebarLayout(
            # Sidebar for input
            sidebarPanel(
                helpText("Analyze bike usage in the Capital Bikeshare System from 2011-2012"),
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
                        checkboxGroupInput("rider_demographic",
                            "Split Between Casual & Registered",
                            choices = c(
                                "Enable Split" = "split"
                            ),
                            selected = NULL 
                        ),
                        plotlyOutput("time_plot")
                    ),
                    tabPanel(
                        "Scatterplot",
                        p("TODO: name the factors better"),
                        selectInput("x",
                            "X axis",
                            choices = c("Temperature" = "temp",
                                        "Feeling Temperature" = "atemp", 
                                        "Humidity" = "hum", 
                                        "Windspeed" = "windspeed")
                        ),
                        radioButtons("color_by",
                            "Color by",
                            choices = c("Season" = "season",
                                        "Weather" = "weathersit"),
                        ),
                        plotlyOutput("scatter_plot")
                    ),
                    tabPanel(
                        "Demand Patterns",
                        p("TODO: make month & season cant both be selected"),
                        p("also considering putting this on a different page since it doesnt use the side filters"),
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
        data <- daily

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
            labs(
                title = "Daily Bike Rentals",
                x = "Date",
                y = "Rentals"
            ) +
            theme_minimal()  +
            theme(
                legend.position = "bottom"
            ) +
            scale_color_manual(
                name = "Category",
                values = c(
                    "Casual" = "#0072B2",
                    "Registered" = "#D55E00",
                    "Holiday" = "#E69F00",
                    "Working Day" = "#009E73"
                )
            )

        if ("split" %in% input$rider_demographic){
            p <- p +
                geom_line(aes(y = casual, color = "Casual")) +
                geom_line(aes(y = registered, color = "Registered"))
        }else{
            p <- p + geom_line()
        }

        # Add holiday markers when selected
        if ("holiday" %in% input$graphpoints) {
            holiday_data <- filter(filtered_data(), holiday == 1)
            if ("split" %in% input$rider_demographic) {
                p <- p +
                    geom_point(
                        data = holiday_data,
                        aes(y = casual, color = "Holiday"),
                        size = 2.5
                    ) +
                    geom_point(
                        data = holiday_data,
                        aes(y = registered, color = "Holiday"),
                        size = 2.5
                    )
            } else {
                p <- p +
                    geom_point(
                        data = holiday_data,
                        aes(y = cnt, color = "Holiday"),
                        size = 2.5
                    )
            }
        }

        # Add workday markers when selected
        if ("workingday" %in% input$graphpoints) {
            workingday_data <- filter(filtered_data(), workingday == 1)

            if ("split" %in% input$rider_demographic) {
                p <- p +
                    geom_point(
                        data = workingday_data,
                        aes(y = casual, color = "Working Day"),
                        size = 2
                    ) +
                    geom_point(
                        data = workingday_data,
                        aes(y = registered, color = "Working Day"),
                        size = 2
                    )
            } else {
                p <- p +
                    geom_point(
                        data = workingday_data,
                        aes(y = cnt, color = "Working Day"),
                        size = 2
                    )
            }
        }

        p
    })

    # Scatter plot
    output$scatter_plot <- renderPlotly({
        p <- ggplot(filtered_data(), aes(x = .data[[input$x]], y = cnt, color = factor(.data[[input$color_by]]))) +
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
        heat_data <- hourly |>
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
