# Bike Rental Analysis (Shiny + bslib)
# Data: https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(bslib)
library(bsicons)

# Shared look for every plot
theme_set(theme_minimal() + theme(panel.grid = element_blank()))

# Season labels
season_labels <- c("Winter", "Spring", "Summer", "Fall")

hourly <- read.csv("./bike/hour.csv") |>
    mutate(
        dteday = as.Date(dteday),
        # Readable labels for the heatmap (weekday 0 = Sunday)
        weekday_name = factor(weekday, levels = 0:6,
                              labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")),
        month_name = factor(mnth, levels = 1:12, labels = month.abb),
        season_name = factor(season, levels = 1:4, labels = season_labels)
    )

daily <- read.csv("./bike/day.csv") |>
    mutate(
        dteday = as.Date(dteday),
        date = as.POSIXct(dteday),
        season_name = factor(season, levels = 1:4, labels = season_labels),
        weather_name = factor(weathersit, levels = 1:4,
                              labels = c("Clear / Partly Cloudy", "Mist / Cloudy",
                                         "Light Rain / Snow", "Heavy Rain / Snow"))
    )

# Scatterplot choices (names are shown in the UI and used as axis/legend titles)
scatter_x_choices <- c("Temperature" = "temp",
                       "Feeling Temperature" = "atemp",
                       "Humidity" = "hum",
                       "Wind Speed" = "windspeed")

# Season colors for the scatterplot
season_colors <- c("Winter" = "#0072B2",   # blue
                   "Spring" = "#009E73",   # green
                   "Summer" = "#E6C200",   # yellow
                   "Fall"   = "#D55E00")   # orange

# Weather colors for the scatterplot
weather_colors <- c("Clear / Partly Cloudy" = "#E6C200",   # sunny yellow
                    "Mist / Cloudy"         = "#7F7F7F",   # gray
                    "Light Rain / Snow"     = "#0072B2",   # blue
                    "Heavy Rain / Snow"     = "#1B2A6B")   # dark navy

# Heatmap choices (names are shown in the UI and used as axis titles)
heat_choices <- c("Hour of Day" = "hr",
                  "Day of Week" = "weekday_name",
                  "Month" = "month_name",
                  "Season" = "season_name")

scatter_color_choices <- c("Season" = "season_name",
                           "Weather" = "weather_name",
                           "None" = "none")

# Define UI 
app_theme <- bs_theme(bootswatch = "cerulean")

ui <- page_navbar(
    id = "nav",
    title="Bike Rental Analysis",
    theme = app_theme,
    navbar_options = navbar_options(bg = bs_get_variables(app_theme, "primary")),
    # Analysis tabs fill the window; Home flows like a normal page
    fillable = c("Daily Analysis", "Demand Patterns"),

    # Sidebar for input (closed on Home, opened on analysis tabs by the server)
    sidebar = sidebar(
        id = "sidebar",
        open = "closed",
        helpText("Analyze bike usage in the Capital Bikeshare System from 2011-2012"),

        # Filters for the Daily Analysis tab
        conditionalPanel(
            condition = "input.nav === 'Daily Analysis'",
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
                        start = as.Date("2011-01-01"),
                        end = as.Date("2011-12-31"),
                        min = min(daily$dteday),
                        max = max(daily$dteday)
            ),
            # Quick date presets
            p("Date Shortcuts:"),
            actionButton("year_2011", "2011"),
            actionButton("year_2012", "2012"),
            actionButton("year_both", "All Dates")
        ),

        # Filters for the Demand Patterns tab
        conditionalPanel(
            condition = "input.nav === 'Demand Patterns'",
            selectInput(
                "heat_x",
                "Select X-axis variable:",
                choices = heat_choices,
                selected = "month_name"
            ),
            # Y-axis choices are narrowed by the server based on the X-axis pick
            selectInput(
                "heat_y",
                "Select Y-axis variable:",
                choices = heat_choices[c("Hour of Day", "Day of Week")],
                selected = "weekday_name"
            )
        )
    ),

    # Home page with instructions
    nav_panel("Home",
        fluidRow(
            # Left: text
            column(8,
                h3("About the Data"),
                p("Daily and hourly Capital Bikeshare rentals in Washington, D.C. (2011-2012), with weather and calendar details. ",
                  a("UCI dataset", href = "https://archive.ics.uci.edu/dataset/275/bike+sharing+dataset", target = "_blank")),
                h3("How to Use"),
                tags$ul(
                    tags$li(strong("Daily Analysis: "), "daily rentals over time and vs. weather.",
                        tags$ul(
                            tags$li(em("Left sidebar:"), " pick a date range, and mark holidays (red) or work days."),
                            tags$li(em("Time Series:"), " split casual vs. registered riders, and smooth with a rolling average."),
                            tags$li(em("Scatterplot:"), " choose a weather metric, and color by season or weather.")
                        )
                    ),
                    tags$li(strong("Demand Patterns: "), "average hourly rentals as a heatmap.",
                        tags$ul(
                            tags$li(em("Left sidebar:"), " choose the X and Y variables (hour, day, month, or season).")
                        )
                    )
                )
            ),
            # Right: image (fills its column, shrinks on small screens)
            column(4,
                img(src = "image.jpeg", width = "90%", alt = "Cyclist riding past the White House")
            )
        )
    ),
    nav_panel(
        "Daily Analysis",
        layout_columns(
            fill = FALSE,
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
                "Over Selected Date Range"
            )
        ),
        navset_card_tab(
            nav_panel(
                "Time Series",
                layout_sidebar(
                    sidebar = sidebar(
                        position = "right",
                        open = "always",
                        input_switch("split", "Split Between Casual & Registered"),
                        helpText("Casual riders use the system without a membership (e.g. single rides or day passes); registered riders are members, often regular commuters."),
                        sliderInput("smooth", "Rolling Average (days)",
                                    min = 1, max = 28, value = 1, step = 1),
                        helpText("1 = no smoothing")
                    ),
                    plotlyOutput("time_plot")
                )
            ),
            nav_panel(
                "Scatterplot",
                layout_sidebar(
                    sidebar = sidebar(
                        position = "right",
                        open = "always",
                        selectInput("x",
                            "Weather Metric",
                            choices = scatter_x_choices
                        ),
                        helpText("Weather variables are normalized to a 0-1 scale."),
                        radioButtons("color_by",
                            "Color by",
                            choices = scatter_color_choices
                        )
                    ),
                    plotlyOutput("scatter_plot")
                )
            )
        )
    ),
    nav_panel(
        "Demand Patterns",
        plotlyOutput("heatmap")
    )
)

server <- function(input, output, session) {
    # refresh data after each input change
    filtered_data <- reactive({
        daily |> filter(between(dteday, input$date[1], input$date[2]))
    })

    # Close the sidebar on Home, open it on the analysis tabs
    observeEvent(input$nav, {
        toggle_sidebar("sidebar", open = input$nav != "Home")
    })

    # Heatmap Y-axis variables that can't be paired with the chosen X-axis variable:
    # the same variable, and month with season (they describe the same thing)
    heat_blocked <- function(x) {
        if (x %in% c("month_name", "season_name")) c("month_name", "season_name") else x
    }

    # Narrow the Y-axis choices whenever the X-axis variable changes
    observeEvent(input$heat_x, {
        y_choices <- heat_choices[!heat_choices %in% heat_blocked(input$heat_x)]
        y_selected <- if (input$heat_y %in% y_choices) input$heat_y else y_choices[1]
        updateSelectInput(session, "heat_y", choices = y_choices, selected = y_selected)
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
        # Centered rolling average of the rental counts (window can't exceed the days shown)
        window <- min(input$smooth, nrow(filtered_data()))
        smoothed_data <- filtered_data() |>
            mutate(across(c(cnt, casual, registered),
                          \(x) as.numeric(stats::filter(x, rep(1 / window, window), sides = 2))))

        p <- ggplot(smoothed_data, aes(x = date, y = cnt)) +
            labs(
                title = "Daily Bike Rentals",
                x = "Date",
                y = "Rentals"
            ) +
            theme(legend.position = "bottom") +
            scale_color_manual(
                name = NULL,
                values = c(
                    "Total" = "#333333",
                    "Casual" = "#0072B2",
                    "Registered" = "#D55E00",
                    "Holiday" = "#CC0000",
                    "Working Day" = "grey85"
                )
            )

        # Shade working days gray (added first so the lines draw on top)
        # plotly can't draw infinite heights, so shade from 0 to the tallest plotted value
        if ("workingday" %in% input$graphpoints) {
            if (input$split) {
                y_top <- max(filtered_data()$casual, filtered_data()$registered)
            } else {
                y_top <- max(filtered_data()$cnt)
            }

            # One box per run of consecutive working days, from the first day's point
            # to the last day's point (e.g. Monday to Friday)
            workday_runs <- filtered_data() |>
                arrange(date) |>
                mutate(run = consecutive_id(workingday)) |>
                filter(workingday == 1) |>
                group_by(run) |>
                summarise(start = min(date), end = max(date), .groups = "drop") |>
                # a lone working day would have zero width, so widen it half a day each side
                mutate(
                    single = start == end,
                    start = if_else(single, start - 43200, start),
                    end = if_else(single, end + 43200, end)
                )

            p <- p +
                geom_rect(
                    data = workday_runs,
                    # Uses the color scale (not fill) so the plot has a single legend
                    aes(xmin = start, xmax = end, ymin = 0, ymax = y_top,
                        color = "Working Day"),
                    fill = "grey85",
                    inherit.aes = FALSE
                )
        }

        if (input$split) {
            p <- p +
                geom_line(aes(y = casual, color = "Casual")) +
                geom_line(aes(y = registered, color = "Registered"))
        } else {
            p <- p + geom_line(aes(color = "Total"))
        }

        # Mark holidays with dotted red vertical lines when selected
        if ("holiday" %in% input$graphpoints) {
            p <- p +
                geom_vline(
                    data = filter(filtered_data(), holiday == 1),
                    aes(xintercept = date, color = "Holiday"),
                    linetype = "dotted"
                )
        }

        p
    })

    # Scatter plot
    output$scatter_plot <- renderPlotly({
        scatter_data <- filtered_data() |>
            mutate(day_type = if_else(workingday == 1, "Working Day", "Non-working Day"))

        p <- ggplot(scatter_data, aes(x = .data[[input$x]], y = cnt)) +
            labs(
                x = names(scatter_x_choices)[scatter_x_choices == input$x],
                y = "Bike Rentals"
            )

        # Color by season or weather (skipped for "None")
        if (input$color_by != "none") {
            p <- p +
                aes(color = .data[[input$color_by]]) +
                scale_color_manual(values = c(season_colors, weather_colors)) +
                labs(color = names(scatter_color_choices)[scatter_color_choices == input$color_by])
        }

        # Draw working days as an X (keeping their color) when selected
        if ("workingday" %in% input$graphpoints) {
            p <- p +
                geom_point(aes(shape = day_type), size = 2.5, alpha = 0.5) +
                scale_shape_manual(
                    name = NULL,
                    values = c("Non-working Day" = 16, "Working Day" = 4)
                )
        } else {
            p <- p + geom_point(size = 2.5, alpha = 0.5)
        }

        # Circle holidays with a red open circle when selected
        if ("holiday" %in% input$graphpoints) {
            p <- p +
                geom_point(
                    data = filter(scatter_data, holiday == 1),
                    shape = 1,
                    size = 5,
                    stroke = 1,
                    color = "#CC0000"
                )
        }

        p
    })

    # Demand Patterns
    output$heatmap <- renderPlotly({
        # Wait for the Y-axis choices to update if the pair isn't allowed yet
        req(!(input$heat_y %in% heat_blocked(input$heat_x)))

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
                x = names(heat_choices)[heat_choices == input$heat_x],
                y = names(heat_choices)[heat_choices == input$heat_y],
                fill = "Avg. Rentals"
            )

        p
    })

    # Date preset buttons
    set_dates <- function(start, end) {
        updateDateRangeInput(session, "date", start = as.Date(start), end = as.Date(end))
    }
    observeEvent(input$year_2011, set_dates("2011-01-01", "2011-12-31"))
    observeEvent(input$year_2012, set_dates("2012-01-01", "2012-12-31"))
    observeEvent(input$year_both, set_dates("2011-01-01", "2012-12-31"))
}

# Run the application 
shinyApp(ui = ui, server = server)
