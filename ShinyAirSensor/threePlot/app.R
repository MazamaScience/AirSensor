# This Shiny app demonstrates the use of AST models for PurpleAir
# - Mazama Science

# ---- Debug 
pas <- example_pas[which(stringr::str_detect(example_pas$label, "SCNP")),] 
pas_labels <- 
    pas$label[-which(stringr::str_detect(pas$label, " B"))]
# ----

# Define UI for application
ui <- shiny::fluidPage(
    shiny::fluidRow(
        column(
            width = 2,
            
            
            # PAS selection input
            shiny::selectInput(
                inputId = "pas_select", 
                label = "Purple Air Sensors:",
                choices = pas_labels
            ),
            
            shiny::dateRangeInput(
                inputId = "date_range", 
                label = "Date Range:"
            ),
            
            # Period Selection
            shiny::textInput(
                inputId = "period_select", 
                label = "Period:",
                value = "5 min", 
                placeholder = "X month/week/day/hour/min/sec"
            ),
            
            # Plot stat selection
            shiny::selectInput(
                inputId = "stat_select", 
                label = "Plot Stats:", 
                choices = c(
                    "Mean" = "_mean",
                    "Standard Deviation" = "_sd"
                )
            )
            
            # shiny::actionButton("update_button", "update") # Update Button
        ), 
        
        column(
            width = 10,
            # Plot outputs
            shiny::plotOutput(outputId = "pm25_plot"), 
            shiny::plotOutput(outputId = "temp_plot"), 
            shiny::plotOutput(outputId = "hum_plot") 
            
        )
    )
)
# Define server logic
server <- function(input, output) {
    
    # Table render
    output$pas_tbl <- 
        shiny::renderTable({
            
            pat <- 
                AirSensor::pat_load(
                    label = input$pas_select, 
                    startdate = 20190411, # ---- DEMO DATERANGE
                    enddate = 20190521
                )
            
            tbl <- 
                pat[["meta"]] %>% 
                dplyr::select(
                    .data$label, 
                    .data$sensorType, 
                    .data$longitude, 
                    .data$latitude, 
                    .data$stateCode
                )
            
        })
    
    output$pm25_plot <- 
        shiny::renderPlot({
            pat <- 
                AirSensor::pat_load(
                    label = input$pas_select, 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                ) 
            ast <- 
                AirSensor::pat_createASTimeseries(
                    pat = pat, 
                    period = input$period_select 
                )
            
            pm25_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[
                            paste0("pm25_A", input$stat_select)
                            ]]
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "PM2.5"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("\u03bcg / m\u00b3")
            
            pm25_A <- 
                ggplot2::geom_point(
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[paste0(
                            "pm25_A",
                            input$stat_select
                        )]]
                    ), color = "red", shape = 18,
                    size = 2, 
                    alpha = 1/2
                )
            
            pm25_B <- 
                ggplot2::geom_point(
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[paste0(
                            "pm25_B",
                            input$stat_select
                        )]]
                    ), 
                    color = "blue", shape = 18,
                    size = 2,
                    alpha = 1/2
                )
            
            # Smoothing via polynomial regression 
            
            # smooth_A <- 
            #     ggplot2::geom_smooth(
            #         data = ast$data,
            #         method = "loess",
            #         mapping = ggplot2::aes(
            #             x = .data$datetime, 
            #             y = .data[[paste0(
            #                 "pm25_B",
            #                 input$stat_select
            #             )]]
            #         )
            #     )
            # 
            # smooth_B <- 
            #     ggplot2::geom_smooth(
            #         data = ast$data,
            #         method = "loess",
            #         mapping = ggplot2::aes(
            #             x = .data$datetime, 
            #             y = .data[[paste0(
            #                 "pm25_B",
            #                 input$stat_select
            #             )]]
            #         )
            #     )
            
            
            return(pm25_plot + pm25_A + pm25_B)
            
        })
    
    output$hum_plot <- 
        shiny::renderPlot({
            pat <- 
                AirSensor::pat_load(
                    label = input$pas_select, 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                ) 
            ast <- 
                AirSensor::pat_createASTimeseries(
                    pat = pat, 
                    period = input$period_select 
                )
            
            humidity_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[
                            paste0("humidity", input$stat_select)
                            ]]
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "Humidity"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("RH%") +
                ggplot2::geom_point(
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[paste0(
                            "humidity",
                            input$stat_select
                        )]]
                    ), 
                    color = "black", shape = 18, 
                    size = 2,
                    alpha = 1/2
                )
            
            return(humidity_plot)
            
        })
    
    output$temp_plot <- 
        shiny::renderPlot({
            pat <- 
                AirSensor::pat_load(
                    label = input$pas_select, 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                ) 
            ast <- 
                AirSensor::pat_createASTimeseries(
                    pat = pat, 
                    period = input$period_select 
                )
            
            temperature_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[
                            paste0("temperature", input$stat_select)
                            ]]
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "Temperature"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("\u00b0F") +
                ggplot2::geom_point(
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data[[paste0(
                            "temperature",
                            input$stat_select
                        )]]
                    ), 
                    color = "black", 
                    shape = 18, 
                    size = 2, 
                    alpha = 1/2
                )
            
            return(temperature_plot)
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
