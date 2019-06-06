# This Shiny app demonstrates the use of AST Models to display daily averages
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
            )
            
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
                    period = "1 day" 
                )
            
            pm25_AB_avg <- 
                ast$data %>% 
                dplyr::select(
                    .data$pm25_A_mean, 
                    .data$pm25_B_mean
                ) %>% 
                dplyr::transmute(
                    pm25_AB_avg = 
                        (.data$pm25_A_mean + .data$pm25_B_mean) / 2
                )
            
            pm25_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = pm25_AB_avg
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "PM2.5"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("\u03bcg / m\u00b3")
            
            pm25_avg_bar <- 
                ggplot2::geom_bar(                   
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = pm25_AB_avg[,1]
                    ), 
                    stat = "identity"
                )
            
            return(pm25_plot + pm25_avg_bar)
            
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
                    period = "1 day"
                )
            
            humidity_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = .data$humidity_mean
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "Humidity"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("RH%") +
                ggplot2::geom_bar(                   
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data$humidity_mean
                    ), 
                    stat = "identity"
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
                    period = "1 day"
                )
            
            temperature_plot <- 
                ast$data %>% 
                ggplot2::ggplot(
                    ggplot2::aes(
                        x = .data$datetime, 
                        y = .data$temperature_mean
                    )
                ) + 
                ggplot2::ggtitle(
                    label = "Temperature"
                ) + 
                ggplot2::xlab("Datetime") + 
                ggplot2::ylab("\u00b0F") +
                ggplot2::geom_bar(                   
                    data = ast$data,
                    mapping = ggplot2::aes(
                        x = .data$datetime, 
                        y = .data$temperature_mean
                    ), 
                    stat = "identity"
                )
            
            return(temperature_plot)
            
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
