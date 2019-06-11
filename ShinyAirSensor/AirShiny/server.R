#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(AirSensor)
# ---- Debug 
pas <- example_pas[which(stringr::str_detect(example_pas$label, "SCNP")),] 
pas_labels <- 
    pas$label[-which(stringr::str_detect(pas$label, " B"))]
# ----

# Define server logic 
shiny::shinyServer(
    function(input, output) {
        
        output$pm25_plot <- 
            shiny::renderPlot({
                
                pat <- 
                    AirSensor::pat_load(
                        label = input$leaflet_marker_click[1], #uses marker id from leaflet
                        startdate = input$date_range[1],
                        enddate = input$date_range[2]
                    )
                
                    AirSensor::pat_multiplot(
                        pat, 
                        columns = 4)

            })
        
        # Leaflet render
        output$leaflet <- 
            leaflet::renderLeaflet({
                if ( stringr::str_detect(input$leaflet_select, "pm25") ) {
                    AirSensor::pas_leaflet(
                        pas, 
                        parameter = input$leaflet_select, 
                        paletteName = "Spectral")
                }  else {
                    AirSensor::pas_leaflet(
                        pas,
                        parameter = input$leaflet_select, 
                        paletteName = input$leaflet_select
                    )
                }
            })
        
        output$test <-
            shiny::renderText(paste0(input$leaflet_marker_click))
        
        output$pm25_daily_plot <- 
            shiny::renderPlot({
                pat <- 
                    AirSensor::pat_load(
                        label = input$leaflet_marker_click[1], 
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
        
        
    }
)
