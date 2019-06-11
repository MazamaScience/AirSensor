#
# This is the server logic of AirShiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about Shiny applications here: 
#
#    http://shiny.rstudio.com/
#
# - Mazama Science

library(AirSensor)
# ---- Debug 
pas <- example_pas[which(stringr::str_detect(example_pas$label, "SCNP")),] 
pas_labels <- 
    pas$label[-which(stringr::str_detect(pas$label, " B"))]
# ----

# Define server logic 
shiny::shinyServer(
    function(input, output, session) {

        # Leaflet render
        output$leaflet <- 
            leaflet::renderLeaflet({
                if ( stringr::str_detect(input$leaflet_select, "pm25") ) {
                    AirSensor::pas_leaflet(
                        pas, 
                        parameter = input$leaflet_select, 
                        paletteName = "Spectral")
                } else {
                    AirSensor::pas_leaflet(
                        pas,
                        parameter = input$leaflet_select, 
                        paletteName = input$leaflet_select
                    )
                }
            })
        
        # Update Selected pas based on leaflet selection
        shiny::observe({
            shiny::updateSelectInput(
                session, 
                inputId = "pas_select", 
                selected = input$leaflet_marker_click[1]
            )
        })
        
       # # Text debug output 
       #  output$test <-
       #      shiny::renderText(paste0(input$leaflet_marker_click))
        
        # Standard Plot output   
        fplot <-
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
                
                if ( input$plot_type_select == "daily_plot" ) {
                    
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
                        ggplot2::ylab("\u03bcg / m\u00b3") + 
                        ggplot2::theme_minimal()
                    
                    pm25_avg_bar <- 
                        ggplot2::geom_bar(                   
                            data = ast$data,
                            mapping = ggplot2::aes(
                                x = .data$datetime, 
                                y = pm25_AB_avg[,1],
                                fill = pm25_AB_avg[,1]), 
                            stat = "identity", 
                            show.legend = FALSE
                        )
                
                    return(pm25_plot + pm25_avg_bar)
                    
                } else if ( input$plot_type_select == "multi_plot" ) { 
                    
                    return(AirSensor::pat_multiplot(pat))
                    
                } else if ( input$plot_type_select == "raw_plot" ) {
                    
                    return(AirSensor::pat_multiplot(pat, plottype = "pm25"))
                           
                }
            
            })
        
        # Dygraph (JS) output
        fdygraph <- 
            dygraphs::renderDygraph({
                    pat <- 
                        AirSensor::pat_load(
                            label = input$leaflet_marker_click[1], 
                            startdate = input$date_range[1], 
                            enddate = input$date_range[2]
                        ) 
                    AirSensor::pat_dygraph(pat) # HANDLE SPECIAL CASE
            })
        
        output$selected_plot <- fplot 
        
        # TODO: HANDLE SPECIAL DYGRAPH CASE
        
    }
)
