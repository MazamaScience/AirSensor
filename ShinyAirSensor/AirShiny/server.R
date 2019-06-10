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
                        label = input$pas_select, 
                        startdate = input$date_range[1],
                        enddate = input$date_range[2]
                    )
                
                    AirSensor::pat_multiplot(
                        pat, 
                        columns = 1)

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
        
        
    }
)
