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
pas <- AirSensor::example_pas 
pas_community <- unique(AirSensor::example_pas$communityRegion) %>% na.omit()
# ----

# Define server logic 
shiny::shinyServer(
    function(input, output, session) {
    
    # Leaflet render
    output$leaflet <- 
        leaflet::renderLeaflet({
            
            pas_in_comm <- 
                pas[which(
                    stringr::str_detect(
                        pas$communityRegion, 
                        input$comm_select)
                ),]
            
            AirSensor::pas_leaflet_shiny(
                pas_in_comm, 
                parameter = "pm25_1day", 
                paletteName = "Spectral"
            )
            
        })
    
    # Update Selected pas based on leaflet selection
    shiny::observe({
        
        pas_in_comm <- 
            pas[which(
                stringr::str_detect(
                    pas$communityRegion, 
                    input$comm_select
                )
            ),]
        
        shiny::updateSelectInput(
            session, 
            inputId = "pas_select", 
            selected = input$leaflet_marker_click[1], 
            choices = pas_in_comm$label
        )
        
    })
    
    # # Text debug output 
    #  output$test <-
    #      shiny::renderText(paste0(input$leaflet_marker_click))
    
    # Standard Plot output   
    output$selected_plot <-
        shiny::renderPlot({
            
            # NOTE: The current method is not filtering ANY outliers for
            # NOTE: ANY of the plots - may be prone to change.
            pat <- 
                AirSensor::pat_load(
                    label = input$leaflet_marker_click[1], 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                )
            
            if ( input$plot_type_select == "daily_plot" ) {
                
                return(AirSensor::shiny_barplot(pat, period = "1 day"))
                
            } else if ( input$plot_type_select == "multi_plot" ) { 
                
                return(AirSensor::pat_multiplot(pat))
                
            }  else if ( input$plot_type_select == "hourly_plot" ) {
                
                return(AirSensor::shiny_barplot(pat, period = "1 hour"))
            }
            
        })
    
    # TODO: HANDLE SPECIAL DYGRAPH CASE
    
    # Data Table
    output$data_explorer <- 
        shiny::renderDataTable({
            
            pat <- 
                AirSensor::pat_load(
                    label = input$leaflet_marker_click[1], 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                )
            
            data <- pat$data 
            
            return(data)
            
        })
    
    # Meta Table
    output$meta_explorer <- 
        shiny::renderTable({
            
            pat <- 
                AirSensor::pat_load(
                    label = input$leaflet_marker_click[1], 
                    startdate = input$date_range[1], 
                    enddate = input$date_range[2]
                )
            
            meta <-
                pat$meta %>% 
                dplyr::select(
                    "Sensor Label" = .data$label,
                    "Sensor Type" = .data$sensorType,
                    "Longitude" = .data$longitude, 
                    "Latitude" = .data$latitude,
                    "State" = .data$stateCode, 
                    "Country" = .data$countryCode, 
                    "Timezone" = .data$timezone
                )
            
            return(meta)
            
        })
    
    # Download button
    # TODO: FIX RENAMING SCHEME ON PAS SELECTION (REACTIVE?)
    output$download_data <- 
        shiny::downloadHandler(
            filename = paste0(
                input$leaflet_marker_click[1],
                "_",
                input$date_range[1],
                "_",
                input$date_range[2], 
                ".csv"
            ), 
            
            content = function(file) {
                write.csv(
                    AirSensor::pat_load(
                        label = input$leaflet_marker_click[1], 
                        startdate = input$date_range[1], 
                        enddate = input$date_range[2]
                    )$data,
                    file = file
                )
                
            }
            
        )
    
    }

)
