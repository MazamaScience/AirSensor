#
# This is the server logic of AirShiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about Shiny applications here: 
#
#    http://shiny.rstudio.com/
#
# - Mazama Science

# ---- Debug 
library(AirSensor)
library(rlang)
library(magrittr) 
library(MazamaCoreUtils)
pas <- AirSensor::pas_load()
pas_community <- unique(pas$communityRegion) %>% na.omit()

logger.debug("----- server -----")
# ----

# Define server logic 
shiny::shinyServer(
    function(input, output, session) {
        
        # Reload the PAT based on the selected PAT and date interval 
        reload_pat <- function() {
            
            logger.debug(" # reload_pat #")
            
            pat <- AirSensor::pat_load(
                label = input$leaflet_marker_click[1], 
                enddate = lubridate::ymd(input$date_selection), 
                startdate = lubridate::ymd(input$date_selection) - 
                    lubridate::ddays(as.numeric(input$lookback_days))
            ) 
            
            return(pat)
            
        }    
        
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
                    parameter = "pm25_current", 
                    paletteName = "Purple"
                )
                
            })
        
        # Update Selected pas based on leaflet selection
        shiny::observe({
            
            pas_in_comm <- 
                pas[which(
                    stringr::str_detect(
                        pas$communityRegion, 
                        input$comm_select
                    ) & 
                        !stringr::str_detect(
                            pas$label, 
                            " B"
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
                
                sd <- lubridate::ymd(input$date_selection) - 
                    lubridate::ddays(as.numeric(input$lookback_days))
                ed <- lubridate::ymd(input$date_selection)
                
                # Validate a pas selection has been made. If not display message.
                validate(
                    need(
                        input$leaflet_marker_click != "", 
                        "Select a Purple Air Sensor"
                    )
                )
                
                # NOTE: The current method is not filtering ANY outliers for
                # NOTE: ANY of the plots - may be prone to change.
                pat <- reload_pat()
                
                if ( input$plot_type_select == "daily_plot" ) {
                    
                    AirSensor::shiny_barplot(
                        pat, 
                        period = "1 day", 
                        startdate = sd, 
                        enddate = ed
                    )
                    
                } else if ( input$plot_type_select == "multi_plot" ) { 
                    
                    AirSensor::pat_multiplot(pat)
                    
                }  else if ( input$plot_type_select == "hourly_plot" ) {
                    
                    AirSensor::shiny_barplot(
                        pat, 
                        period = "1 hour",
                        startdate = sd, 
                        enddate = ed
                    )
                    
                }
                
            })
        
        # TODO: HANDLE SPECIAL DYGRAPH CASE
        
        # Data Table
        output$data_explorer <- 
            shiny::renderDataTable({
                
                # Validate a pas selection has been made. If not display message.
                validate(
                    need(
                        input$leaflet_marker_click != "", 
                        "Select a Purple Air Sensor from the Interactive Map."
                    )
                )
                
                pat <- reload_pat()
                
                data <- pat$data 
                
                return(data)
                
            })
        
        # Meta Table
        output$meta_explorer <- 
            shiny::renderTable({
                
                pat <- reload_pat()
                
                meta <-
                    dplyr::select(
                        pat$meta,
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
        output$download_data <- 
            shiny::downloadHandler(
                filename = function() {
                    
                    sd <- lubridate::ymd(input$date_selection) - 
                        lubridate::ddays(as.numeric(input$lookback_days))
                    ed <- lubridate::ymd(input$date_selection)
                    
                    paste0(
                        input$leaflet_marker_click[1],
                        "_",
                        sd,
                        "_",
                        ed, 
                        ".csv"
                        
                    )
                    
                }, 
                
                content = function(file) {
                    
                    write.csv(reload_pat()$data, file = file)
                    
                }
                
            )
        
    }
    
)
