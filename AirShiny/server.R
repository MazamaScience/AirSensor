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
library(MazamaCoreUtils)

pas <- AirSensor::pas_load()
pas_community <- unique(pas$communityRegion) %>% na.omit()

logger.debug("----- server() -----")

# ----- Define Server Logic ----------------------------------------------------

shiny::shinyServer(
    function(input, output, session) {
       
        # Capture date inputs  
        get_dates <- function() {
            
            sd <- lubridate::ymd(input$date_selection) - 
                  lubridate::ddays(as.numeric(input$lookback_days))
            
            ed <- lubridate::ymd(input$date_selection)
            
            return(c(sd, ed))
            
        }
        
        # Capture PAT selection from leaflet(?)
        reload_pat <- function(selector = FALSE) {
            
            logger.debug(" # reload_pat #")
            
            d <- get_dates()
            
            if ( selector ) {
                
                pat <-
                    AirSensor::pat_load(
                        label = input$pas_select, 
                        startdate = d[1],
                        enddate = d[2]
                    ) 
            
            } else { 
                
                pat <-
                    AirSensor::pat_load(
                        label = input$leaflet_marker_click[1], 
                        startdate = d[1],
                        enddate = d[2]
                    ) 
            
            } 
            
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
                d <- get_dates()
                
                if ( input$plot_type_select == "daily_plot" ) {
                    
                    AirSensor::shiny_barplot(
                        pat, 
                        period = "1 day", 
                        startdate = d[1], 
                        enddate = d[2]
                    )
                    
                } else if ( input$plot_type_select == "multi_plot" ) { 
                    
                    AirSensor::pat_multiplot(pat)
                    
                }  else if ( input$plot_type_select == "hourly_plot" ) {
                    
                    AirSensor::shiny_barplot(
                        pat, 
                        period = "1 hour",
                        startdate = d[1], 
                        enddate = d[2]
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
                        input$pas_select != "", 
                        "Select a Sensor"
                    )
                )
                
                pat <- reload_pat(selector = TRUE)
                
                data <- pat$data 
                
                return(data)
                
            })
        
        # Meta Table
        output$meta_explorer <- 
            shiny::renderTable({
                
                pat <- reload_pat(selector = TRUE)
                
                meta <-
                    dplyr::data_frame(
                        "Community" = input$comm_select,
                        "Sensor Type" = pat$meta$sensorType,
                        "Longitude" = pat$meta$longitude, 
                        "Latitude" = pat$meta$latitude,
                        "State" = pat$meta$stateCode, 
                        "Country" = pat$meta$countryCode, 
                        "Timezone" = pat$meta$timezone
                    )
                
                return(meta)
                
            })
        
        # Download button
        output$download_data <- 
            shiny::downloadHandler(
                filename = function() {
                    
                    d <- get_dates()
                    pat <- reload_pat(selector = TRUE)
                    
                    paste0(
                        pat$meta$label,
                        "_",
                        d[1],
                        "_",
                        d[2], 
                        ".csv"
                        
                    )
                    
                }, 
                
                content = function(file) {
                    
                    pat <- reload_pat(selector = TRUE)
                    write.csv(pat$data, file = file)
                    
                }
                
            ) 
        
        # Leaflet selection label
        output$selected_label <- 
            shiny::renderTable({
                
                validate(
                    need(
                        input$leaflet_marker_click != "", 
                        ""
                    )
                )
                
                dplyr::tibble(
                    "Sensor" = input$leaflet_marker_click[1], 
                    "Latitude" = input$leaflet_marker_click[3],
                    "Longitude" = input$leaflet_marker_click[4]
                )
                
            })

    }
    
)
