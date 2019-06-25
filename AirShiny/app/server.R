#
# This is the server logic of AirShiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about Shiny applications here: 
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

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
        get_pat <- function(selector = FALSE) {
            
            logger.debug(" # get_pat #")
            
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
        
        # Capture selected PAS based on community selection
        get_pas <- function() {
            
            if ( input$comm_select == "all" )  {
                
                pas <- PAS[which(!is.na(PAS$communityRegion)),]
                
            } else { 
                
                pas <- 
                    PAS[which(
                        stringr::str_detect(
                            PAS$communityRegion, 
                            input$comm_select)
                    ),]
                
            }
            
            return(pas)
            
        }
        
        
        # Leaflet render
        output$leaflet <- 
            leaflet::renderLeaflet({
        
                pas <- get_pas()
                    
                AirSensor::AirShiny_leaflet(
                    pas[which(!stringr::str_detect(pas$label, " B")),],
                    parameter = "pm25_current", 
                    paletteName = "Purple"
                )
                
            })
        
        # Update Selected pas based on leaflet selection
        shiny::observe({
            
            pas <- get_pas()
            
            pas_choices <- 
                pas$label[which(!stringr::str_detect(pas$label, " B"))]
            
            shiny::updateSelectInput(
                session, 
                inputId = "pas_select", 
                selected = input$leaflet_marker_click[1], 
                choices = pas_choices
            ) 
            
        })
        
        # Standard Plot output   
        output$selected_plot <-
            shiny::renderPlot({
                
                # Validate a pas selection has been made. If not display message
                validate(
                    need(
                        input$leaflet_marker_click != "", 
                        "Select a Purple Air Sensor"
                    )
                )
                
                # NOTE: The current method is not filtering ANY outliers for
                # NOTE: ANY of the plots - may be prone to change.
                pat <- get_pat()
                d <- get_dates()
                
                if ( input$plot_type_select == "daily_plot" ) {
                    
                    AirSensor::AirShiny_barplot(
                        pat, 
                        period = "1 day", 
                        startdate = d[1], 
                        enddate = d[2]
                    )
                    
                } else if ( input$plot_type_select == "multi_plot" ) { 
                    
                    AirSensor::pat_multiplot(pat)
                    
                }  else if ( input$plot_type_select == "hourly_plot" ) {
                    
                    AirSensor::AirShiny_barplot(
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
                
                # Validate a pas selection has been made & display if not
                validate(
                    need(
                        input$pas_select != "", 
                        "Select a Sensor"
                    )
                )
                
                pat <- get_pat(selector = TRUE)
                
                data <- pat$data 
                
                return(data)
                
            })
        
        # Meta Table
        output$meta_explorer <- 
            shiny::renderTable({
                
                validate(need(input$pas_select != "", ""))
                
                pat <- get_pat(selector = TRUE)
                
                pas <- get_pas()
                
                community <- 
                    pas$communityRegion[which(pas$label == pat$meta$label)]
                
                meta <-
                    dplyr::tibble(
                        "Community" = community,
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
                    pat <- get_pat(selector = TRUE)
                    
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
                    
                    pat <- get_pat(selector = TRUE)
                    write.csv(pat$data, file = file)
                    
                }
                
            ) 
        
        # Leaflet selection label
        output$selected_label <- 
            shiny::renderTable({
                
                validate(need(input$leaflet_marker_click != "", ""))
                
                dplyr::tibble(
                    "Sensor" = input$leaflet_marker_click[1], 
                    "Latitude" = input$leaflet_marker_click[3],
                    "Longitude" = input$leaflet_marker_click[4]
                )
                
            })

    }
    
)
