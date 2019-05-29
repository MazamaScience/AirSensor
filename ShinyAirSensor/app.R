# A Basic Shiny App For AirSensor R Package

# --- Debugging
pas <- example_pas[which(stringr::str_detect(example_pas$label, "SCNP")),] 
pas_labels <- 
    pas$label[-which(stringr::str_detect(pas$label, " B"))]
# ---

ui <- 
    shiny::fluidPage(
        # Leaflet Selection input
        shiny::selectInput(
            inputId = "leaflet_select", 
            label = "Map type:", 
            choices = c("Current PM2.5" = "pm25_current",
                        "30 Min. PM2.5" = "pm25_30min", 
                        "1 hour PM2.5" = "pm25_1hr",
                        "1 day PM2.5" = "pm25_1day",
                        "1 week PM2.5" = "pm25_1week",
                        "Humidity" = "humidity", 
                        "Pressure" = "pressure", 
                        "Temperature" = "temperature")
        ),
        
        leaflet::leafletOutput(
            outputId = "leaflet" 
        ),
        
        # PAS selection input
        shiny::selectInput(
            inputId = "pas_select", 
            label = "Purple Air Sensors:",
            choices = pas_labels
        ),
        
        # Meta table view output
        shiny::tableOutput(outputId = "pas_tbl"),
        
        # Date input 
        shiny::dateRangeInput(
            inputId = "dateRange",
            label = "Date Range:"
        ), 
        
        # Dygraph selection input 
        shiny::selectInput(
            inputId = "dygraph_select", 
            label = "Graph type:", 
            choices = c("PM2.5" = "pm25", 
                        "Temperature" = "temperature", 
                        "Humidity" = "humidity") 
            ),
        
        
        # Dygraph output
        dygraphs::dygraphOutput("dygraph"),
        
        
        # Plot selection input
        shiny::selectInput(
            inputId = "plot_select", 
            label = "Plot type:", 
            choices = c(
                        "Multiplot" = "pat_multiplot", 
                        "Outliers" = "pat_outliers"
                        )
        ),
        
        # Plot output
        shiny::plotOutput("plot")
        
    )

server <- 
    function(input, output) {
        
        # Table render
        output$pas_tbl <- 
            shiny::renderTable({
                
                pat <- 
                    AirSensor::pat_load(
                        label = input$pas_select, 
                        startdate = 20190411, 
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
        
        # Plot render
        output$plot <- 
            shiny::renderPlot({
                
                pat <- 
                    AirSensor::pat_load(
                        label = input$pas_select, 
                        startdate = input$dateRange[1],
                        enddate = input$dateRange[2]
                    )
                
                selected <- input$plot_select
                
                if ( selected == "pat_multiplot" ) {
                    AirSensor::pat_multiplot(pat)
                }
                if ( selected == "pat_outliers" ) {
                    AirSensor::pat_outliers(pat)
                }
                
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
        
        # Dygraph render 
        output$dygraph <- 
            dygraphs::renderDygraph({
                pat <- 
                    AirSensor::pat_load(
                        label = input$pas_select, 
                        startdate = input$dateRange[1],
                        enddate = input$dateRange[2]
                    )
                
                selected <- input$dygraph_select
                AirSensor::pat_dygraph(pat,parameter = selected)
                })
        
    }

shiny::shinyApp(ui = ui, server = server)