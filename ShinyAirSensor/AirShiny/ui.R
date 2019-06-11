#
# This is the user-interface definition of AirShiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about Shiny apps here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science

pas <- example_pas[which(stringr::str_detect(example_pas$label, "SCNP")),] 
pas_labels <- 
    pas$label[-which(stringr::str_detect(pas$label, " B"))]

# Define UI for application 
shiny::shinyUI(
    shiny::navbarPage(
        
        # ----- Nav Bar --------------------------------------------------------
        title = "AirShiny (Beta)",
        shiny::tabPanel("Interactive Map"),
        shiny::tabPanel("Data Explorer"),
        
        shiny::fluidRow(
        #### ----- L Column  ---------------------------------------------------
            shiny::column(
                width = 2,
                
                # Community Selection input
                shiny::selectInput(
                    inputId = "comm_select", 
                    label = "Community Selection", 
                    choices = c("SCNP") # TODO: Implement other communities 
                ), 
                
                # DISABLED
                # # PAS selection input
                # shiny::selectInput(
                #     inputId = "pas_select", 
                #     label = "Purple Air Sensors:",
                #     choices = pas_labels
                # ),
                
                # Leaflet Selection input
                shiny::selectInput(
                    inputId = "leaflet_select", 
                    label = "Map type:", 
                    choices = c("Current PM2.5" = "pm25_current",
                                "1 hour PM2.5" = "pm25_1hr",
                                "1 day PM2.5" = "pm25_1day",
                                "1 week PM2.5" = "pm25_1week",
                                "Humidity" = "humidity", 
                                "Pressure" = "pressure", 
                                "Temperature" = "temperature")
                ),
                
                shiny::dateRangeInput(
                    inputId = "date_range", 
                    label = "Date Range:", 
                    min = "2017-01-01", 
                    start = "2019-04-01", 
                    end = "2019-04-10"
                ),
                
                #shiny::plotOutput(outputId = "pm25_daily_plot")
                
                # Plot type selection
                shiny::selectInput(
                    inputId = "plot_type_select", 
                    label = "Plot Selection:", 
                    choices = c("Daily Average" = "daily_plot",
                                "Interactive PM2.5 Plot" = "dygraph_plot",
                                "Multi-sensor Raw Data" = "multi_plot",
                                "Raw PM2.5 Data" = "raw_plot")
                
                )
                
            ), 
            
            shiny::column(
            ####----- R Column -------------------------------------------------
                width = 10,
                # Plot outputs
                leaflet::leafletOutput(
                    outputId = "leaflet", height = 500
                ), 
                
                # # Debug text
                # shiny::textOutput("test"),
                
                # Selected Plot
                shiny::plotOutput(outputId = "selected_plot")
                
                # Dygraph (?)
                #dygraphs::dygraphOutput(outputId = "dygraph_selected")
                
            )
            
        )
    
    )

)
