#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define UI for application 
shiny::shinyUI(
    shiny::navbarPage(
        title = "AirShiny",

     shiny::fluidRow(
        shiny::column(
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
            ),
            
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
            shiny::plotOutput(outputId = "pm25_plot")
            

        ), 
        
        shiny::column(
            width = 10,
            # Plot outputs
            
            leaflet::leafletOutput(
                outputId = "leaflet" 
            ), 
            
            shiny::textOutput("test")
            
        )
        
    )
))
