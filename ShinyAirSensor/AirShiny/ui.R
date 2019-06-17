#
# This is the user-interface definition of AirShiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about Shiny apps here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
library(AirSensor)
library(rlang)
library(magrittr)
# ---- Debug 
pas <- AirSensor::pas_load()
pas_community <- unique(pas$communityRegion) %>% na.omit()
# ----

# Define UI for application 
shiny::shinyUI(
    shiny::navbarPage(
        
        # ----- Nav Bar --------------------------------------------------------
        title = "AirShiny (Beta)", 
        
        ## ---- Tab 1 ----------------------------------------------------------
        shiny::tabPanel(
            
            title = "Interactive Map",
            
            shiny::fluidRow(
                
                #### ----- L Column  -------------------------------------------
                shiny::column(
                    
                    width = 2,
                    
                    # Community Selection input
                    shiny::selectInput(
                        inputId = "comm_select", 
                        label = "Community Selection", 
                        choices = pas_community 
                    ), 
                    
                    # PAS selection input
                    shiny::selectInput(
                        inputId = "pas_select",
                        label = "Purple Air Sensor:", 
                        choices = ""
                    ),
                    
                    # End Date input 
                    shiny::dateInput(
                        inputId = "date_selection",
                        label = "Date:"),
                    
                    # Lookback interval
                    shiny::radioButtons(
                        inputId = "date_interval", 
                        label = "Look back", 
                        choices = c("3 Days" = 3,
                                    "7 Days" = 7,
                                    "30 Days" = 30)
                    ),
                    
                    # Plot type selection
                    shiny::selectInput(
                        inputId = "plot_type_select", 
                        label = "Plot Selection:", 
                        choices = c("Hourly Average" = "hourly_plot",
                                    "Daily Average" = "daily_plot",
                                    "Multi-sensor Raw Data" = "multi_plot")
                    )
                    
                ), 
                
                ####----- R Column ---------------------------------------------
                shiny::column(
                    
                    width = 10,
                    
                    # Plot outputs
                    leaflet::leafletOutput(
                        outputId = "leaflet", height = 500
                    ), 
                    
                    # # Debug text
                    # shiny::textOutput("test"),
                    
                    # Selected Plot
                    shiny::plotOutput(
                        outputId = "selected_plot"
                    )
                    
                )
                
            )
            
        ),
        
        ## ----- Tab 2 ---------------------------------------------------------
        
        shiny::tabPanel(
            
            title = "Data Explorer",
            
            # Meta explorer
            shiny::column( 
                width = 11,
                shiny::tableOutput(
                    outputId = "meta_explorer"
                )
            ),
            
            # Download Button
            shiny::column( 
                width = 1,
                shiny::downloadButton(
                    outputId = "download_data"
                )
            ),
            
            # Data explorer
            shiny::dataTableOutput(
                outputId = "data_explorer"
            )
            
        )
        
    )
    
)
