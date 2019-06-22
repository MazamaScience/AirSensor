#
# This is the user-interface definition of AirShiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about Shiny apps here:
#
#    http://shiny.rstudio.com/
#
# - Mazama Science
#

logger.debug("----- ui() -----")

# ----- Define UI --------------------------------------------------------------
shiny::shinyUI(
    shiny::navbarPage(
        
        # ----- Nav Bar --------------------------------------------------------
        title = "AirShiny (Beta)",
        theme = shinythemes::shinytheme("lumen"),
        inverse = TRUE, 
        
        # ----- Tab 1 ----------------------------------------------------------
        shiny::tabPanel(
            
            title = "Interactive Map",
            
            shiny::fluidRow(
                
                # ----- L Column -----------------------------------------------
                shiny::column(
                    
                    width = 2,
                    
                    # Community Selection input
                    shiny::selectInput(
                        inputId = "comm_select", 
                        label = "Community Selection", 
                        choices = c("All..." = "all", 
                                    PAS_COMM) 
                    ), 
                    
                    # End Date input 
                    shiny::dateInput(
                        inputId = "date_selection",
                        label = "Date:"
                    ),
                    
                    # Lookback interval
                    shiny::radioButtons(
                        inputId = "lookback_days", 
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
                    ),
                    
                    # Display leaflet selection
                    shiny::tableOutput(outputId = "selected_label")
                    
                ), 
                
                #----- R Column ------------------------------------------------
                shiny::column(
                    
                    width = 10,
                    
                    # Plot outputs
                    leaflet::leafletOutput(
                        outputId = "leaflet", height = 550
                    ), 

                    # Selected Plot
                    shiny::plotOutput(
                        outputId = "selected_plot", height = 270
                    )
                    
                )
                
            )
            
        ),
        
        # ----- Tab 2 ----------------------------------------------------------
        
        shiny::tabPanel(
            
            title = "Data Explorer",
            
            # PAS selection input
            shiny::column(
                width = 2,
                shiny::selectInput(
                    inputId = "pas_select",
                    label = "Purple Air Sensor:", 
                    choices = ""
                )
            ),
            
            # Meta explorer
            shiny::column( 
                width = 9,
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
            
        ), 
        
        # ----- Tab 3 ----------------------------------------------------------
        
        shiny::tabPanel(
            
            title = "About"
        
            # TODO: Get an about section.  
            
        )
        
    )
    
)
