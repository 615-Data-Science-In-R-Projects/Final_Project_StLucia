
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(shinypanels)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(leaflet)
library(readxl)


#stlucia_map_data <- read_excel("stlucia_map_data.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  setBackgroundImage(
    src = "St-Lucia.jpeg"
  ),

  navbarPage(id = "tabs",
             title = "St. Lucia",
             
             #maps, general information:
             navbarMenu(title = "St Lucia",
                        tabPanel("About St. Lucia",
                                 p("St. Lucia is an Island Nation in the South East Caribbean and part of
                                 the Windward Isles of the Lesser Antilles Chain. These include Dominica, Grenada, 
                                   Saint Lucia, Saint Vincent and the Grenadines and the French Overseas Territory 
                                   of Martinique"),
                                 p(),
                                 p("Saint Lucia, like most of its island nation relatives and neighbors relies
                                   heavily on ")
                                 
                                 ),
                        tabPanel("A Map of St. Lucia",
                                 leafletOutput("stLucia_map", height = 900)
                                 
                        ),
                        
                        tabPanel("St Lucia in the South Eastern Caribbean",
                                 leafletOutput("region_st_lucia_map", height = 900))
             )
             
             
  )

)

# Define server logic 
server <- function(input, output) {

    output$stLucia_map <- renderLeaflet({
        leaflet() |> 
        addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 11) |> 
        addMarkers(lng=-60.966667, lat=13.883333)
      
    })
    
    output$region_st_lucia_map<- renderLeaflet({
      leaflet() |> 
      addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 7) |> 
      addMarkers(lng=-60.966667, lat=13.883333, 
                 popup= "St. Lucia")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
