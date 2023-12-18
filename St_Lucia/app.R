
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
  
  

  navbarPage(id = "tabs",
             titlePanel(title = span(img(src="lc-flag.jpeg",height=30, width =40, style = "float:left;")), 'Saint Lucia'),
             
             #maps, general information:
             navbarMenu(title = "St Lucia",
                        tabPanel("About St. Lucia",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 p(style="font-size:14pt","St. Lucia is an Island Nation in the South East Caribbean and part of
                                 the Windward Isles of the Lesser Antilles Chain. These include Dominica, Grenada, 
                                   Saint Lucia, Saint Vincent and the Grenadines and the French Overseas Territory 
                                   of Martinique"),
                                 p(),
                                 p(style="font-size:14pt","Saint Lucia, like most of its island nation relatives and neighbors relies
                                   heavily on ")
                                 
                                 ),
                        tabPanel("A Map of St. Lucia",
                                 leafletOutput("stLucia_map", height = 900)
                                 
                        ),
                        
                        tabPanel("St Lucia in the South Eastern Caribbean",
                                 leafletOutput("region_st_lucia_map", height = 900))
             ),
             
             navbarMenu(title = "Demographics",
                        tabPanel("How has Saint Lucia's Population Aged?",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 p("Saint Lucia is a SID or Small Island Developing Nation. The distinction is made from 
                                   developing nations purely because of the economic idisyncracies being an island nation brings with it.
                                   That said, I wanted to look at how the population of Saint Lucia has Aged."),
                                 p(),
                                 p("Characteristic of developing nations in general is the shape of their population pyramid. These tend to 
                                   have large bases and taper to the top(there are simply, fewer older people relative to the whole population.
                                   As economies and countries age, these pyramids start to get fatter around the middle and if the pyramid inverts,
                                   you're in big trouble."),
                                 p(),
                                 p("Move the slider on the graph below to see how the shape of Saint Lucia's population pyramid has changed since 1991"),
                                 
                                 sidebarLayout(
                                   
                                   sidebarPanel(
                                     sliderInput("Year",
                                                 "Year",
                                                 min = 1991,
                                                 max = 2023,
                                                 value = 2023
                                       
                                     )
                                    
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("pyramidPlot")
                                   )
                                 )
                              
                                 )
                        
                        
                        ),
             
             navbarMenu(title="Comparing Saint Lucia's Economy to that of its Neighbours",
                        
                        tabPanel("Introduction",
                          img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                          p(),
                          p(style="font-size:14pt","Here, I'll compare the economy of Saint Lucia to that of other full members of 
                          CARICOM (Caribbean Communities). These include the countries that are part of the 
                          Lesser Antilles chain of islands - including the Windward and Leeward Islands - 
                          as well as Antigua and Barbuda, Jamaica, Haiti, The Bahamas and Barbados."),
                        ),
                        tabPanel("Economic Growth:",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;")
                                
                        
                        )
             
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
