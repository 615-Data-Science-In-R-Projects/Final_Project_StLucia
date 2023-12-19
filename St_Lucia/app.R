
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
library(scales)
library(ggplot2)
library(tidyverse)
library(lubridate)

#Setting up data for population pyramids
pop_by_age<- read.csv("IDB_12-18-2023.csv") |> select(Year, GROUP,
                                                      Male.Population,
                                                      Female.Population,
)

pyramid_frame<- pop_by_age |> pivot_longer(cols = c(Male.Population,Female.Population),names_to = "Gender", values_to="Population") 

pyramid_frame$Gender <- ifelse(pyramid_frame$Gender=="Male.Population", "M","F")

pyramid_frame$Year<- ymd(pyramid_frame$Year, truncated=2L)
pyramid_frame<- pyramid_frame |> mutate(Population=
                                          ifelse(Gender=="M", Population*(-1),
                                                 Population)) |> filter(GROUP!= "100+") |>filter(GROUP!="TOTAL")

#Setting up economic Growth Data:
caricom_growth<- read.csv("caricom_growth.csv") |> select(Year:Caribbean.small.states)
caricom_growth$Year<- ymd(caricom_growth$Year, truncated=2L)
colnames(caricom_growth)<- c("Year","Antigua and Barbuda", "The Bahamas","Barbados", "Belize",
                             "Dominica","Grenada", "Guyana","Haiti", "Jamaica",
                             "St.Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines",
                             "Suriname", "Trinidad and Tobago", "CARICOM")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  

  navbarPage(id = "tabs",
             titlePanel(title = span(img(src="lc-flag.jpeg",height=30, width =40, style = "float:left;")), 'Saint Lucia - The effect of Tourism and Migration on the aging of an small Island Nation'),
             
             #maps, general information:
             navbarMenu(title = "St Lucia",
                        tabPanel("About St. Lucia",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 h3("About St. Lucia"),
                                 p(),
                                 p(style="font-size:14pt","St. Lucia is an Island Nation in the South East Caribbean and part of
                                 the Windward Isles of the Lesser Antilles Chain. These include Dominica, Grenada, 
                                   Saint Lucia, Saint Vincent and the Grenadines and the French Overseas Territory 
                                   of Martinique."),
                                 p(),
                                 p(style="font-size:14pt","Saint Lucia, like most of its island nation relatives and neighbors relies
                                   heavily on tourism. In 2018, tourism accounted for 65% of Saint Lucia's GDP. The country is known for
                                   its beaches, hiking, watersports, an annual Jazz Festival and its own cuisine"),
                                 p(),
                                 p(style="font-size:14pt","Saint Lucia also has a rich history. 
                                   In the 1550's a pirate known as Jambe-du-Bois(wooden leg in french, his real name was Francois Le Clerc) made Pigeon Island(now artifically attached to the rest of Saint
                                   Lucia) his home and attacked Spanish ships. This was the same pirate who had attacked Santiago de Cuba which was then the most prosperous
                                   city in Cuba.The damage he did to that city and the sum of money he sailed away with meant that Havana soon replaced it as Cuba's largest, richest city. 
                                   These sorts of historical tales and forts and lighthouses left by European settlers make Saint Lucia unique in its touristic appeals
                                   even among the many other nearby Caribbean Islands."),
                                 p(),
                                 p(style="font-size:14pt","The dominant natural landmarks in Saint Lucia are the pitons - two Mountains created by volcaninc activity 16-18 million years ago.
                                   They have very distinctive shapes and in fact, you can see them in the background image. The fatter, shorter one is known as Gros Piton and the
                                   skinnier, taller mountain is called Petit Piton. The two mountains together form the shape we see in Saint Lucia's flag."),
                                 p(),
                                 p(style="font-size:14pt","Use the drop down menu under 'St Lucia' to look at maps of St. Lucia. You'll see the Pitons and the capital city of Castries marked out.You can use
                                 that map or the regional map and zoom out to see where Saint Lucia lies relative to the Caribbean Islands around it and where it lies around the world.")
                                 
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
                                                 min = year(min(pyramid_frame$Year)),
                                                 max = year(max(pyramid_frame$Year)),
                                                 value = year(max(pyramid_frame$Year)),
                                                 sep=""
                                       
                                     )
                                    
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("pyramidPlot")
                                   )
                                 ),
                                 
                                 p(),
                                 p("As you drag the slider above from left to right, you can see how the population
                                   pyramid of Saint Lucia goes from pyramid shaped to more 'kite' shaped. This is representative
                                   of a rapidly aging population. But what gives? Relative to other developing countries, this seems to
                                   be a very rapid shift in the age profile, happening over only 32 years. Countries like India, Indonesia and even,
                                   more comparably, Jamaica have not seen their age demographics shift to the same extent."),
                                 p(),
                                 p("Now obviously, with a country with a small population like Saint Lucia, any shift at all will have a larger 
                                   relative effect. ")
                              
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
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 plotOutput("economicGrowth")
                                
                        
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
    
    output$pyramidPlot<- renderPlot({
      year<-ymd(input$Year, truncated=2L)
      
      range_pop<- range(pyramid_frame$Population)
      breaks_pop<- pretty(range_pop, n=7)
      
      pyramid_frame<-pyramid_frame |> filter(Year==year)
      
      ggplot(data=pyramid_frame, aes(x=GROUP, y = Population, fill=Gender))+
        geom_bar(stat="identity")+coord_flip()+
        scale_fill_manual(name = "Gender",labels = c("Male", "Female"),
                          values = c('#581845','#FFC300'))+
        scale_y_continuous(breaks  = breaks_pop,
                           labels = abs(breaks_pop))
    })
    
    output$economicGrowth<- renderPlot({
      ggplot(caricom_growth, aes(x=Year))+
        geom_line(aes(y=`St. Lucia`, color = 'St. Lucia'))+
        geom_line(aes(y= CARICOM, color = 'CARICOM'))+
        ggtitle("Economic Growth of Saint Lucia relative to rest of CARICOM")+
        xlab("Year")+ylab('% Change in GDP')+
        scale_color_manual(name="Country", values = c('#581845','#FFC300') )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
