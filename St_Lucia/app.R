
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
                                                 Population)) |> filter(GROUP!= "100+") |>filter(GROUP!="TOTAL") |> filter(GROUP!= "5 - 9")

#Setting up economic Growth Data:
caricom_growth<- read.csv("caricom_growth.csv") |> select(Year:Caribbean.small.states)
caricom_growth$Year<- ymd(caricom_growth$Year, truncated=2L)
colnames(caricom_growth)<- c("Year","Antigua and Barbuda", "The Bahamas","Barbados", "Belize",
                             "Dominica","Grenada", "Guyana","Haiti", "Jamaica",
                             "St.Kitts and Nevis", "St. Lucia", "St. Vincent and the Grenadines",
                             "Suriname", "Trinidad and Tobago", "CARICOM")

#For tourism data:
tourism<- read_excel("Tourism.xlsx")

#For Net Migration:
migration<- read_excel("Net_Migration.xlsx")
migration$date<- ymd(migration$date)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  

  navbarPage(id = "tabs",
             title = 'Saint Lucia - The effect of Tourism and Migration on Population Aging',
             
             #maps, general information:
             navbarMenu(title = "St Lucia",
                        tabPanel("About St. Lucia",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 h3("About St. Lucia"),
                                 p(),
                                 p(style="font-size:14pt","St. Lucia is an Island Nation in the South East Caribbean and part of
                                 the Windward Isles of the Lesser Antilles Chain. These include Dominica, Grenada, 
                                   Saint Lucia, Saint Vincent and the Grenadines and the French Overseas Territory 
                                   of Martinique. Saint Lucia is a constitutional monarchy with its own Prime Minister (currently Phillip J Pierre) 
                                   and with King Charles III as its monarch."),
                                 p(),
                                 p(style="font-size:14pt","Saint Lucia, like most of its island nation relatives and neighbors relies
                                   heavily on tourism. In 2018, tourism accounted for 48.6% of Saint Lucia's GDP. The country is known for
                                   its beaches, hiking, watersports, an annual Jazz Festival and its distinctive cuisine"),
                                 p(),
                                 p(style="font-size:14pt","Saint Lucia also has a rich history. 
                                   In the 1550's a pirate known as Jambe-du-Bois(wooden leg in french, his real name was Francois Le Clerc) made Pigeon Island(now artifically attached to the rest of Saint
                                   Lucia) his home and attacked Spanish ships. This was the same pirate who had attacked Santiago de Cuba which was then the most prosperous
                                   city in Cuba.The damage he did to that city and the sum of money he sailed away with meant that Havana soon replaced it as Cuba's largest, richest city. 
                                   These sorts of historical tales and forts and lighthouses left by European settlers make Saint Lucia unique in its touristic appeals
                                   even among the many other nearby Caribbean Islands."),
                                 p(),
                                 p(style="font-size:14pt","The dominant natural landmarks in Saint Lucia are the Pitons - two mountains created by volcaninc activity 16-18 million years ago.
                                   They have very distinctive shapes and in fact, you can see them in the background image. The fatter, shorter one is known as Gros Piton and the
                                   skinnier, taller mountain is called Petit Piton. The two mountains together form the shape we see in Saint Lucia's flag. The Mountains together are a World Heritage and are located on the 
                                    southwest coast of Saint Lucia in the district of Soufriere."),
                                 p(),
                                 p(style="font-size:14pt","Use the drop down menu under 'St Lucia' to look at maps of St. Lucia. You'll see the Pitons and the capital city of Castries marked out.You can use
                                 that map or the regional map and zoom out to see where Saint Lucia lies relative to the Caribbean Islands around it and where it lies around the world."),
                                 p(),
                                 h4("A few statistics about Saint Lucia Economy"),
                                 p(),p(),
                                 tableOutput("econStatistics"),
                                 p(),p(),
                                 h4("What this app looks at"),
                                 p(),
                                 p(style="font-size:14pt","This app looks generally at how Saint Lucia's population has been aging at quite a substantial rate over the last few years.
                                   It compares this to factors such as economic growth and migration in Saint Lucia and in other CARICOM countries. Finally, we shall
                                   perform a SWOT analysis based, to some extent on the other discussions and points we have made.")
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
                                 p(style="font-size:14pt","Saint Lucia is a SID or Small Island Developing Nation. The distinction is made from 
                                   developing nations purely because of the economic idisyncracies being an island nation brings with it.
                                   That said, I wanted to look at how the population of Saint Lucia has Aged."),
                                 p(),
                                 p(style="font-size:14pt","Characteristic of developing nations in general is the shape of their population pyramid. These tend to 
                                   have large bases and taper to the top(there are simply, fewer older people relative to the whole population.
                                   As economies and countries age, these pyramids start to get fatter around the middle and if the pyramid inverts,
                                   you're in big trouble."),
                                 p(),
                                 p(style="font-size:14pt","Move the slider on the graph below to see how the shape of Saint Lucia's population pyramid has changed since 1991"),
                                 
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
                                 p(style="font-size:14pt","As you drag the slider above from left to right, you can see how the population
                                   pyramid of Saint Lucia goes from pyramid shaped to more 'kite' shaped. This is representative
                                   of a rapidly aging population. But what gives? Relative to other developing countries, this seems to
                                   be a very rapid shift in the age profile, happening over only 32 years. Countries like India, Indonesia and even,
                                   more comparably, Jamaica have not seen their age demographics shift to the same extent."),
                                 p(),
                                 p(style="font-size:14pt","Now obviously, with a country with a small population like Saint Lucia, any shift at all will have a larger 
                                   relative effect. However, it might also make sense that emigration and immigration patterns would affect the age 'shape' of such a country.
                                   Are Saint Lucia's young people leaving to find work elsewhere while aged retirees come and fall in love and settle down?
                                   How much of a problem is this for the island?"),
                                 p()
                        ),
                        tabPanel(title="Migration to and from Saint Lucia",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 p(style="font-size:14pt","Below is a graph of the net migration into Saint Lucia, overlaid with immigration. The two graphs together should
                                   be sufficient to allow us to gains ome idea of what we can do to identify and confirm the trends we suspect above."),
                                 plotOutput("migrationGraph"),
                                 p(),
                                 p(style="font-size:14pt","We can see that immigration into Saint Lucia has greatly increased, more so even than emigration might have decreased. This could account for 
                                   why Saint Lucia's population has aged. However, we can see that emigration has greatly increased too and since this might be overwhelmingly young people,
                                   this could pose serious questions for Saint Lucia's future - questions that even more developed nations such as Japan are struggling to face right now.")
                        )
                        
                        ),
             
             navbarMenu(title="Comparing Saint Lucia's Economy to that of its Neighbours",
                        tabPanel("Economic Growth:",
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 p(style="font-size:14pt","Here, I'll compare the economy of Saint Lucia to that of 
                          CARICOM (Caribbean Communities), the economic grouping to which it belongs. These include the countries that are part of the 
                          Lesser Antilles chain of islands - including the Windward and Leeward Islands - 
                          as well as Antigua and Barbuda, Jamaica, Haiti, The Bahamas and Barbados."),
                                 plotOutput("economicGrowth"),
                                 p(),
                                 p(style="font-size:14pt","We can see that the growth rate of Saint Lucia is pretty well correlated with that of its neighbors. 
                                   However, we can see that it was impacted much worse by COVID than its neighbors were on average but has recovered much much better too.
                                   This might be down to how much more heavily it depends on tourism for its GDP than other members of CARICOM. That would
                                   explain both why it suffered much more during Covid and why, in the travel boom that followd it recovered much faster.")

                             
                        
                        ),
                        tabPanel(title="Tourism as a percent of GDP",
                                 p(),
                                 img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                 p(style="font-size:14pt","We compare Saint Lucia's dependence on Tourism, as measured by its percent contribution to GDP
                                   with that of other Caribbean countries, not all of whom are in CARICOM. All the same, there are only two countries on the chart below who are
                                   more dependent economically on tourism. High dependence on tourism might reinforce that tourists will settle into the country but it
                                   also indicates why young people might be leaving. As evidenced by the Gini Coefficient we cited in the first tab of this Shiny App, 
                                   there is a lot of income inequality in Saint Lucia. This is because most tourim jobs are generally low paying and traditional roles and customs combined
                                   with that factor can often drive young people to seek employment outside of their home countries. "),
                                 plotOutput("tourismGraph")
                                 )
                                
             
               ),
             
             navbarMenu(title="SWOT Analysis",
                       tabPanel(title = "Strengths",
                                img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                       p(style="font-size:14pt","Saint Lucia's strengths lie in its unique culture, cuisine and its clear appeal to tourists and potential settlers as evidenced
                                         by its rebounding growth after COVID and the change in net migration over the years. It also has a larger share of agricultural as a percentage
                                         of GDP than some of its other neighbors and is better positioned to take advantage of that.")
                         ),
                         tabPanel(title = "Weakness",
                                  img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                               p(style="font-size:14pt","Saint Lucia's weaknesses clearly lie in the competition its major economic product has with other countries in the region.
                                      This is exacerbated by its rapidly aging population, which is a problem which if left unsolved could lead to increasingly severe
                                      economic challenges. Not being able to keep its young people in and attracting aging people might stymie the countries economic growth in the future
                                 and impact a lot of its tourist business. Further, as evidenced in the GDP Growth rate chart, an overreliance on tourism leaves the country very vulnerable to 
                                 global events outside of its control such as pandemics and recessions with no other counter-cyclical industries to bear the brunt. This might further reinforce cycles of 
                                 young people leaving the country and more analysis to establish whether this is true could be very interesting indeed. ")
                               ),
                         tabPanel(title = "Opportunity",
                                  img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                               p(style="font-size:14pt","That Saint Lucia's net migration has gotten a lot better over the years and that its able to attract foreigners is a plus. 
                                        An opportunity may lie in expanding its other industries to entice its young people to stay and perhaps to entice young people
                                        from outside the country to come and stay too. Look at Bermuda for example, which in the aftermath of the pandemic was offering
                                        temporary residence permits to remote workers to come work there. Who wouldn't want to work from the beach of an island paradise 
                                        such as Saint Lucia?")
                               ),
                         tabPanel( title = "Threats",
                                   img(src="St-Lucia.jpeg", style = "width:100%; position: absolute; opacity: 0.2;"),
                                p(style="font-size:14pt","This is the big one. There could be no greater threat to any island nation than the rising sea levels brought on
                                         by climate change. If the rest of the world doesn't get its act together we could see the beautiful coastlines of Saint Lucia
                                         underwater in just a couple of generations. At that point, it might not matter whether or not Saint Lucia is able to halt an exodus of its young people,
                                         in fact it might even be desirable to encourage it."))
               
             ),
             
             navbarMenu(title="Bibliography",
                        align="right",
                        tabPanel(tags$a(href="https://en.wikipedia.org/wiki/Saint_Lucia",
                                        "Wikipedia/St.Lucia")),
                        tabPanel(tags$a(href="https://www.shinyapps.io/",
                                        "shinyapps.io for publishing")),
                        
                        
                        tabPanel(tags$a(href="https://www.njtierney.com/post/2022/08/09/ggplot-pyramid/")),
                        
                        tabPanel(tags$a(href = "https://data.worldbank.org/indicator/SM.POP.NETM?display=gr&locations=LC","net migration data")),
                        tabPanel(tags$a(href="https://www.statista.com/statistics/789517/caribbean-direct-contribution-travel-tourism-gdp-country/","Tourism and GDP")),
                        tabPanel(tags$a(href="https://rstudio.github.io/leaflet/",
                                        "Leaflet doc")),
                        tabPanel(tags$a(href="https://www.census.gov/programs-surveys/international-programs/about/idb.html","Population data"))


)
)
)


# Define server logic 
server <- function(input, output) {
  
    output$image<- renderUI({
      tags$img(src="lc-flag.jpeg", width = 200 )
    })
    
    output$stLucia_map <- renderLeaflet({
        leaflet() |> 
        addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 11) |> 
        addMarkers(lng=-61.00614, lat=113.9957, 
                   popup= "Castries") |> addMarkers(lng=-61.0666664,lat=13.7999968, popup = "Gros Piton") |> 
        addMarkers(lng=-61.0666664, lat = 13.83333 , popup = "Petit Piton")
      
    })
    
    output$econStatistics<- renderTable({
      data.frame(Statistic=c("Population", "GDP","HDI", "Gini Coefficient"),Value = c("179,651","$3.452 billion", "0.751 (high)", "0.51 (high)"))
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
                          values = c('#581845','#FFC300'))+xlab("Age Groups")+
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
    
    output$tourismGraph<- renderPlot({
      ggplot(tourism,aes(y=Country, x=`Percent GDP Spent on Tourism`))+
        geom_bar(stat="identity")+
        theme(axis.text.x = element_text(angle = 45, hjust=1))+
        ggtitle("Caribbean Countries and their Economic Dependency on Tourism")
    })
    
    output$migrationGraph <- renderPlot({
      ggplot(migration, aes(x=date))+
        geom_line(aes(y=`Migrant Population`, color = 'Immigration'))+
        geom_line(aes(y= `Net Migration`, color = 'Net Migration'))+
        ggtitle("Immigration and Emigration in Saint Lucia")+
        xlab("Year")+ylab('Number of Migrants')+
        scale_color_manual(name="Migration Type", values = c('#581845','red'))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
