library(leaflet)

stLucia_map<- leaflet() |> 
              addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 9.75) |> 
                addMarkers(lng=-60.966667, lat=13.883333)
stLucia_map


region_st_lucia_map<- leaflet() |> 
  addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 7) |> 
  addMarkers(lng=-60.966667, lat=13.883333, 
             popup= "St. Lucia")

region_st_lucia_map


caribbean_st_lucia_map<- leaflet() |> 
  addTiles() |> setView(lng=-60.966667, lat=13.883333, zoom = 4) |> 
  addMarkers(lng=-60.966667, lat=13.883333, 
             popup= "St. Lucia")

caribbean_st_lucia_map
