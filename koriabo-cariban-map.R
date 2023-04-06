library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.extras)
library(shiny)
library(sp)
library(rgdal)
library(magrittr)
library(htmlwidgets)




# Loadding required shapefiles
koriaboPottery <- sf::read_sf("data_raw/koriabo.shp", crs = 4326)             # Shapefile for the distribution of Koriabo pottery
caribanLanguages <- sf::read_sf("data_raw/cariban_languages.shp", crs = 4326) # Shapefile for the distribution of Cariban Languages
ethnomies <- sf::read_sf("data_raw/group_ethnomies.shp", crs = 4326)          # Shapefile for group ethnomies base on Nimuendaju's map
water <- sf::read_sf("data_raw/sa_water.shp", crs = 4326)                     # Shapefile of Hydrography
world <- rnaturalearth::ne_countries(scale = 'medium', type = 'map_units', returnclass = 'sf') # Loading world layer vector
dated_sites <- read.csv("data_raw/dated_sites.csv") %>%                       #Dates sites with Koriabo Pottery
  st_as_sf(
    coords = c(2, 3),
    crs = 4326
  )

#Setting up an input for chossing different options of display
selectInput("country", "Choose a country to filter the dated sites",
                          choices = c(
                            "Select All",
                            "Guyana",
                            "Suriname",
                            "French Guiana",
                            "Brazil (Amapá)",
                            "Brazil (Pará)"),
                          selected = "Select All")


#Setting the classes for displaying in the map
Country <- reactive({
  switch(input$country,
         "Select All" = dated_sites,
         "Guyana" = dated_sites$country[subset(dated_sites, country == "Guyana")],
         "Suriname" = dated_sites$country[subset(dated_sites, country == "Suriname")],
         "French Guiana" = dated_sites$country[subset(dated_sites, country == "French Guiana)")],
         "Brazil (Amapá)" = dated_sites$country[subset(dated_sites, country == "Brazil (Amapa)")],
         "Brazil (Para)" = dated_sites$country[subset(dated_sites, country == "Brazil (Para)")]
  )
})

mapPopup1 <- paste(
  "Ethnonym:", ethnomies$Etnomio, "<br>",
  "Early Register:", ethnomies$early_date, "<br>",
  "Late Register:", ethnomies$late_date, "<br>")

mapPopup2 <- paste(
  "Site:", "<br>",
  dated_sites$site, "<br>",
  "Oldest Date:", "<br>",
  dated_sites$c14oldest_date_bp, "+/-", dated_sites$standard_dev, "BP", "<br>",
  "Ocupation Interval:", "<br>",
  "BC/AD", dated_sites$cal_early, "to", dated_sites$cal_late, "<br>")


ui <- fluidPage(
  leafletOutput(outputId = "koriaboMap")
)

server <- function(input, output){
  output$koriaboMap <- renderLeaflet() %>% 
    leaflet() %>% 
    setView(lng = -60, lat = 2, zoom = 4.5) %>% 
    
    #Basemap layers and Minimap
    addProviderTiles(providers$Esri.WorldTerrain, group = "ESRI Terrain") %>%
    addProviderTiles(providers$OpenTopoMap, group = "Open Topographical") %>%
    addTiles(group = "Open Street Map") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "ESRI Imagery") %>%
    addMiniMap(position = "bottomleft") %>%
    
    
    #Layers Cariban/Koriabo
    addPolygons(data = water, color = "lightblue", stroke = FALSE, opacity = 1, fillOpacity = 1, group = "Main Rivers") %>% 
    
    addPolygons(data = caribanLanguages, color = alpha("red", 0.3), opacity = 1, group = "Cariban Languages") %>% 
    
    addPolygons(data = ethnomies, color = "darkgrey", opacity = 1, highlight = highlightOptions(color = "yellow", fillOpacity = .9, bringToFront = FALSE ), group = "Ethnomies", popup = mapPopup1) %>% 
    
    addPolygons(data = koriaboPottery, color = "blue", group = "Koriabo Pottery") %>%
    
    addCircles(data = dated_sites,
               lat = ~lat,
               lng = ~long,
               weight = 1,
               radius = ~c14oldest_date_bp*15,
               color = "black",
               fillColor = "black",
               highlight = highlightOptions(color = "yellow", fillOpacity = .9, bringToFront = TRUE),
               popup = mapPopup2,
               group = "Dated Sites",
               opacity = 2) %>%
    
    #Layers Control
    addLayersControl(
      position = "topright",
      baseGroups = c("ESRI Terrain", "Open Topographical", "ESRI Imagery", "Open Street Map"),
      overlayGroups = c("Cariban Languages", "Ethnomies", "Koriabo Pottery", "Dated Sites", "Main Rivers"),
      options = layersControlOptions(collapsed = FALSE)
    ) %>% 
    
    #Legends
    addLegend(
      position = "topright",
      values = c(caribanLanguages, ethnomies, koriaboPottery, dated_sites),
      colors = c("red", "darkgrey", "blue", "black"),
      labels = c("Cariban Languages", "Ethnomies", "Koriabo Pottery", "Dated Sites")) %>% 
    
    #Measure widget
    addMeasure(primaryLengthUnit = "kilometers", primaryAreaUnit = "kilometers")
}

shinyApp(ui, server)

#saveWidget(widget = koriaboMap, file = "C:/Users/Bruno/OneDrive/Documentos/koriabo-map/index.html")

