forest_area_km <- read.csv("forest_area_km.csv")
forest_area_percent <- read.csv("forest_area_percent.csv")

library(dplyr)
library(maps)
library(leaflet)
library(geojsonio)
library(htmltools)

forest_area_km <- forest_area_km %>% 
  select(Country.Name, Country.Code, X2021)
colnames(forest_area_km) <- c("Country.Name", "Country.Code", "km")

forest_area_percent <- forest_area_percent %>% 
  select(Country.Name, Country.Code, X2021)
forest_area_percent <- forest_area_percent %>% 
  inner_join(forest_area_km, join_by("Country.Code")) %>% 
  select(Country.Name.x, Country.Code, X2021, km)

world <-
  geojson_read(
    "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp"
  )

world@data <- world@data %>%
  left_join(forest_area_percent, join_by("id" == "Country.Code"))

bins <- c(0,5,10,20,30,40,50,60,70,100)
pal <- colorBin(c(
  "#ccfccf",
  "#b5f7b8",
  "#91ed95",
  "#54d15a",
  "#36ad3c",
  "#208a25",
  "#117016",
  "#074a0a",
  "#002902"
), domain = world$X2021, bins = bins, na.color = NA)

labels <- sprintf(
  "<strong>%s</strong><br/>Forest area: <br/>%g<span>&#37;</span> <br/>(%g km<sup>2</sup>)",
  world$name, world$X2021, world$km
) %>% lapply(htmltools::HTML)


m <- leaflet(world) %>% 
  addTiles() %>%
  setView(0, 0, 1) %>% 
  addPolygons(
  fillColor = ~pal(X2021),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 5,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>% 
  addLegend(pal = pal, values = ~X2021, opacity = 0.7, title = "Percentage of forest area",
                position = "bottomleft") %>% 
  addControl(HTML(paste0("<h4> World forest cover in 2021 </h4>")), position = "bottomright")

m

