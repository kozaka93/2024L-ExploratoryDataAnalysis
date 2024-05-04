
library(dplyr)
library(ggplot2)
library(leaflet)
library(geojsonio)
library(sf)

# Przygotowanie danych ----------------------------------------------------

drinking_age <- read.csv("drinking-age-by-country-2024.csv", sep = ",")
alcohol_cons <- read.csv("alcohol-consumption-by-country-2024.csv", sep = ",")

alcohol <- alcohol_cons %>% 
  left_join(drinking_age) %>% 
  select(country, BothSexes, Males, Females, OnPremiseSale) %>% 
  mutate(country = ifelse(country == "United States", "United States of America", country)) %>% 
  mutate(country = ifelse(country == "Tanzania", "United Republic of Tanzania", country)) %>% 
  mutate(country = ifelse(country == "DR Congo", "Democratic Republic of the Congo", country)) %>% 
  mutate(country = ifelse(country == "North Macedonia", "Macedonia", country)) %>% 
  mutate(country = ifelse(country == "Serbia", "Republic of Serbia", country))
  
world <- geojsonio::geojson_read("countries.geojson", what = "sp")
# https://github.com/datasets/geo-countries/blob/master/data/countries.geojson


# Dołączam ramkę danych do pliku geojson za pomocą funkcji z pakietu sf
# Otrzymuje w efekcie nowy plik geojson

world_sf <- st_as_sf(world)
world2 <- left_join(world_sf, alcohol, by = c("ADMIN" = "country"))
st_write(world2, "merged_data.geojson")
world3 <- geojsonio::geojson_read("merged_data.geojson", what = "sp")



# Wizualizacja ------------------------------------------------------------

bins <- c(0, 4, 6, 8, 10, 12, 14)
pal <- colorBin("Reds", domain = world3$BothSexes, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>Alcohol consumption: %g l/capita<br/>Legal age for buying: %s",
  world2$ADMIN, world2$BothSexes, world3$OnPremiseSale
) %>% lapply(htmltools::HTML)


map <- leaflet(world3) %>% 
  addTiles() %>% 
  addPolygons(
  fillColor = ~pal(BothSexes),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "2", 
  fillOpacity = 0.7,
  highlightOptions = highlightOptions(
    weight = 3,
    color = "#666",
    dashArray = "",
    fillOpacity = 0.7,
    bringToFront = TRUE),
  label = labels,
  labelOptions = labelOptions(
    style = list("font-weight" = "normal", padding = "3px 8px"),
    textsize = "15px",
    direction = "auto")) %>% 
  addLegend(pal = pal, values = ~BothSexes, opacity = 0.7, title = NULL,
            position = "bottomright")
map  



