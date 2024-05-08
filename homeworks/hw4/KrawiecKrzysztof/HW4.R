library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)
library(readxl)
library(leaflet)
library(tidyverse)
library(broom)
library(geojsonio)

wojewodztwa <- geojson_read("wojewodztwa-medium.geojson", what = "sp")
populacja_wojewodztwa_2023 <- read_excel("demografia2023.xlsx", sheet = "demografia2023")

populacja_wojewodztwa_2023 %>% 
  mutate(Ogółem = Ogółem * 1000,
         gestosc = round(Ogółem/`Powierzchnia w km^2`),
         gestosc_mezczyzn = round(Mężczyźni * 1000 / `Powierzchnia w km^2`),
         gestosc_kobiet = round(Kobiety * 1000 / `Powierzchnia w km^2`))-> populacja_wojewodztwa_2023

data.frame(wojewodztwa$nazwa) -> nazwy_wojewodztw

full_join(nazwy_wojewodztw, populacja_wojewodztwa_2023, 
          by = c("wojewodztwa.nazwa" = "Województwo")) -> populacja_wojewodztwa_2023

mean(populacja_wojewodztwa_2023$gestosc)

bins <- c(50, 100, 120, 150, 200, 350, 400)
pal <- colorBin("YlOrRd", domain = populacja_wojewodztwa_2023$gestosc, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>Gęstość zaludnienia ogółem: %g os/m<sup>2</sup><br/>Gęstość zaludnienia kobiet: %g os/m<sup>2</sup><br/>Gęstość zaludnienia mężczyzn: %g os/m<sup>2</sup>",
  wojewodztwa$nazwa, populacja_wojewodztwa_2023$gestosc, populacja_wojewodztwa_2023$gestosc_kobiet, populacja_wojewodztwa_2023$gestosc_mezczyzn
) %>% lapply(htmltools::HTML)

leaflet(wojewodztwa) %>% 
  setView(18.96020, 52, 6.25) %>% 
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(populacja_wojewodztwa_2023$gestosc),
    stroke = TRUE,
    color = "white",
    weight = 1.5,
    opacity = 0.8,
    fill = TRUE,
    dashArray = "1",
    fillOpacity = 0.7,
    smoothFactor = 1.5,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "gray",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>% 
    addLegend(pal = pal, values = ~populacja_wojewodztwa_2023$gestosc, 
              opacity = 0.7, 
              title = "Gęstość zaludnienia",
              position = "bottomleft")


