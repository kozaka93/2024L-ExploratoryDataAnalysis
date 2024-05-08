library(leaflet)
library(dplyr)
library(geojsonio)
library(sp)
library(htmltools)

homicide_data <- read.csv("homicide_data.csv")
names(homicide_data) <- unlist(homicide_data[1, ])
homicide_data <- homicide_data[-1, c(3,48)]
colnames(homicide_data) <- c("name", "rate")

countries <- geojsonio::geojson_read("custom.geo.json", what = "sp")
spdata <- merge(countries, homicide_data, by = "name", all.y = TRUE)

bins <- c(0, 10, 20, 50, 100, Inf)
pal <- colorBin("YlOrRd", domain = spdata$rate, bins = bins)


m <- leaflet(spdata) %>%
  setView(0, 30, 2) %>%
  addTiles()
m
m %>% addPolygons()

m %>% addPolygons(
  fillColor = ~pal(rate),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
m %>% addPolygons(
  fillColor = ~pal(rate),
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
    bringToFront = TRUE))

# Dodanie informacji, które będą się wyświetlać po najechaniu na stan.

labels <- sprintf(
  "<strong>%s</strong><br/>%g deaths / 100k",
  spdata$name, spdata$rate
) %>% lapply(htmltools::HTML)

m <- m %>% addPolygons(
  fillColor = ~pal(rate),
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
    direction = "auto"))
m

# Dodanie legendy 

m %>% addLegend(pal = pal, values = ~rate, opacity = 0.7, title = "Homicide Rate in 2021\n(per 100k inhabitants)",
                position = "bottomright")
