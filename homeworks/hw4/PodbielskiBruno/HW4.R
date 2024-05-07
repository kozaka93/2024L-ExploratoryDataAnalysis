library(dplyr)
library(leaflet)

power_plants <- read.csv("power_plants.csv") %>%
  select(2:7) %>%
  filter(primary_fuel == "Nuclear")
colnames(power_plants)[c(2,3)] <- c("name", "capa_in_MW")

labels <- paste("<strong>", power_plants$name, "</strong><br>",
                "Country:", power_plants$country_long, "<br>",
                "Capacity:", power_plants$capa_in_MW, "MW <br>") %>% lapply(htmltools::HTML)

pts <- leaflet(power_plants, options = leafletOptions(minZoom=2)) %>%
  addTiles() %>%
  addControl(html="<p style='font-size:20px'> Nuclear power plants (figures for 2021) </p>", position = "topright") %>%
  addCircles(lng = ~longitude,
             lat = ~latitude,
             color = "red",
             opacity = 0.5,
             weight = 6,
             highlightOptions = highlightOptions(
                color = "blue",
                weight = 10,
                bringToFront = TRUE),
             label = labels,
             labelOptions = labelOptions(
                 style = list("font-weight" = "normal", padding = "3px 8px"),
                 textsize = "15px",
                 direction = "auto")) %>%
  setMaxBounds(lng1=-180, lat1=80, lng2=180, lat2=-80)
pts