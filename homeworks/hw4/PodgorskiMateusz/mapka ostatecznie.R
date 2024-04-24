library(leaflet)
library(dplyr)
library(sf)
library(maps)
library(mapdata)

# Import danych dotyczących szczęścia
data_happiness <- read.csv("C:/Users/mateu/OneDrive/Dokumenty/R laby/hap_word_2019.csv", header = TRUE, sep = ",")

# Przygotowanie mapy świata
global_map <- st_as_sf(map("world", fill = TRUE, plot = FALSE))

# Modyfikacja i ujednolicenie nazw krajów
data_happiness <- data_happiness %>%
  rename(nation = Country.or.region) %>%
  mutate(nation = case_when(
    nation == "United States" ~ "USA",
    nation == "United Kingdom" ~ "UK",
    nation == "Congo (Brazzaville)" ~ "Democratic Republic of the Congo",
    nation == "Congo (Kinshasa)" ~ "Republic of Congo",
    TRUE ~ nation
  ))

# Łączenie danych z mapą świata
world_happiness <- global_map %>%
  left_join(data_happiness, by = c("ID" = "nation")) %>%
  st_transform(crs = 4326) %>% 
  rename(nation=ID) 

# Ustawienie palety kolorów
color_scheme <- colorNumeric(palette = "Blues", domain = world_happiness$Score)




labels <- sprintf("<strong>Country: </strong>%s<br><strong>Score: </strong>%g", world_happiness$nation, world_happiness$Score) %>% lapply(htmltools::HTML)

# Tworzenie mapy z danymi na temat szczęścia
happiness_map <- leaflet(world_happiness) %>%
  setView(lng = 0, lat = 30, zoom = 2) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~color_scheme(Score),
    fillOpacity = 0.8,
    color = "gray",
    weight = 1.5,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#555",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(style = list("font-weight" = "normal"), textsize = "14px", direction = "auto")
  ) %>%
  addLegend(title = "Happiness Score", pal = color_scheme, values = ~Score, position = "bottomright") %>%
  addMarkers(lng = 27, lat = 62, popup = "<strong>Top rated: </strong> Finland")

# Wyświetlenie gotowej mapy
happiness_map
