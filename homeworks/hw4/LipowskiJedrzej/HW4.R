library(dplyr)
library(geojsonio)
library(tigris)
library(leaflet)

# przekształcamy dane Eurostatu, żeby pozbyć się zbędnych kolumn i rzędów oraz
# zamienić dane miesięczne w roczne.
foreign <- read.csv("foreign.csv")
foreign <- foreign %>%
  slice(-1) %>% 
  select(1, seq(from = 2, to = 820, by = 2)) %>%
  select(1, 398:409)
foreign[] <- lapply(foreign, gsub, pattern=',', replacement='')
foreign <- foreign %>% 
  mutate_at(2:13, as.numeric) %>%
  transmute(country = TIME, yearly = rowSums(across(2:13))/1000000) %>% 
  filter(!is.na(yearly)) %>% 
  slice(c(-1, -2))

# łączymy dane Eurostatu z mapą Europy.
europe <- geojsonio::geojson_read("europe.json", what = "sp")
europe <- geo_join(europe, foreign, by_sp = "name", by_df = "country")

# ustawiamy początkowy widok na Europę.
m <- leaflet(europe) %>%
  setView(15, 50, 3.65) %>%
  addTiles()

# poniżej znajdują się dane, które będzie widać po najechaniu myszą na państwo.
labels <- ifelse(!is.na(europe$yearly),
                 sprintf("<strong>%s</strong><br/>%g milionów nocy spędzonych<br/>w hotelach przez zagranicznych turystów.",
                         europe$name_pl, europe$yearly),
                 sprintf("<strong>%s</strong><br/>Brak danych na temat ilości nocy spędzonych<br/>w hotelach przez zagranicznych turystów.",
                         europe$name_pl)
) %>% lapply(htmltools::HTML)

# ustalamy paletę kolorów
pal <- colorNumeric(palette = "viridis", domain = europe$yearly)

# łączymy wszystko w całość i dodajemy legendę.
m <- m %>% addPolygons(
  fillColor = pal(europe$yearly),
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
  addLegend(pal = pal, values = foreign$yearly, opacity = 0.7, title = "Liczba noclegów <br/> zagranicznych turystów [mln]",
            position = "bottomright")
m
