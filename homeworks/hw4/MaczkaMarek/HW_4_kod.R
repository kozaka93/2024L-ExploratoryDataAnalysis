library(dplyr)
library(leaflet)
library(stringr)
library(htmlwidgets)
library(htmltools)


# violations <- read.csv("Building_Violations_20240418.csv", header = TRUE)
# 
# violations <- violations %>% 
#   mutate(VIOLATION.DATE = as.Date(VIOLATION.DATE, format = c("%m/%d/%Y"))) %>% 
#   filter(grepl(pattern = "2023-01-1", VIOLATION.DATE))
# 
# write.csv(violations, "Building_Violations_small.csv", row.names = FALSE)


violations <- read.csv("Building_Violations_small.csv", header = TRUE)

day = 18

punkciki <- violations %>% 
  filter(VIOLATION.DATE == paste0("2023-01-", day)) %>% 
  select(LONGITUDE, LATITUDE, ID, VIOLATION.DESCRIPTION, ADDRESS, 
         DEPARTMENT.BUREAU, VIOLATION.INSPECTOR.COMMENTS, VIOLATION.STATUS)

number <- dim(punkciki)[1]

# stworzenie ikonki przypinki
ikonka <- makeAwesomeIcon(
  icon = "building",
  iconColor = "black",
  markerColor = "blue",
  library = "fa"
)

mapka <- punkciki %>% 
  leaflet(options = leafletOptions(minZoom = 10)) %>%
  setView(-87.71, 41.83, 10) %>%
  addProviderTiles(providers$OpenStreetMap.HOT) %>%
  addAwesomeMarkers(lng = ~LONGITUDE, 
                   lat = ~LATITUDE,
                   icon = ikonka,
                   clusterOptions = TRUE,
                   popup = paste("<b> Address: </b>", 
                                 str_to_title(punkciki$ADDRESS),"<br>", 
                                 "<b> Department: </b>", 
                                 str_to_title(punkciki$DEPARTMENT.BUREAU), "<br>",
                                 "<b> Violation: </b>", 
                                 str_to_sentence(punkciki$VIOLATION.DESCRIPTION),
                                 "<br>",
                                 "<b> Status: </b>", 
                                 str_to_title(punkciki$VIOLATION.STATUS), "<br>",
                                 "<b> Inspector comments: </b> <br>",
                                 str_to_sentence(punkciki$VIOLATION.INSPECTOR.COMMENTS))) %>% 
  # ustawienie ograniczeń wyświetlania mapy
  setMaxBounds(lng1 = -87.94,
               lat1 = 41.64454,
               lng2 = -87.52414, 
               lat2 = 42.3) %>% 
  # dodanie tytułu
  addControl(HTML(paste0("<h2> Building violations in Chicago</h2> <h5> Found on January ", 
                         day,", 2023 </h5>")), position = "topright") %>% 
  # dodanie informacji o liczbie naruszeń
  addControl(HTML(paste("<b>Number of violations: </b><br>", number)), 
             position = "topright")

mapka
