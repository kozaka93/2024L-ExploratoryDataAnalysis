
library(rvest)
library(dplyr)
library(ggplot2)
library(plotly)

#Pobranie danych

url <- "https://en.wikipedia.org/wiki/List_of_Eurovision_Song_Contest_winners"

page <- read_html(url)

df <- html_element(page, "table.wikitable.sortable") %>% 
  html_table()

#Selekcja danych do mapy

new_rows <- data.frame(
  region = c("Albania","Andorra","Belarus","Bosnia and Herzegovina","Bulgaria",
             "Croatia","Cyprus","Czech Republic","Hungary","Iceland","Kosovo",
             "Liechtenstein","Lithuania","Malta","Moldova","Montenegro",
             "North Macedonia","Poland","Romania","San Marino","Slovakia",
             "Slovenia"),
  n = rep(0,22)
)

df1 <- df %>% 
  group_by(Country) %>% 
  summarise(n = n()) %>% 
  arrange(-n) %>% 
  mutate(Country = replace(Country, Country == "United Kingdom", "UK")) %>% 
  filter(Country != "Contest cancelled due to the COVID-19 pandemic") %>% 
  rename(region = Country) %>% 
  bind_rows(new_rows) %>% 
  print(n = 50)

#Tworzenie mapy

mapdata <- map_data("world")
mapdata <- left_join(mapdata, df1, by = "region")

mapdata1 <- mapdata %>% 
  filter(!is.na(mapdata$n))

map1 <- ggplot(mapdata1, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = as.factor(n)), color = "white", linewidth= 0.1) +
  scale_fill_manual(values = c("grey", "#A3CCE1","#88AAD2","#4C5FB2","#232C9D","#000058","#000027")) +
  theme_bw() +
  coord_fixed(1.5) +
  labs(title = "Amount of wins in the Eurovision Song Contest (as of April 2024)",
       fill = "Amount of wins") +
  theme(
    plot.title = element_text(size = 16),  
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 12))

map1_interactive <- ggplotly(map1, tooltip = c("region", "n"))
print(map1_interactive)
