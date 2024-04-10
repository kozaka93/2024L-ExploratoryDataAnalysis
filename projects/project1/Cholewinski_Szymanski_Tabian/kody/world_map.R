library(ggplot2)
library(dplyr)
library(maps)
library(mapdata)

users_by_country <- read.csv("netflix-users-by-country-2024.csv")
users_by_country1 <- users_by_country %>% 
  select(-NetflixSubscribers2023) %>% 
  mutate(country = ifelse(country == "United States", "USA", country)) %>% 
  mutate(country = ifelse(country == "United Kingdom", "UK", country)) 

world <- map_data("world") %>%
  filter(long <= 180) %>% 
  left_join(users_by_country1, join_by ("region" == "country"))

## DO PREZENTACJI
plot2 <- world %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = netflixSubscribersPerCapita2023), color = "black") +
  coord_map("mollweide") +
  scale_fill_gradient(low = "#fee5d9",
                      high = "#99000d",
                      na.value = "white") +
  theme_void() +
  theme(panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        text = element_text(color = "white")) +  
  labs(title = "Percentage of population subscribing to Netflix") +
  theme(legend.title = element_blank()) 

plot2
ggsave("plot1.png", plot = plot2)



