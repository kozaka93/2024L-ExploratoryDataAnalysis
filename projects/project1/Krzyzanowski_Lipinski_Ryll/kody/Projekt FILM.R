library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

movies <- read.csv("rotten_tomatoes_movies.csv")

new_data <- movies %>% 
  filter(boxOffice != "" & is.na(audienceScore) == FALSE & is.na(tomatoMeter) == FALSE) %>% 
  mutate(boxOffice = case_when(
    substring(boxOffice, nchar(boxOffice), nchar(boxOffice)) == "K" ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)-1))*1000,
    substring(boxOffice, nchar(boxOffice), nchar(boxOffice)) == "M" ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)-1))*1000000,
    TRUE ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)))),
    genre = factor(case_when(grepl("Action", genre) ~ "Action",
                            TRUE ~ "Rest"), levels = c("Action", "Rest"))) %>% 
    filter(boxOffice >= 125000000) %>% 
    arrange(-boxOffice) 

new_data %>% 
  ggplot(aes(x = audienceScore, y = boxOffice, color = genre)) + 
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("blue", "#fdebd3"),
                     name = "", labels = c("Filmy akcji", "Pozostałe filmy")) +
  labs(title = "Zarobki filmów w zależności od recenzji widzów",
       x = "Procent pozytywnych recenzji użytkowników Rotten Tomatoes",
       y = "Zarobki filmu w dolarach amerykańskich") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), 
                     limit = c(125e6, 900e6),
                     breaks = seq(from = 0, to = 875e6, by = 125e6)) +
  scale_x_continuous(limit = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 10)) +
  theme(panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white"),
    plot.background = element_rect(fill = '#4173ab', color = NA), #transparent
    legend.text = element_text(color = "white"),
    axis.title.x = element_text(colour = "white"),
    axis.title.y = element_text(colour = "white"),
    title = element_text(colour = "white"),
    axis.text.x = element_text(colour = "white"),
    axis.text.y = element_text(colour = "white"))


new_data %>% 
  ggplot(aes(x = tomatoMeter, y = boxOffice, color = genre)) + 
  geom_point() +
  theme_minimal() +
  scale_color_manual(values = c("blue", "#fdebd3"), 
                     name = "", labels = c("Filmy akcji", "Pozostałe filmy")) +
  labs(title = "Zarobki filmów w zależności od recenzji krytyków",
       x = "Procent pozytywnych recenzji krytyków w Rotten Tomatoes",
       y = "Zarobki filmu w dolarach amerykańskich") +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6), 
                     limit = c(125e6, 900e6),
                     breaks = seq(from = 0, to = 900e6, by = 125e6)) +
  scale_x_continuous(limit = c(0, 100),
                     breaks = seq(from = 0, to = 100, by = 10))  +
  theme(panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white"),
        plot.background = element_rect(fill = '#4173ab', color = NA), #transparent
        legend.text = element_text(color = "white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"),
        title = element_text(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))
