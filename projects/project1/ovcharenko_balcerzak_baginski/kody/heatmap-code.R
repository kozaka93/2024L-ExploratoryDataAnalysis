library("dplyr")
library("ggplot2")

data <- read.csv("breaking_bad.csv")

data <- mutate(data, Season = as.factor(Season),
               Episode = as.factor(Episode))
data$Deaths <- c(1, 0, 1, 0, 0, 1, 1,
                 1, 1, 0, 0, 0, 2, 3, 0, 0, 0, 1, 1, 167,
                 11, 0, 0, 1, 0, 2, 2, 4, 0, 0, 0, 4, 5,
                 1, 0, 0, 3, 0, 3, 0, 1, 1, 14, 0, 0, 5,
                 0, 3, 0, 0, 1, 0, 1, 10, 0, 10, 0, 0, 0, 2, 1, 10)

data$Rating_Category <- cut(data$Rating_IMDB, 
                            breaks = c(10, 9.5, 9, 8.5, 8, 0), 
                            labels = c("(9.5; 10.0]","(9.0; 9.5]", "(8.5; 9.0]", "(8.0; 8.5]", "[1.0; 8.0]"),
                            include.lowest = TRUE)

p <- ggplot(data, aes(x = Episode, y = Season, fill = Rating_Category)) +
  geom_tile(color = "black") +
  geom_text(aes(label = round(Rating_IMDB, 1)), color = "white", size = 8) +
  geom_text(aes(label = ifelse(Deaths > 0, paste("💀×", as.character(Deaths), sep=""), "")), color = "white", size = 4.0, vjust = 2.5) +
  scale_fill_manual(values = c("#E7DA19", "#A0A715", "#778913", "#486812", "#0D3E0F"),
                    labels = c("[1.0; 8.0]", "(8.0; 8.5]", "(8.5; 9.0]", "(9.0; 9.5]", "(9.5; 10.0]")) +
  labs(title = "Rating of each episode by season",
       x = "Episode",
       y = "Season",
       fill = "Rating\n(IMDB)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18),
        panel.grid = element_blank(),
        legend.text = element_text(size = 16, color = "#2a2a2a"),
        legend.title = element_text(size = 16),
        axis.text = element_text(size = 16, color = "#2a2a2a"),
        axis.title = element_text(size = 16)) +
  guides(fill = guide_legend(reverse = TRUE))
p


ggsave("plot.png", plot = p, bg = "transparent", dpi = 1200)


# Dodatkowe statystyki
# correlation <- cor(data$Rating_IMDB, data$bd)
# 
# data$bd <- c(1, 0, 1, 0, 0, 1, 1,
#              1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 1,
#              1, 0, 0, 1, 0, 1, 1, 1, 0, 0, 0, 1, 1,
#              1, 0, 0, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1,
#              0, 1, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1)
# 
# data %>%
#   group_by(bd) %>% 
#   summarise(mean(Rating_IMDB))

