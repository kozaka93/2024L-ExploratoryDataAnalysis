install.packages('dplyr')
library(dplyr)
sciezka2 <- "C:/Users/User/Desktop/wizualizacja/projekt1/himym_episodewise.csv"
data2 <- read.csv(sciezka2)
episodes_path <- "C:/Users/User/Desktop/wizualizacja/projekt1/himym_episodes.csv"
episodes <- read.csv(episodes_path)
imdb_path <- "C:/Users/User/Desktop/wizualizacja/projekt1/himym_imdb.csv"
imdb <- read.csv(imdb_path)



library(ggplot2)
library(dplyr)



imdb %>%
  ggplot(aes(x = as.factor(season),
             y = imdb_rating,
             color = as.factor(season))) +
  geom_boxplot() +
  stat_summary(fun = mean,
               aes(label = round(after_stat(y), 2)),
               geom = "text",
               color="black",
               fontface = "bold",
               position = position_dodge(width = 0.75),
               vjust = 1.5) +
  theme_classic() +
  theme(legend.position = "none") +
  scale_color_manual(values = rainbow(nlevels(as.factor(imdb$season)))) +
  labs(title = "uśrednione oceny na sezon",
       y = "oceny wg IMDB",
       x = "nr sezonu") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold",
                                  size = 16),
        axis.text.x = element_text(vjust = 0.5)) +
  scale_y_continuous(limits = c(0, 10))


