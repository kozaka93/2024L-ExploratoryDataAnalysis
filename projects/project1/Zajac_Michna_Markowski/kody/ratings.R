library(ggplot2)
library(dplyr)
library(forcats)

sciezka2 <- "data/himym_episodewise.csv"
data2 <- read.csv(sciezka2)
episodes_path <- "data/himym_episodes.csv"
episodes <- read.csv(episodes_path)
imdb_path <- "data/himym_imdb.csv"
imdb <- read.csv(imdb_path)

sciezka_frineds <- "data/friends_episodes_v2.csv"
imdb_friends <- read.csv(sciezka_frineds)

sciezka2 <- "data/modern_family_info.csv"
imdb_modern <- read.csv(sciezka2)




### Wspólna tabela ####
#1. filrowanie danych

himym_imdb_clear <- imdb %>%
  select(season, imdb_rating) %>%
  rename(Rating = imdb_rating, Season = season)


friends_imdb_clear <- imdb_friends %>%
  select(Season, Stars) %>%
  filter(Season <= 9) %>%
  rename(Rating = Stars)


add_season_column <- function(df) {
  df$Season <- as.integer(gsub("^S(\\d+)-.*", "\\1", df$Season.Episode))
  return(df)
}
modern_imdb_clear <- add_season_column(imdb_modern) %>%
  select(Season, Rating) %>%
  filter(Season <= 9)


#2.Łączymy w jedną tabelę

dane_do_wykresu <- rbind(
  cbind(himym_imdb_clear, Series = "Himym"),
  cbind(friends_imdb_clear, Series = "Friends"),
  cbind(modern_imdb_clear, Series = "Modern Family")
)

dane_do_wykresu <- mutate(dane_do_wykresu,Season = as.factor(Season))


#3. Tworzymy wykres

wykres_ocen <- ggplot(dane_do_wykresu, aes(x = Season, y = Rating, fill = Series)) + 
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Comparison of ratings for HIMYM, Friends and Modern Family",
    x = "Season",
    y = "Rating",
    fill = "Series"
  ) +
  ylim(0,NA) +
  scale_fill_manual(values = c(
    "Himym" = "yellow",
    "Modern Family" = "violet",
    "Friends" = "red"
    )
  ) +
  theme(axis.text.x = element_text(size = 12, color = "yellow"),
        axis.text.y = element_text(size = 12, color = "yellow"),
        axis.title.x = element_text(size = 14, face = "bold", color = "yellow"),
        axis.title.y = element_text(size = 14, face = "bold", color = "yellow"),
        plot.title = element_text(size = 18, face = "bold", color = "yellow"),
        legend.title = element_text(size = 14, face = "bold", color = "yellow"),
        legend.text = element_text(size = 12, color = "yellow"),
        axis.line = element_line(linewidth =1, color = "yellow"))

wykres_ocen
