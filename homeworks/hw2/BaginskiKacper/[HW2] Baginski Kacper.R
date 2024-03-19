library(dplyr)
library(ggplot2)
library(RColorBrewer)

spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 <- spotify %>%
  filter(released_year == 2023, released_month %in% c(1, 2, 3)) %>% 
  mutate(streams = as.numeric(streams))

ggplot(df1, aes(x = factor(artist_count), y = streams, fill = factor(artist_count))) +
  geom_violin() +
  labs(x = "Number of Artists", y = "Streams", fill = "Artist Count") +
  ggtitle("Distribution of Streams by Number of Artists in Q1 2023") +
  scale_y_continuous(labels = scales::comma) +
  scale_color_manual(values = brewer.pal(3, "Set1"))

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 <- spotify %>%
  filter(released_year %in% 2019:2022) %>%
  mutate(day_of_week = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) %>%
  count(released_year, day_of_week)

ggplot(df2, aes(x = day_of_week, y = n, fill = factor(released_year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Day of Week", y = "Number of Songs", fill = "Year") +
  ggtitle("Distribution of Released Songs by Day of Week (2019-2022)") +
  scale_color_manual(values = brewer.pal(4, "Set1"))

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

top20 <- quantile(spotify$in_spotify_playlists, probs = 0.8)

df3 <- spotify %>%
  filter(in_spotify_playlists >= top20)
  
ggplot(df3, aes(x = mode, y = bpm, fill = mode)) +
  geom_violin() +
  labs(x = "Mode", y = "BPM") +
  ggtitle("Distribution of Tempo by Mode for Top 20% Most Played Songs on Spotify") +
  scale_color_manual(values = brewer.pal(2, "Set1"))

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 <- spotify %>%
  group_by(artist.s._name) %>%
  summarise(total_streams = sum(as.numeric(streams))) %>%
  top_n(10, total_streams)

ggplot(df4, aes(x = reorder(artist.s._name, total_streams), y = total_streams)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Artist Name", y = "Total Streams", title = "Top 10 Artists by Total Streams") +
  scale_y_continuous(expand = c(0,0), labels = scales::comma) +
  coord_flip()

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(x = danceability_., y = energy_., color = factor(artist_count))) +
  geom_jitter(size = 2) +
  labs(x = "Danceability (%)", y = "Energy (%)", color = "Artist Count") +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("Relationship between Danceability and Energy by Artist Count") +
  scale_color_manual(values = brewer.pal(8, "Set1"))

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


df6 <- spotify %>%
  filter(released_year >= 2013 & released_year <= 2023) %>%
  group_by(released_year) %>%
  slice(which.max(streams))

ggplot(df6, aes(x = as.numeric(streams), y = paste(released_year, "|", track_name))) +
  geom_col(fill = "skyblue") +
  labs(x = "Streams",
       y = "Year + Song Name",
       fill = "Year",
       title = "Most Popular Song by Year (2013-2023)") +
  scale_x_continuous(expand = c(0,0), labels = scales::comma)

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7 <- spotify %>%
  filter(released_year == 2022) %>% 
  mutate(released_month = factor(released_month, levels = 1:12, labels = month.name))

ggplot(df7, aes(x = released_month)) +
  geom_bar(aes(y = in_spotify_playlists, fill = "Spotify"), position = position_dodge(width = 0.9), stat = "identity") +
  geom_bar(aes(y = in_apple_playlists, fill = "Apple Music"), position = position_dodge(width = 0.9), stat = "identity") +
  labs(x = "Month", y = "Total Playlists", title = "Total Playlists on Spotify and Apple Music in 2022") +
  scale_fill_manual(name = "Platform", values = c("Spotify" = "skyblue", "Apple Music" = "salmon"))
