
spotify <- read.csv("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Labiska/Pursko domowskie 2/spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(forcats)
library(stringr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

rozklad_licz_wyk <- spotify %>%
  filter(released_year == 2023 & released_month <= 4) %>%
  select(artist_count,streams) %>%
  mutate(streamy = as.numeric(streams))

wykres_1 <- ggplot(rozklad_licz_wyk, aes(x = as.factor(artist_count), y = streamy)) + geom_boxplot()
  
wykres_1



# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

rozklad_piosenki_dni <- spotify %>%
  filter(released_year > 2018 & released_year < 2023) %>%
  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         dzientyn = weekdays(released_date)) %>%
  select(released_year,dzientyn,streams) %>%
  group_by(released_year,dzientyn) %>%
  ggplot(aes(x = dzientyn, y = streams, fill = as.factor(released_year))) + geom_col() + facet_wrap(~released_year)

rozklad_piosenki_dni
  








# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?










# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

najpopularniejsi <- spotify %>%
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>%
  mutate(streams = as.numeric(streams)) %>%
  summarise(total_streams = sum(streams)) %>%
  arrange(-total_streams) %>%
  head(10) 
ggplot(najpopularniejsi, aes(x = forcats::fct_reorder(artist.s._name, -total_streams), y = total_streams)) + geom_col()

najpopularniejsi








# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = danceability_., y = energy_., color = as.factor(artist_count))) + geom_point() + facet_wrap(~artist_count)
  








# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?








# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?








