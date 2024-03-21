library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

spotify <- read.csv("spotify-2023.csv")
spotify$streams = as.double(spotify$streams)
spotify = spotify %>% 
  mutate(released_date = make_date(year = released_year, month = released_month, day = released_day),
         released_day_of_week = wday(released_date, label = TRUE),
         released_month_name = month(released_date, label = TRUE))



# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 = spotify %>% 
  filter(released_year == 2023 & released_month >=1 & released_month <=3) 

p1 = ggplot(df1, aes(x = as.factor(artist_count),
             y = streams/10^6))+ 
  geom_boxplot() + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_log10()+
  labs(x = "Liczba wykonawców",
       y = "Liczba odtworzeń (skala logarytmiczna)",
       title = "Jak liczba wykonawców wpływa na odtworzenia?",
       subtitle = "Pierwszy kwartał 2023")+
  theme_minimal()

# Na podstawie wykresu widzimy, że średnio najwięcej odtworzeń miały piosenki, które
# zostały stworzone przez jednego wykonawcę.
 

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 = spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022)

p2 = ggplot(df2, 
            aes(x = released_day_of_week)) + 
  geom_bar() + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0))+
  labs(x = "Dzień tygodnia",
       y = "Liczba wypuszczonych piosenek",
       title = "Dzień tygodnia a liczba wypuszczonych piosenek",
       subtitle = "Lata 2019-2022") +
  theme_minimal()

# Wykres jasno pokazuje, że najwięcej premier ma miejsce w piątki. Najpewniej wynika
# to z zasad notowań na listach przebojów - wypuszczając piosenkę w piątek, gwarantuje
# się jej więcej czasu na listach.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3 = spotify %>% 
  top_frac(0.2, wt = in_spotify_playlists)

p3 = ggplot(df3, 
            aes(x = bpm,
                y = mode,
                fill = mode)) + 
  geom_violin() + 
  scale_y_discrete(expand = c(0,0)) +
  scale_x_continuous() +
  labs(x = "Tempo w BPM",
       y.ticks = NULL, 
       y = NULL,
       title = "Skala a tempo",
       subtitle = "Top 20% utworów pod względem liczby playlist na Spotify") + 
  theme(legend.position = 'none')

# Na wykresach widać, że jednym z najczęstszych temp jest około 120BPM. Ponadto,
# tempa piosenek w skali durowej rozkładają się mniej więcej równomiernie, z dodatnią
# asymetrią. W skali molowej większość piosenek ma tempo około 120BPM, wyższe tempa
# są mało popularne.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 = spotify %>% 
  separate_rows(artist.s._name, sep = ", ") %>% 
  group_by(artist.s._name) %>% 
  summarise(streams_total = sum(streams)) %>% 
  top_n(10, wt = streams_total) %>% 
  arrange(-streams_total)

p4 =  ggplot(df4, aes(x = streams_total/10^9,
                      y = reorder(artist.s._name, streams_total),
                      fill = artist.s._name
                      ))+
  geom_col()+
  geom_text(aes(label = artist.s._name, hjust = 1.25, size = 7))+
  scale_y_discrete(expand = c(0,0), breaks = NULL)+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Najpopularniejsi artyści",
       subtitle = "Sumaryczna liczba odtworzeń",
       y = "Artysta",
       x = "Liczba odtworzeń (w mld)") +
  theme(legend.position = 'none')

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od taneczności patrząc przez pryzmat liczby artystów?

df5 = spotify 

p5 = ggplot(df5, aes(x = energy_.,
                     y = danceability_.,
                     color = as.factor(artist_count)))+
  geom_point() +
  facet_wrap(~artist_count, ncol = 4)+
  labs(title = "Taneczność a energetyczność",
       subtitle = "Zależność między powyższymi a liczbą artystów",
       y = "Taneczność",
       x = "Energetyczność") +
  theme(legend.position = 'none')

# Z wykresów można wyciągnąć wniosek, że niewiele utworów znacząco się różni jeśli chodzi
# o taneczność i energetyczność, tzn. rzadko trafia się utwór z wysokim współczynnikiem
# taneczności, a niskim energetyczności i podobnie. Dodatkowo, większość utworów jest 
# żywsza; duże zagęszczenie punktów można zauważyć dla wysokich wartości obydwu zmiennych.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

df6 = spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  filter(streams == max(streams))

p6 = ggplot(df6, aes(y = as.factor(released_year),
                     x = streams/10^9,
                     fill = track_name))+
  geom_col()+
  geom_text(aes(label = track_name, hjust = 1.25, size = 7))+
  scale_y_discrete(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0))+
  labs(title = "Najpopularniejsze piosenki dekady",
       subtitle = "Liczba odtworzeń w latach 2013-2023",
       y = "Rok",
       x = "Liczba odtworzeń (w mld)",
       fill = "Tytuł") +
  theme(legend.position = 'none')



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7 = spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month_name) %>% 
  summarize(Apple = sum(in_apple_playlists), Spotify = sum(in_spotify_playlists)) %>% 
  pivot_longer(!released_month_name, names_to = 'platform', values_to = 'count')
options(scipen = 12)
p7 = ggplot(df7, aes(x = released_month_name,
                     y = count,
                     fill = platform))+
  geom_col(position = 'dodge')+
  # scale_x_discrete(expand = c(0,0))+
  scale_y_log10(expand = c(0,0))+
  scale_x_discrete()+
  labs(title = "Apple vs Spotify",
       subtitle = "Suma piosenek na playlistach, rok 2022",
       x = "Miesiąc",
       y = "Liczba piosenek",
       fill = "Platforma")

# Z obrazka widać, że Spotify bezsprzecznie wygrywa z Apple