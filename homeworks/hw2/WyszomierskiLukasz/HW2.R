spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(lubridate)
# install.packages("forcats")
library(forcats)
library(scales)
library(tidyr)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 & released_month %in% c(1,2,3)) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  ggplot(aes(x = factor(artist_count), y = streams)) +
  geom_boxplot(fill = "seagreen")+
  scale_y_continuous(labels = label_number()) +
  labs(title = "Rozkład liczby odtworzeń piosenek opublikowanych w pierwszym kwartale 2023 w zależności od liczby wykonawców",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń piosenek")

# Na podstawie wykresów możemy zaobserwować, że mediana w liczby odtworzeń jest podobna
# dla każdej liczby wykonawców. W przypadku piosenek z trzema wykonawcami rozstęp 
# miedzykwartylowy jest najmniejszy, a w przypadku piosenek z jednym wykonawcą widzimy
# sporo obesrwacji odstających ze znacznie wyższą liczbą odtworzeń.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year>= 2019 & released_year<=2022) %>% 
  mutate(day_of_the_week = wday(paste(released_year, released_month, released_day, sep = "-"), label=TRUE, abbr=FALSE)) %>% 
  ggplot(aes(x = day_of_the_week)) +
    geom_bar(fill = "seagreen") +
    facet_wrap(~released_year, scales = "free_y") +
    labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia w latach 2019-2022",
       x = "dzień tygodnia",
       y = "Liczba wypuszczonych piosenek")

# W każdym roku najwięcej piosenek było wypuszczanych w piątki, a drugim dniem tygodnia 
# z największą liczbą wypuszczanych piosenek był czwartek. Liczba piosenek publikowanych
# w pozostałe dni zależy od roku.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(streams_per_playlist = as.numeric(streams) / in_spotify_playlists) %>% 
  filter(streams_per_playlist >= quantile(streams_per_playlist, probs = 0.8, na.rm = TRUE)) %>% 
  ggplot(aes(x = mode, y = bpm)) +
    geom_boxplot(fill = "seagreen") +
    labs(title = "Rozkład tempa względem skali dla piosenek, które sa wśród 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist",
       x = "skala",
       y = "tempo (beats per minute)")

# Zarówno w przypadku skali Minor i Major mediana tempa piosenki wynosi około 
# 125 bpm. Dla skali Minor rozstęp mniędzykwartylowy tempa jest mniejszy, ale 
# występują wartości odstające o wyższym tempie.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(streams)) %>% 
  arrange(-total_streams) %>% 
  head(10) %>% 
  mutate(artist.s._name = fct_reorder(artist.s._name, -total_streams)) %>% 
  ggplot(aes(x = artist.s._name, y = total_streams)) +
  geom_col(fill = "seagreen") +
  scale_y_continuous(labels = label_number()) +
  labs(title = "Top 10 artystów, którzy w sumie mają najwięcej odtworzeń swoich piosenek",
       x = "Imię artysty / nazwa zespołu",
       y = "Łączna liczba odtworzeń")

#Odp:
#artist.s._name  total_streams
#<chr>                   <dbl>
#1 The Weeknd        14185552870
#2 Taylor Swift      14053658300
#3 Ed Sheeran        13908947204
#4 Harry Styles      11608645649
#5 Bad Bunny          9997799607
#6 Olivia Rodrigo     7442148916
#7 Eminem             6183805596
#8 Bruno Mars         5846920599
#9 Arctic Monkeys     5569806731
#10 Imagine Dragons    5272484650

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  ggplot(aes(x = danceability_., y = energy_., color = factor(artist_count))) +
  geom_point() +
  scale_color_manual(values = c("navyblue", "orange", "green", "lightblue", "yellow", "red", "hotpink", "darkgrey"))+
  labs(title = "Zależnośc energetyczności od taneczności w zależności od liczby artystów",
       x = "Taneczność",
       y = "Energetyczność",
       color = "Liczba artystów")

# Trudno wskazać jednoznaczne zależności między tanecznością a energetycznością, 
# jednak na ogół im piosenka jest bardziej taneczna, tym bardziej jest energetyczna.

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year %in% 2013:2023) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  group_by(released_year) %>% 
  top_n(1,streams) %>% 
  arrange(released_year) %>% 
  select(track_name, artist.s._name, streams) %>% 
  mutate(released_year = factor(released_year)) %>% 
  ggplot(aes(x = released_year, y = streams)) +
  geom_col(fill = "seagreen") +
  scale_y_continuous(labels = label_number()) +
  geom_text(aes(y = streams * 0.5, label = track_name), angle = -90) +
  labs(title = "Piosenki z największą liczbą odtworzeń w latach 2013 - 2023",
       x = "Rok",
       y = "Liczba odtworzeń")

#Odp:
#released_year track_name                artist.s._name                  streams
#<int> <chr>                     <chr>                             <dbl>
#1          2013 Take Me To Church         Hozier                       2135158446
#2          2014 Thinking Out Loud         Ed Sheeran                   2280566092
#3          2015 Love Yourself             Justin Bieber                2123309722
#4          2016 One Dance                 Drake, WizKid, Kyla          2713922350
#5          2017 Shape of You              Ed Sheeran                   3562543890
#6          2018 Someone You Loved         Lewis Capaldi                2887241814
#7          2019 Blinding Lights           The Weeknd                   3703895074
#8          2020 Heat Waves                Glass Animals                2557975762
#9          2021 STAY (with Justin Bieber) Justin Bieber, The Kid Laroi 2665343922
#10          2022 As It Was                 Harry Styles                 2513188493
#11          2023 Flowers                   Miley Cyrus                  1316855716

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(spotify= sum(in_spotify_playlists),
            apple = sum(in_apple_playlists)) %>% 
  pivot_longer(!released_month, names_to="platform", values_to="sum") %>% 
  ggplot(aes(fill = platform, x = factor(released_month), y = sum)) +
  geom_bar(position="dodge", stat="identity") +
  labs(title = "Porównanie sum piosenek na playlistach na spotify i apple w 2022",
       x = "Miesiąc",
       y = "Suma piosenek na playlistach",
       fill = "Platforma")

# Zarówno w przypadku apple jak i spotify suma piosenek na playlistach jest najwyższa 
# w maju. W każdym miesiącu suma piosenek na playlistach na apple jest znacznie niższa,
# niż na spotify.