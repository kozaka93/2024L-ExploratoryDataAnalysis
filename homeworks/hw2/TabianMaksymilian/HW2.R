spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)
options(scipen = 12)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 <- spotify %>% 
  filter(released_year == 2023 &
         released_month %in% c(1,2,3) &
         streams != "Love Grows (Where My Rosemary Goes)") %>% 
  mutate(streams = as.integer(streams),
         artist_count = as.factor(artist_count),
         released_month = as.factor(released_month))


ggplot(df1, aes(x = artist_count, y = streams)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = "dla pierwszego kwartału 2023",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń piosenek (w skali logarytminczej)")
  
# Dla 1 wykonawcy mamy obserwacje odstające od normy. 
# Mediana liczby odwtorzeń jest największa dla 2 wykonawców, nieco mniejsza
# dla 1, a najmniejsza dla 3 wykonawców.
# Dla 2 wykonawców możemy zaobserwować największy rozrzut liczby odtworzeń

  
# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 <- spotify %>% 
  filter(released_year %in% c(2019,2020,2021,2022)) %>%
  mutate(data = as.Date(paste(released_year, released_month, released_day, sep = "-")),
         released_year = as.factor(released_year),
         weekday = weekdays(data),
         weekday = fct_relevel(weekday, c("poniedziałek", "wtorek", "środa",
                                          "czwartek", "piątek", "sobota", "niedziela")))

ggplot(df2, aes(x = weekday)) +
  geom_bar() + 
  facet_wrap(~released_year, scales = "free_y") +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "W latach 2019-2022",
       x = "Dzień tygodnia",
       y = "Liczba wypuszczonych piosenek") +
  scale_x_discrete(guide = guide_axis(angle = 20))

# W każdym roku zdecydowanie najwięcej piosenek wypuszczono w piątek,
# Liczba wypuszczanych piosenek rośnie wraz z upływem dni tygodnia,
# osiągając maksimum wypuszczanych piosenek w piątek.

  
# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3 <- spotify %>%
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>% 
  mutate(streams = as.double(streams),
         rate = streams/in_spotify_playlists,
         top_rate = case_when(
           rate < quantile(rate, 0.8) ~ "no",
           rate >= quantile(rate, 0.8) ~ "yes")) %>%
  filter(top_rate == "yes")

ggplot(df3, aes(x = mode, y = bpm)) +
  geom_boxplot() +
  labs(title = "Rozkład tempa względem skali",
       x = "Skala",
       y = "Tempo (bpm)")
  
# Mediana tempa jest obu skal jest porównywalna i wynosi około 125 bpm
# Dla skali Minor możemy zaobserwować pojedyncze obserwacje odstające
# W przypadku skali Major możemy zaobserwować większy rozrzut tempa


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 <- spotify %>%
  filter(track_name != "Love Grows (Where My Rosemary Goes)") %>% 
  mutate(streams = as.double(streams)) %>% 
  group_by(artist.s._name) %>% 
  summarise(sum = sum(streams)) %>% 
  arrange(-sum) %>% 
  head(10) %>% 
  mutate(artist.s._name = fct_reorder(artist.s._name, sum))
  

ggplot(df4, aes(x = sum, y = artist.s._name))+
  geom_col() +
  labs(title = "Top 10 artystów z największą liczbą odtworzeń ",
       x = "Suma odtworzeń",
       y = "Artysta")

# Artyści z największą sumą odwtowrzeń swoich piosenek to kolejno
# The Weekend, Taylor Swift, Ed Sheeran, Harry Styles, Bad Bunyy itd.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

df5 <- spotify %>%
  mutate(artist_count = as.factor(artist_count))
  
ggplot(df5, aes(x = energy_., y = danceability_., color = artist_count))+
  geom_point() + 
  labs(title = "Zależnośc energetyczności od taneczności",
       subtitle = "w zależności od liczby artystów",
       x = "Energetyczność",
       y = "Taneczność",
       color = "Liczba artystów")

# Bez względu na liczbę artystów obserwowana jest tendencja, że
# im większa energetyczność utworu, tym większa jego taneczność


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


df6 <- spotify %>% 
  filter(released_year <= 2023 & released_year >= 2013) %>% 
  mutate(streams = as.double(streams),
         released_year = as.factor(released_year)) %>% 
  group_by(released_year) %>% 
  top_n(1, streams)

ggplot(df6, aes(x = released_year, y = streams)) +
  geom_col() + 
  geom_text(aes(label = track_name ), angle = 90,  colour = "Red") +
  labs(title = "Piosenki z największą liczbą odtworzeń ",
       subtitle = "dla lat 2013 - 2023",
       x = "Rok",
       y = "Liczba odtworzeń")

# Najpopularniejsze piosenki w kolejnych latach zostały podpisane na wykresie
# Najpopularniejszą piosenką, podsumowując wszystkie lata,
# jest Blinding Lights, a potem Shape of You.
# Najmniej popularne jest Flowers


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  mutate(released_month = as.factor(released_month)) %>% 
  group_by(released_month) %>% 
  summarise(sum_spotify = sum(in_spotify_playlists), sum_apple = sum(in_apple_playlists)) %>% 
  pivot_longer(cols = c(sum_spotify, sum_apple))

ggplot(df7, aes(x = released_month, y = value, fill = name))+
  geom_bar(stat="identity", position = position_dodge()) +
  scale_y_log10() +
  labs(title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple",
       y = "Liczba piosenek w skali logarytmicznej",
       x = "Miesiące")

# W każdym miesiącu na spotify jest więcej pioenek niż na apple
#  W maju było najwięcej piosenek na playlistach zarówno na spotify, 
# jak i na apple
# We wrześniu było najmniej piosenek na playlistach zarówno na spotify,
# jak i na apple
