spotify <- read.csv("spotify-2023.csv")
spotify <- mutate(spotify, streams = as.numeric(streams))
options(scipen = 12)

setwd("C:\\Users\\malgo\\Documents\\WED")
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_month %in% 1:4,
         released_year == 2023) %>%
  mutate(artist_count = as.character(artist_count)) %>% 
  ggplot(aes(y = streams, x = artist_count)) +
  geom_boxplot(fill = "#5BF725", outlier.colour = "darkgreen")+
  labs(title = "Rozkład liczby odtworzeń piosenek",
       subtitle = "W pierwszym kwartale roku 2023",
       x = "Liczba wykonawców",
       y = "Odtworzenia")+
  scale_y_continuous(limits = c(0, 8e8))+
  theme(plot.background = element_rect(fill = "#F3FFEE"),
        panel.background = element_rect(fill = "#E0FFD5"))
  

# Odp. Dla każdej z trzech grup mediana wypada w podobnym miejscu 
# (około 150 000 000), dla 3 wykonawców jest najmniejszy rozrzut, dla 1 trochę 
# większy a dla 2 wykonawców jest znacznie większy (około 3-4 razy) w porównaniu
# do rozrzutu dla 3 wykonawców.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>% 
  filter(released_year %in% 2019:2022) %>% 
  transmute(date = paste(released_year, released_month, released_day, sep = "-"),
            date = as.Date(date), 
            weekday = factor(weekdays(date),
                             levels = c("Monday", "Tuesday", "Wednesday",
                                        "Thursday", "Friday", "Saturday",
                                        "Sunday")),
            year = as.character(released_year)) %>% 
  ggplot(aes(x = weekday))+
  geom_bar(fill = "#F79A25")+
  facet_wrap(~year, scales = "free_y")+
  labs(title = "Liczba wypuszczonych piosenek w latach 2019-2022",
       x = "Dzień tygodnia",
       y = "Liczba piosenek")+
  theme(plot.background = element_rect(fill = "#FFF2E1"),
        axis.text = element_text(size = 15, color = "#603200"),
        axis.title = element_text(size = 15,color = "#603200"),
        plot.title = element_text(size = 20,color = "#603200"),
        strip.background = element_rect(fill = "#F79A25"),
        strip.text = element_text(size = 15, color = "#603200"))+
  scale_x_discrete(guide = guide_axis(angle = 35))

# W roku 2022 ogólnie znacznie więcej piosenek było wypuszczonych bez wzgledu na dzien
# tygodnia. W każdym roku przeważająca liczba piosenek była wypuszczona w piątek.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

spotify %>% 
  mutate(streams_per_playlist = streams/in_spotify_playlists) %>% 
  slice_max(streams_per_playlist, prop = 0.2) %>% 
  ggplot(aes(x = bpm, fill = mode))+
  geom_density(alpha = 0.3)+
  scale_fill_manual(labels = c("Durowa", "Molowa"),
                    values = c("#FF5500","#00DCFF"))+
  labs(
    title = "Rozkład tempa",
    subtitle = "Najczęściej odtwarzanych piosenek",
    x = "Tempo (beats per minute)",
    y = "",
    fill = "Skala")+
  theme(axis.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        plot.subtitle = element_text(size = 16))


# Odp : Najwięcej jest piosenek średnio szybkich (około 130 bpm), 
# Zauważalnie więcej jest piosenek w skali molowej w przedziale od około 120 bpm  
# do 150 bpm. Od 150 bpm przeważają piosenki w skali durowej.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>%
  group_by(artist.s._name) %>% 
  summarise(sum_streams = sum(streams)) %>% 
  slice_max(sum_streams, n = 10) %>%
  mutate(name = fct_reorder(artist.s._name, sum_streams)) %>% 
  ggplot(aes(name, sum_streams)) +
  geom_col(fill = "#7257BB")+
  scale_x_discrete(guide = guide_axis(angle = 35))+
  labs(title = "Czyje piosenki są odtwarzane najczęściej",
       x = "Artysta",
       y = "Łączna liczba odtworzeń")+
  theme(plot.background = element_rect(fill = "#ECE4FF"),
        axis.text = element_text(size = 12, color = "#11003D"),
        axis.title = element_text(size = 15,color = "#11003D"),
        plot.title = element_text(size = 20,color = "#11003D"))
  
#Odp. W top 10 najwięcej mają The Weekend, Taylor Swift i Ed Sheeran -
# podobnie dużo.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>% 
  mutate(N_artists = as.character(artist_count)) %>% 
  ggplot(aes(danceability_., energy_.))+
    geom_point(color = "#CF387D", alpha = 0.4)+
  facet_wrap(~N_artists, nrow = 2)+
  labs(title = "Energetyczność a taneczność",
       subtitle = 'W zależności od liczby artystów',
       x = "Taneczność",
       y = "Energetyczność")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 17))

# Odp: Dla liczby artystów 4-7 jest za mało danych żeby powiedzieć coś konkretnego,
# Energetyczność raczej rośnie wraz ze wzrostem taneczności ale rozproszenie jest duże.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka była najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter(released_year %in% 2013:2023) %>% 
  group_by(released_year) %>%
  arrange(-streams, by_group = TRUE) %>% 
  summarise(Top = first(track_name), N_streams = first(streams)) %>% 
  mutate(year = as.character(released_year)) %>% 
  ggplot(aes(N_streams, year, label = Top))+
    geom_col(fill = "#0008A7")+
  labs(title = "Najpopularniejsze piosenki w latach 2013-2023",
       x = "Liczba odsłuchań",
       y = "Rok")+
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20))+
  geom_text(aes(label = Top), hjust = 2, color = "#f9faff", size = 5)

# Odp : Zaczynając od 2023 : Flowers, As It Was, STAY, Heat Waves, Blinding Lights,
# Someone You Loved, Shape of You, One Dance, Love Yourself, Thinking Out Loud,
# Take Me To Church. Na przestrzeni lat wybijają się dwa tytuły z większą liczbą
# odtworzeń, najwięcej ma Blinding Lights a zaraz potem Shape of You.

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

Sys.setlocale("LC_TIME", "Polish.UTF-8")

spotify %>% 
  filter(released_year == 2022) %>% 
  transmute(on_spotify = as.numeric(in_spotify_playlists > 0),
         on_apple = as.numeric(in_apple_playlists > 0),
         month = fct_reorder(strftime(paste("24", released_month, "01", sep = "-"), "%b"),
                             released_month)) %>%
  group_by(month) %>% 
  summarise(Spotify = sum(on_spotify),
            Apple = sum(on_apple)) %>% 
  pivot_longer(cols = c(Spotify, Apple),
               names_to = "Platform",
               values_to = "Count") %>% 
  ggplot(aes(month, Count, fill = Platform))+
  geom_col(position = "dodge")+
  scale_fill_manual(values = c("#2E0219","#6EEB83"))+
  labs(title = "Ile piosenek jest na playlistach?",
       x = "Miesiąc wydania",
       y = "Liczba piosenek",
       fill = "Platforma")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        plot.title = element_text(size = 20),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+
  scale_y_continuous(breaks = seq(0, 80, 10),
                     minor_breaks = seq(0, 80, 5))

# Odp: W każdym miesiącu piosenek na playlistach Apple było nie więcej niż na 
# playlistach Spotify. Piosenki wydane w maju znalazły się na największej liczbie playlist.




