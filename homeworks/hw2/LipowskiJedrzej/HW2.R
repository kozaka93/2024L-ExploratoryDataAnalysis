library(forcats)
library(tidyverse)
library(dplyr)
library(scales)
library(ggplot2)
spotify <- read.csv("spotify-2023.csv")

# Funkcja Sys.setlocale("LC_TIME", "C") jest potrzebna, żeby weekdays zwracało
# nazwy dni tygodnia po angielsku. LC_TIME można zmienić z powrotem funkcją
# Sys.setlocale("LC_TIME", "Polish_Poland.utf8").

# Sys.getlocale("LC_TIME") #"Polish_Poland.utf8"
# Sys.setlocale("LC_TIME", "Polish_Poland.utf8")
Sys.setlocale("LC_TIME", "C")

spotify %>% 
  View()

options(scipen = 4)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>% 
  filter(released_year == 2023 & released_month <= 3) %>%
  ggplot(aes(y = factor(artist_count), x = as.numeric(streams))) +
  geom_boxplot()

# Odp.: Utwory wykonywane przez 3 osoby miały zazwyczaj najmniej odtworzeń, a
# utwory wykonywane przez 2 osoby najwięcej (aczkolwiek mediana odtworzeń utworów
# wykonywanych przez 2 osoby nie jest dużo wyższa niż mediana dla utworów
# wykonywanych przez 1 osobę). Widzimy też, że dla utworów wykonywanych przez 1 osobę
# jest kilka wartości daleko odstających od reszty.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?


p <- spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>%
  select(released_year, released_month, released_day) %>% 
  mutate(weekday = factor(weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-"))),
                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                     "Friday", "Saturday", "Sunday"))) %>% 
  ggplot(aes(weekday)) + 
  geom_bar() +
  theme(axis.text.x = element_text(angle = 15))

p + facet_wrap(~factor(released_year))

p + facet_wrap(~factor(released_year), scales = "free") #lepiej tu widać poszczególne
# lata, ale trzeba uważnie czytać osie, bo wykresy są w różnych skalach.

# Odp.: Zdecydowanie najwięcej piosenek było wypuszczanych w piątki, 
# a potem w czwartki.



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

p <- spotify %>% 
  select(streams, bpm, mode) %>% 
  mutate(streams = as.numeric(streams)) %>% 
  filter(streams  >= quantile(streams, 0.80, na.rm = TRUE))

ggplot(p, aes(y = factor(mode), x = bpm)) +
  geom_boxplot()

ggplot(p, aes(y = factor(mode), x = bpm)) +
  geom_violin()

#Odp.: widzimy, że piosoneki o tonacji minorowej mają zazwyczaj niższe tempo
# niż piosenki o tonacji majorowej.

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

spotify %>% 
  filter(artist_count == 1) %>% 
  group_by(artist.s._name) %>% 
  summarise(total_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  mutate(artist.s._name = forcats::fct_reorder(artist.s._name, total_streams)) %>%
  arrange(-total_streams) %>% 
  head(10) %>% 
  ggplot(aes(x = artist.s._name, y = total_streams)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 15)) +
  labs(x = "Total number of streams", y = "Artist's name")

# Odp.: Można to odczytać z wykresu. Są to:
#1 The Weeknd        
#2 Taylor Swift     
#3 Ed Sheeran        
#4 Harry Styles     
#5 Bad Bunny         
#6 Olivia Rodrigo     
#7 Eminem          
#8 Bruno Mars        
#9 Arctic Monkeys  
#10 Imagine Dragons


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

p <- spotify %>% 
  select(artist_count, energy_., danceability_.) %>%
  ggplot(aes(y = energy_., x = danceability_.)) +
  geom_point()

p + facet_wrap(~factor(artist_count))

# Odp.: Trudno sformułować jakieś wnioski na temat tej zależności. 
# 

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

spotify %>% 
  filter( 2013 <= released_year & released_year <= 2023) %>%
  group_by(released_year) %>% 
  filter(streams == max(streams, na.rm = TRUE)) %>% 
  select(track_name, streams, released_year) %>% 
  mutate(track_name = forcats::fct_reorder(paste(as.character(released_year), track_name ,sep=" - "),
                                           released_year),
         streams = as.numeric(streams)) %>% 
  ggplot(aes(x = track_name, y = streams)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title = "Most popular songs in years 2013-2023", x = "Song Title", y = "Streams")

# Odp.: Można to odczytać z wykresu, są to:
#2013 - Get Lucky - Radio Edit                  
#2014 - I Love You So                   
#2015 - Wait a Minute!               
#2016 - No Lie                      
#2017 - Dark Red  
#2018 - we fell in love in october
#2019 - Arcade           
#2020 - HEARTBREAK ANNIVERSARY                 
#2021 - Where Are You Now                          
#2022 - Anti-Hero            
#2023 - Queencard.     


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

spotify %>% 
  filter(released_year == 2022) %>%
  select(released_month, in_spotify_playlists, in_apple_playlists) %>% 
  group_by(released_month) %>% 
  summarise(spotify_sum = sum(in_spotify_playlists, na.rm = TRUE), 
            apple_sum = sum(in_apple_playlists, na.rm = TRUE)) %>%
  pivot_longer(-released_month) %>% 
  ggplot(aes(x = released_month, y = value, fill = name)) + 
  geom_col(position = "dodge") +
  scale_x_continuous(breaks= pretty_breaks()) +
  labs(y = "Number of songs in playlists", x = "Months")  # + scale_y_log10()

  
# Odp.: Piosenek na playlistach apple było zdecydowanie mniej niż na playlistach
# spotify w każdym miesiącu roku 2022.
  