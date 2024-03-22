spotify <- read.csv("spotify-2023.csv")
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?
options(scipen = 12)

zad1 <- spotify %>% 
    filter(released_month %in%  c(1,2,3) &  released_year == 2023) %>%
    ggplot(aes(x = as.factor(artist_count),
               y = as.numeric(streams) / 10^6)) +
    geom_boxplot() +
    labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
         subtitle = "Pierwszy kwartał 2023",
         x = "Liczba wykonawców",
         y = "Liczba odtworzeń (mln)")+
    scale_x_discrete(expand = c(0,0)) +
    theme_minimal(base_size = 16)
    
zad1

# Widać, że największą medianę odtworzeń mają piosenki z dwójką wykonawców.
# te utwory mają też największy rozstęp międzykwartylowy i najwyższy pierwszy kwartyl. W utworach jednego
# wykonawcy widoczny jest 1 outlier z znacznie wyższą ilością odtworzeń.
# Utwory trójki wykonawców mają najmniejszy IQR.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?
dni_tygodnia <- c("Poniedziałek", "Wtorek", "Środa", "Czwartek", "Piątek", "Sobota", "Niedziela")

zad2 <- spotify %>% 
    filter(released_year >= 2019 & released_year <= 2022) %>%
    mutate(date = make_date(year = released_year, month = released_month, day = released_day)) %>% 
    mutate(day_of_week = wday(date, week_start = 1, label = TRUE)) %>% 
    group_by(released_year, day_of_week) %>% 
    summarise(number_songs= n()) %>%
    ggplot(aes(y = factor(day_of_week),
               x = number_songs, 
               fill = factor(released_year)))+
    geom_col(position = "dodge") +
    theme_minimal(base_size = 16) +
    labs(title = "Liczba wypuszczonych piosenek w zależności od dnia tygodnia",
         x = '',
         y = '',
         fill = "Rok")+
    scale_y_discrete(labels = dni_tygodnia) +
    scale_x_continuous(expand = c(0,0))
    
    
zad2

# Widać, że najwięcek piosenek jest wypuszczanych w piątki.
# Warto zauważyć również, że w 2022 roku powstało znacznie więcej piosenek niz
# w 3 poprzednich latach. Natomiast tendencja piątkowa jest zawsze widoczna.



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

zad3 <- spotify %>% 
    filter(in_spotify_playlists >= quantile(in_spotify_playlists, 0.8)) %>% 
    ggplot(aes(x = mode,
               y = bpm)) +
    geom_boxplot() +
    labs(title = "Rozkład tempa względem skali",
         subtitle = "20% najczęsciej odtwarzanych piosenek",
         x = "Skala",
         y = "Tempo (bpm)")+
    scale_x_discrete(expand = c(0,0)) +
    theme_minimal(base_size = 16)


zad3

# Utwory w skali Major mają minimalnie wyższą medianę tempa niż te w skali Minor,
# Natomiast rozkład międzykwartylowy tempa w skali Major jest większy niż 
# w skali Minor

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

zad4 <- spotify %>% 
    filter(artist_count == 1) %>% 
    group_by(artist.s._name) %>% 
    summarize(total_streams = sum(as.numeric(streams), na.rm = TRUE) / 10^6) %>% 
    arrange(desc(total_streams)) %>% 
    head(10) %>% 
    ggplot(aes(y = reorder(artist.s._name, total_streams),
               x = total_streams)) +
    geom_col(color = "#8CB9BD", fill = "#8CB9BD")+
    labs(title = "Artyści z największą sumą odtworzeń piosenek",
         x = "Łączne odtworzenia (mln)",
         y = "")+
    scale_y_discrete(expand = c(0,0)) +
    scale_x_continuous(expand = c(0,0)) +
    theme_minimal(base_size = 16)

zad4

# Można zauważyć, że w pierwszej trójce łączna liczba odtworzeń jest całkiem zbliżona






# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?
brewer.pal()

zad5 <- spotify %>% 
    ggplot(aes(x = danceability_.,
               y = energy_.)) +
    geom_point()+
    labs(title = "Zależność energetyczności od taneczności piosenek",
         subtitle = "Numer nad wykresem oznacza liczbę artystów",
         x = "Taneczność",
         y = "Energetyczność")+
    facet_wrap(~artist_count,ncol = 4) +
    theme_minimal(base_size = 16) 
zad5

# Łatwo zauważyć, że najwiecej piosenek było tworzonych przez jednego lub dwóch artystów.
# Piosenki tworzone przez jednego lub dwóch wykonawców zdają się być zbalansowane,
# tj. są równomiernie rozłożone względem stosunku taneczności i energetyczności.
# Co ciekawe im więcej twórców bierze udział w tworzeniu danej piosenki, tym
# bardziej energetyczna i taneczna ona się staje. (szczególnie widać to dla grup
# powyżej 5 osób)


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

zad6 <- spotify %>% 
    filter(2013 <= released_year & released_year <= 2023) %>% 
    group_by(released_year) %>% 
    filter(streams == max(as.numeric(streams))) %>% 
    ggplot(aes(x = factor(released_year),
               y = as.numeric(streams) / 10^6))+
    geom_col(color = "#8CB9BD", fill = "#8CB9BD")+
    geom_text(aes(label = track_name),color = "#FEFBF6",                   
               position = position_dodge2(width = 0.5),
               show.legend = FALSE, hjust = 1.1, size = 4) +
    labs(title = "Najpopularniejsza piosenka w danym roku",
         subtitle = "2013 - 2023",
         y = "Liczba odtworzeń (mln)",
         x = '')+
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_minimal(base_size = 16)+
    coord_flip()
zad6

# Na wykresie widać jasno, jaki utwór był najbardziej popularny w dany roku.
# Uwagę można zwrócić na fakt, że piosenki z 2019 i 2017 roku
# miały znacznie więce odtworzeń niż pozostałe najpopularniejsze.
# Zadziwiająco mało odtworzeń ma najpopularniejsza piosenka ubiegłego roku.




# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?
miesiace <- c("Styczeń", "Luty", "Marzec", "Kwiecień", "Maj", "Czerwiec", "Lipiec", "Sierpień", "Wrzesień", "Październik", "Listopad", "Grudzień")


zad7 <- spotify %>% 
    filter(released_year == 2022) %>% 
    group_by(released_month) %>% 
    summarise(apple_sum = log(sum(in_apple_playlists, na.rm = TRUE)), spotify_sum = log(sum(in_spotify_playlists, na.rm = TRUE))) %>% 
    mutate(released_month = factor(miesiace, levels = miesiace)) %>% 
    pivot_longer(cols = c(apple_sum, spotify_sum)) %>% 
    ggplot(aes(x = released_month, y = value, group = name, fill = name))+
    geom_col(position = "dodge")+
    labs(title =  "Suma piosenek na playlistach",
         subtitle = "Spotify vs Apple - rok 2022",
         y = "Liczba piosenek na playlistach (skala logarytmiczna)",
         x = "",
         fill = "Platforma")+
    scale_fill_discrete(labels = c("Apple", "Spotify"))+
    scale_x_discrete(expand = c(0,0)) + 
    scale_y_continuous(expand= c(0,0)) + 
    theme_minimal(base_size = 16) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
zad7
# Widać, że w każdym miesiącu na playlistach spotify jest wiecej utworów niż na
# tych w Apple Music.




