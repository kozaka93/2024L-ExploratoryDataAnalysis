library(dplyr)
library(ggplot2)
spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

zadanie1_data <- spotify %>% 
  filter(released_year == 2023, released_month %in% 1:3) %>%
  mutate(count = as.factor(artist_count)) %>% mutate(streams = as.numeric(streams))

zadanie1_plot <- ggplot(zadanie1_data, aes(x = count, y = streams)) + 
  geom_boxplot() +
  labs(x = "Liczba wykonawców", 
       y = "Liczba odtworzeń",
       title = "Rozkład liczby odtworzeń piosenek opublikowanych w 2023 roku (I kwartał)") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))

# mozemy zauwazyc, ze najwiekszy rozstrzal wartosci wystepuje w przypadku liczby odtworzen dla dwoch
# wykonawcow
# jednak to dla jednego wykonawcy mamy najbardziej odstajace wartosci
# srednia odtworzen dla kazdej liczby wykonawcow utrzymuje sie na stalym poziomie - jest mniej wiecej
# taka sama dla kazdej liczby wykonawcow

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

zadanie2_data <- spotify %>% filter(released_year %in% 2019:2022) %>%
    mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
    mutate(day_of_the_week = weekdays(released_date)) %>%
    group_by(released_year, day_of_the_week) %>% summarise(num = n())

zadanie2_data$day_of_the_week <- factor(zadanie2_data$day_of_the_week, 
                                         levels = c("Monday", "Tuesday", "Wednesday", 
                                                   "Thursday", "Friday", "Saturday", "Sunday"))

zadanie2_plot <- ggplot(zadanie2_data, aes(x = day_of_the_week, y = num, group = released_year)) +
  geom_bar(stat = "identity", fill = "#ffa9d4") +
  facet_wrap(~released_year, scales = "free") +
  labs(x = "Dzień tygodnia", y = "Liczba piosenek", 
       title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(drop = FALSE) +  theme(plot.title = element_text(hjust = 0.5))

# na podstawie wykresow widzimy, ze dla kazdego roku jest gwaltowny wzrost liczby wypuszczonych
# piosenek w piatek

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

zadanie3_data <- spotify %>% arrange(desc(in_spotify_playlists)) %>% head(ceiling(nrow(spotify)*0.2))

zadanie3_plot_violin <-ggplot(zadanie3_data, aes(x = `mode`, y = `bpm`)) + 
  geom_violin()  + 
  labs(x = "skala", y = "tempo bpm", title = "Rozkład tempa względem skali") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

zadanie3_plot_box <-ggplot(zadanie3_data, aes(x = `mode`, y = `bpm`)) + 
  geom_boxplot()  + 
  labs(x = "skala", y = "tempo bpm", title = "Rozkład tempa względem skali") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

# na podstawie wykresu widzimy, ze wiekszy rozstrzal tempa ma skala "Major"
# ich dolne kwartyle maja zblizone wartosci, tak jak mediany, ale trzecie kwartyle juz znacznie sie roznia
# dodatkowo skala minor ma wiecej wartosci odstajacych
# ekstrema tempa osiagamy dla skali "Major"

# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

# osobno rozumiem tak, że rozważam tylko piosenki towrzone przez jednego artystę,
# czyli takie, że artist_count == 1

zadanie4_data <- spotify %>% 
  filter(artist_count == 1) %>%
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>% 
  summarise(all_streams = sum(streams)) %>% 
  arrange(desc(all_streams)) %>%
  head(10)


zadanie4_plot <- ggplot(zadanie4_data, aes(x = reorder(artist.s._name, desc(all_streams)), y = all_streams)) +
  geom_col(fill = "#ffa9d4") +
  labs(x = "Artysta", y = "Liczba odtworzeń", 
       title = "Top 10 artystów pod względem liczby odtworzeń") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +     
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# na wykresie otrzymujemy top 10 artystow pod wzgledem liczby odtworzeń
# widzimy, ze trzech artystow z najwieksza liczba odtworzen ma bardzo zblizona liczbe
# top 3 to The Weeknd, Taylor Swift, Ed Sheeran
# potem nastepuje gwaltowny spadek i ostatnich czterech artystow z tego zestawienia ma liczbe odtworzen na podobnym poziomie

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

zadanie5_plot <- ggplot(spotify, aes(x = `danceability_.`, y = `energy_.`, color = artist_count)) + 
  geom_point() +
  labs(x = "Taneczność", 
       y = "Energia", 
       color = "Liczba artystów",
       title = "Zależność taneczności od energii w podziale na skale.") +
  theme_bw() +
  theme(legend.position = "right") +
  scale_color_gradient2(low = "#f0c808", high = "#086788", mid = "#06aed5", limits = c(1,8), midpoint = 4) + 
  scale_x_continuous(limits = c(0,100)) + 
  scale_y_continuous(limits = c(0,100)) + theme(plot.title = element_text(hjust = 0.5))

# na wykresie najwiecej punktow nalezy do utworow wykonywanych przez dwoch/trzech artystow
# widzimy kumulacje punktow w prawym gornym rogu wykresu, czyli dla utworow o duzej wartosci
# wspolczynika tanecznosci i energii
# dodatkowo utwory wykonywane przez czterech lub wiecej artystow znajduja sie wlasne w pierwszej
# cwiartce wykresu, czyli dla tancznosci i energii z przedzialu [50, 100]

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


zadanie6_data <- data.frame(Year = seq(from = 2013, to = 2023, by = 1), 
                        title = rep(NA, 11), 
                        streams = rep(NA, 11))

for (i in 1:11){
  x <- spotify %>% filter(released_year == 2012+i) %>% 
    arrange(desc(as.numeric(streams)))
  zadanie6_data[i, 2] = x[1, "track_name"]
  zadanie6_data[i, 3] = x[1, "streams"]
}

zadanie6_plot <- ggplot(zadanie6_data, aes(x = factor(Year), y = streams, fill = title)) +
  geom_bar(stat = "identity") +
  labs(x = "Rok", 
       y = "Liczba odtworzeń", 
       fill = "Najpopularniejsza piosenka",
       title = "Najpopularniejsza piosenka w każdym roku (2013-2023)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(plot.title = element_text(hjust = 0.5))

# na wykresie otrzymujemy zestawienie najpopularniejszych piosenek w latach 2013-2023
# dodatkowo na wykresie mamy zaznaczona liczbe odtworzeń
# nazwy piosenek zaznaczone są kolorkami

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

zadanie7_data <- spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_spotify = sum(in_spotify_playlists), sum_apple = sum(in_apple_playlists))
zadanie7_data$month <- factor(zadanie7_data$released_month, levels = 1:12,
                                   labels = c("Styczeń", "Luty", "Marzec", "Kwiecień",
                                              "Maj", "Czerwiec", "Lipiec", "Sierpień",
                                              "Wrzesień", "Październik", "Listopad", "Grudzień"))

zadanie7_plot1 <- ggplot(zadanie7_data, aes(x = released_month)) +
  geom_line(aes(y = sum_spotify, color = "Spotify")) +
  geom_line(aes(y = sum_apple, color = "Apple")) +
  labs(x = "Miesiąc", 
       y = "Suma piosenek na playlistach",
       color = "Platforma",
       title = "Zależność sumy piosenek na playlistach na Spotify i Apple w 2022 roku") +
  scale_color_manual(values = c("Spotify" = "#1DB954", "Apple" = "#444444")) +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

# ten wykres pokazuje głównie dysproporcje w liczbie sumy piosenek na playlistach

# nastepny wykres pokaze nam juz zaleznosc miedzy suma piosenek na platformach patrzac po miesiacach
zadanie7_plot2 <- ggplot(zadanie7_data, aes(x = sum_spotify, y = sum_apple, color = as.factor(month))) +
  geom_point() +
  labs(x = "Suma piosenek na playlistach Spotify", 
       y = "Suma piosenek na playlistach Apple",
       color = "Miesiąc",
       title = "Zależność sumy piosenek na playlistach na Spotify i Apple w 2022 roku") +
  theme_minimal()

# widzimy, że dla wiekszosci miesiecy punkty oscyluja wokol prostej, a wartosci sumy
# piosenek na playlistach dla spotify sa nie przekraczaja 100000, a dla apple nie przekraczaja 2000
# wyrazny skok mamy natomiast w czerwcu

# dorysujmy prosta dopasowania na wykresie

zadanie7_plot2 <- ggplot(zadanie7_data, aes(x = sum_spotify, y = sum_apple, color = as.factor(month))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey", formula = y ~ x) +
  labs(x = "Suma piosenek na playlistach Spotify", 
       y = "Suma piosenek na playlistach Apple",
       color = "Miesiąc",
       title = "Zależność sumy piosenek na playlistach na Spotify i Apple w 2022 roku") +
  theme_minimal()

# prosta podkresla, ze punkty oscyluja wokol prostej
