spotify <- read.csv("spotify-2023.csv")

library(dplyr)
library(ggplot2)
library(tidyr)
options(scipen=999)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 <- spotify %>% 
  filter(released_year == 2023 & released_month %in% c(1, 2, 3))

plot1 <- ggplot(df1, aes(y = as.numeric(streams), x = as.factor(artist_count))) +
  geom_boxplot() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1350855716),
                     breaks = c(200000000, 400000000, 600000000, 800000000, 1000000000, 1200000000),
                     labels = c("200 000 000", "400 000 000", "600 000 000", "800 000 000", "1 000 000 000", "1 200 000 000")) +
  theme_classic() +
  labs(x = "liczba wykonawców",
       y = "liczba odtworzeń piosenki",
       title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
       subtitle = "na podstawie piosenek wydanych w 1. kwartale 2023 roku")  

plot1

# Niezależnie od liczby wykonawców mediana wynosi około 150 milionów odtworzeń. Najwięcej
# odtorzeń miała piosenka solowego artysty - ponad miliard. Najbardziej zróżnicowana
# jest liczba odtworzeń piosenek stworzonych przez 2 artystów. 


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 <- spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>% 
  mutate(date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>% 
  mutate(weekday = weekdays(date)) %>% 
  group_by(released_year, weekday) %>% 
  summarise(n = n())
  
plot2 <- ggplot(df2, aes(fill=as.factor(released_year), y=n, x=as.factor(weekday))) + 
  geom_bar(position="dodge", stat="identity") +
  scale_x_discrete(limits = c("poniedziałek", "wtorek", "środa", "czwartek", 
                              "piątek", "sobota", "niedziela"), expand = c(0,0)) +
  scale_y_continuous(expand = c(0, 0),  limits = c(0, 265)) +
  theme_classic() +
  labs(x = "dzień tygodnia",
       y = "liczba wydanych piosenek",
       title = "Liczba wypuszczonych piosenek w poszczególnych dniach tygodnia",
       subtitle = "w latach 2019-2022") +
  guides(fill=guide_legend(title="Rok"))
  
plot2

# Bez względu na rok piosenki są najczęściej wydawane w piątek, drugi najczęstszy 
# "termin" to czwartek. W pozostałe dni tygodnia, ta liczba jest dosyć mała.
# W 2019 roku nie wydano żadych piosenek w poniedziałek i w sobotę, za to w 2020
# w żaden wtorek. 
# Najwięcej piosenek zostało wydanych w 2022, a potem w 2021 roku.


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3 <- spotify %>% 
  mutate(new = as.numeric(streams)/as.numeric(in_spotify_playlists)) %>% 
  filter(new >= quantile(new, prob = 0.8, na.rm = TRUE))
  
plot3 <- ggplot(df3, aes(fill = as.factor(mode), x = as.numeric(bpm)))+
  geom_density(alpha = 0.6) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.02)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 225), 
                     breaks = c(25, 50, 75, 100, 125, 150, 175, 200)) +
  labs(x = "tempo (bpm)",
       y = "gęstość",
       title = "Rozkład tempa względem skali",
       subtitle = "dla piosenek, które są w 20% najczęściej odtwarzanych piosenek w 
przeliczeniu na liczbę playlist spotify") +
  guides(fill=guide_legend(title="Skala"))

plot3

# Dla obu skali najczęściej tempo wynosi około 130 bpm, najniżejsze wynosi około 50,
# a najwyższe około 200. W skali major jest więcej piosenek, które mają 150 - 200 bpm, 
# niż dla skali minor. Z kolei w skali minor jest więcej piosenek, których tempo 
# wynosi 100- 150 bpm.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 <- spotify %>% 
  group_by(artist.s._name) %>% 
  summarise(sum_streams = sum(as.numeric(streams), na.rm = TRUE)) %>% 
  arrange(-sum_streams) %>% 
  head(10) %>% 
  mutate(artist = forcats::fct_reorder(artist.s._name, sum_streams))


plot4 <- ggplot(df4, aes(x = sum_streams, y = artist)) +
  geom_col(fill = "#2596be") +
  geom_text(aes(label = sum_streams), vjust = 0.4, hjust = 1.1, colour = "white") +
  theme_classic() +
  scale_x_continuous(expand = c(0, 0), breaks = c(2500000000, 5000000000, 7500000000, 10000000000, 12500000000)) +
  labs(x = "suma odtworzeń",
       y = "artysta",
       title = "Top 10 artystów z największą sumą odtworzeń piosenek")

plot4

# Artysta z największą liczbą odtworzeń piosenek to The Weeknd. Każdy z pierwszych
# czterech artystów ma ponad 10 miliardów odtworzeń. Na ostatnim miejscu znajduje się
# Imagine Dragons z ponad 5 miliardami odtworzeń.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

plot5 <- ggplot(spotify, aes(x = as.numeric(energy_.), y = as.numeric(danceability_.), color = as.factor(artist_count))) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 100)) +
  scale_colour_manual(name = "Liczba artystów w piosence", 
                      values = c("#eff051", "#33ccd0", "#7539a9", "#ea5058", "#ff954d", "#3351d0", "#7bf051", "#ed20c9")) +
  labs(x = "energetyczność",
       y = "taneczność",
       title = "Zależność energetyczności i taneczności",
       subtitle = "ze względu na liczbę artystów w piosence")

plot5

# Większość piosenek jest energetyczna i taneczna, trudno zaobserwować jakąś zależność
# między liczbą artystów w piosence, a tymi dwoma wartościami. Najmniej taneczna i 
# energetyczna piosenka została stworzona przez 3 artystów. 

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

df6 <- spotify %>% 
  filter(released_year >= 2013 & released_year <= 2023) %>% 
  group_by(released_year) %>% 
  filter(streams == max(as.numeric(streams)))

plot6 <- ggplot(df6, aes(y = as.factor(released_year), x = as.numeric(streams))) +
  geom_col(fill = "#2596be") +
  geom_text(aes(label = track_name), vjust = 0.4, hjust = 1.1, colour = "white") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 3803895074), 
                     breaks = c(500000000,1000000000, 1500000000, 2000000000, 2500000000, 3000000000, 3500000000),
                     labels = c(0.5, 1, 1.5, 2, 2.5, 3, 3.5)) +
  theme_classic() +
  labs(x = "liczba odtworzeń w miliardach",
       y = "rok",
       title = "Najbardziej popularne piosenki w latach 2013-2023")

plot6

# Na przestrzeni lat 2013-1023 najpopularniejszą piosenką było 'Blinging Lights' (2019), drugie miejsce
# zajmuje 'Shape of You' (2017). Obie piosenki mają ponad 3,5 miliarda odtworzeń. Piosenki z
# pozostałych lat nie przekroczyły 3 miliardów odtworzeń. Na ostatnim miejscu znajduje się piosenka
# 'Flowers' z 2023 roku, która jako jedyna z zestawienia nie przekroczyła 1,5 miliarda odworzeń.


# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_spotify = log(sum(as.numeric(in_spotify_playlists))), sum_apple = log(sum(as.numeric(in_apple_playlists)))) %>% 
  pivot_longer(cols = c(sum_spotify, sum_apple))

plot7 <- ggplot(df7, aes(x = as.factor(released_month), y = as.numeric(value), fill = name)) +
  geom_bar(position="dodge", stat="identity") +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 13)) +
  labs(x = "miesiąc",
       y = "logarytm z liczby piosenek na playlistach",
       title = "Zależność pomiędzy sumą piosenek na playlistach na Spotify i Apple",
       subtitle = "w 2022 roku, względem miesięcy") +
  scale_fill_discrete( labels = c("Apple", "Spotify"), name = "Platforma")

plot7

# Zastosowałam skalę logarytmiczną, aby zależność była lepiej widoczna.
# W każdym miesiącu na playlistach Spotify znajduje się więcej piosenek niż na
# playlistach Apple.
# Widzimy, że zazwyczaj gdy rosła ilość piosenek na playlistach Spotify to na
# ich liczba w playlistach Apple także rosła. Dla obu serwisów na najwięcej 
# liczbie playlist znalazly się piosenki wydane w maju.