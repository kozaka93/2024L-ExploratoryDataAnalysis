library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
spotify <- read.csv("spotify-2023.csv")

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

spotify %>%
  filter(released_year==2023, released_month %in% c(1,2,3)) %>%
  mutate(bln_streams=as.numeric(streams)/1000000000) %>%
  ggplot(aes(x=as.factor(artist_count), y=bln_streams)) + geom_violin(fill="lightblue", color="slategray") + 
  geom_boxplot(fill="#fab555", color="#ff8c00", width=0.07) +
  labs(title="Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023
w pierwszym kwartale w zależności od liczby wykonawców", 
       x="Liczba wykonawców", y="Liczba wyświetleń w miliardach")

# Na wszystkich wykresach mediana jest na podobnym poziomie.
# Na pierwszym wykresie jest dużo odstających wartości.
# Na drugim 3. kwartyl jest bardzo rozciągnięty.

# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

spotify %>%
  filter(released_year %in% 2019:2022) %>%
  mutate(date=as.Date(paste(released_year, released_month, released_day, sep="-"))) %>%
  mutate(weekday=weekdays(date)) %>%
  ggplot(aes(x=factor(weekday, weekdays(min(date) + -1:5)), fill=factor(released_year))) + geom_bar() +
  facet_wrap(~released_year, nrow=2) + scale_x_discrete(guide = guide_axis(angle = 50)) +
  labs(title="Rozkład liczby wypuszczonych piosenek względem dnia tygodnia
dla poszczególnych lat między 2019 a 2022", x="Dzień tygodnia", y="Liczba wypuszczonych piosenek",
       fill="Rok")

# Widać, że w każdym roku w piątek wypuszczano najwięcej piosenek.

# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

# "Piosenki w przeliczeniu na liczbę playlist" interpretuję jak PKB per capita,
# tzn. dzielimy jedno przez drugie.

spotify %>%
  mutate(streams=as.numeric(streams)) %>%
  filter(streams>0) %>% # Nie chcemy NA
  mutate(streams_per_p=streams/in_spotify_playlists) %>%
  filter(streams_per_p>quantile(streams_per_p, 0.8)) %>%
  ggplot(aes(x=mode, y=bpm)) + geom_violin(fill="darkmagenta", color="#4B0082") + 
  geom_boxplot(width=0.4, fill="darkorchid", color="orchid") +
  labs(title="Rozkład tempa względem skali dla piosenek, które są
w 20% najczęściej odtwarzanych piosenek w przeliczeniu na liczbę
playlist spotify", x="Skala", y="Tempo")

# Wykresy wyglądają podobnie, chociaż dla skali minorowej jest szerszy (większa gęstość).
  
# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

# "Artyści osobno" interpretuję jako nie branie pod uwagę piosenek, gdzie jest >1 artysta.
# Top 10 jako head(10) z tego co wyjdzie.

# Tabelka wydaje mi się czytelniejsza niż np: wykres kolumnowy w tym przypadku, ale jak trzeba to trzeba :)

spotify %>%
  mutate(streams=as.numeric(streams)) %>%
  filter(streams>0) %>% # Nie chcemy NA
  group_by(artist.s._name) %>%
  summarise(sum=sum(streams)) %>%
  arrange(-sum) %>%
  head(10) %>%
  ggplot(aes(x=as.factor(fct_reorder(artist.s._name, sum)), y=sum)) + geom_col(fill="springgreen") +
  coord_flip() + labs(title="Artyści mający najwięcej odtworzeń wszystkich swoich
piosenek w sumie", x="Artysta", y="Suma odtworzeń") +
  scale_y_continuous(labels=label_number())

# Odpowiedź:

# 1 The Weeknd      14185552870
# 2 Taylor Swift    14053658300
# 3 Ed Sheeran      13908947204
# 4 Harry Styles    11608645649
# 5 Bad Bunny        9997799607
# 6 Olivia Rodrigo   7442148916
# 7 Eminem           6183805596
# 8 Bruno Mars       5846920599
# 9 Arctic Monkeys   5569806731
# 10 Imagine Dragons  5272484650

# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

spotify %>%
  ggplot(aes(x=danceability_., y=energy_., color=factor(artist_count))) + geom_point() +
  facet_wrap(~artist_count, nrow=4) + labs(x="Taneczność", y="Energetyczność",
  title="Wykres zależności energetyczności od taneczności
patrząc przez pryzmat liczby artystów", color="Liczba artystów") +
  xlim(c(0,100)) + ylim(c(0,100))

# Nie widać tutaj za bardzo żadnej zależności.

# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka była najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

# Tak samo jak w zad.4 - tabelka wydaje mi się czytelniejsza.

spotify %>%
  filter(released_year %in% 2013:2023) %>%
  group_by(released_year) %>%
  mutate(streams=as.numeric(streams)) %>%
  arrange(released_year) %>%
  top_n(1, streams) %>%
  select(released_year, track_name, artist.s._name, streams) %>%
  ggplot(aes(x=as.factor(released_year), y=streams)) + geom_col(fill="salmon") +
  geom_text(aes(label=track_name), position=position_stack(vjust=0.5), color="white") +
  scale_y_continuous(labels=label_number()) + coord_flip() +
  labs(title="Najbardziej popularna piosenka w każdym roku w latach 2013-2023",
       y="Liczba wyświetleń", x="Rok")

# Odpowiedź:

# 1          2013 Take Me To Church         Hozier                       2135158446
# 2          2014 Thinking Out Loud         Ed Sheeran                   2280566092
# 3          2015 Love Yourself             Justin Bieber                2123309722
# 4          2016 One Dance                 Drake, WizKid, Kyla          2713922350
# 5          2017 Shape of You              Ed Sheeran                   3562543890
# 6          2018 Someone You Loved         Lewis Capaldi                2887241814
# 7          2019 Blinding Lights           The Weeknd                   3703895074
# 8          2020 Heat Waves                Glass Animals                2557975762
# 9          2021 STAY (with Justin Bieber) Justin Bieber, The Kid Laroi 2665343922
# 10          2022 As It Was                 Harry Styles                 2513188493
# 11          2023 Flowers                   Miley Cyrus                  1316855716

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

p <- function(spotify){sp <- spotify %>%
  filter(released_year==2022) %>%
  group_by(released_month) %>%
  summarise(playlists=sum(in_spotify_playlists), apple_playlists=sum(in_apple_playlists))
  ap <- sp %>% select(1,3) %>% cbind("apple")
  colnames(ap)[c(2,3)] <- c("playlists", "name")
  sp <- sp %>% select(1,2) %>% cbind("spotify")
  colnames(sp)[3] <- "name"
  total <- rbind(sp, ap, deparse.level = 1);
  return(total)
}

p <- p(spotify) %>%
  arrange(released_month, sort(name))

ggplot(data=p, aes(x=as.factor(released_month), y=playlists, fill=name)) + geom_col(position="dodge") +
  labs(title="Wykres zależności pomiędzy sumą piosenek na playlistach
na spotify a apple patrząc po miesiącach", x="Miesiąc", y="Suma piosenek", fill="Platforma") +
  scale_y_log10(labels=label_number())

# Stosując skalę ze zwykłymi liczbami nic ciekawego nie widać (poza tym, że apple chyba cieszy się
# dużo mniejszą popularnością), ale na logarytmicznej słupki rozkładają się bardzo podobnie.
# W maju jest najwięcej.

# Miłego sprawdzania :)