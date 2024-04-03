sciezka <- 'C:/Users/User/Desktop/wizualizacja/hw2/spotify-2023.csv'
spotify <- read.csv(sciezka)
install.packages('dyplr')
library(dyplr)
library(ggplot2)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

zad1 <- spotify %>%
  filter(released_year == 2023, released_month %in% 1:3) %>%
  mutate(count = as.factor(artist_count),
         streams = as.numeric(streams))

zad1 %>%
  ggplot(aes(x = count, y = streams)) +
  geom_boxplot() +
  geom_violin()+
  labs(x = "Liczba wykonawców", 
       y = "Liczba odtworzeń",
       title = "Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale w zależności od liczby wykonawców") + 
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5))

#największy rozstęp występuje w przypadku dla liczby odtworzen dla 2 wykonawców, a najmniejszy dla 3 wykonawców;
#średnia liczba odtworzeń oscyluje w 3 przypadkach w  bliskim sobie otoczeniu - na podobnym poziomie;
#w przypadku 1 wykonawców, liczba odtworzeń posiada sporo wartosci odstających i niewiele istotnie odstających





# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?



zad2 <- spotify %>% 
  filter(released_year %in% 2019:2022, !is.na(released_year), !is.na(released_day)) %>%
  mutate(released_date = as.Date(paste(released_year, released_month, released_day, sep = "-"))) %>%
  mutate(day_of_the_week = factor(weekdays(released_date), 
                                  levels = c("poniedziałek", "wtorek", "środa", "czwartek", "piątek", "sobota", "niedziela"))) %>%
  group_by(released_year, day_of_the_week) %>% 
  summarise(num = n())

zad2 %>% 
  
  ggplot( aes(x = day_of_the_week, y = num, group = released_year)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  facet_wrap(~released_year, scales = "free") +
  labs(x = "Dzień tygodnia", y = "Liczba piosenek", 
       title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia w latach 2019-2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(drop = FALSE) +  
  theme(plot.title = element_text(hjust = 0.5))

#w latach 2019-2022 najwiecej piosenek było wypuszczanych w piątki, nastepnie najczesciej wystepujacym dniem tygodnia jest czwartek
#na przestrzeni lat 2020-2021 istotnie zmalała liczba pisoenek publiowanych w poniedziałki, soboty oraz niedziele
#co wiecej liczba wypuszczanych piosenek wzrastała w tym okresie ponad 10ci krotnie porównujac lata 2019 a 2022; 


# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

zad3 <- spotify %>% 
  arrange(desc(in_spotify_playlists)) %>% 
  head(ceiling(nrow(spotify)*0.2))

zad3_gestosc <-ggplot(zad3, aes(x = `mode`, y = `bpm`)) + 
  geom_violin()  + 
  labs(x = "skala", y = "tempo bpm", title = "Rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
 najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

zad3 %>% 
  ggplot( aes(x = `mode`, y = `bpm`)) + 
  geom_violin()  + 
  labs(x = "skala", y = "tempo bpm", title = "Rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
 najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))

#na podstawie wykrsu widzimy, że skala major ma większy rozstęp niz minor
#ekstrema osiagmai dla skali major




# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

#install.packages('scales')
library(scales)

zad4 <- spotify %>% 
  filter(artist_count == 1) %>%
  mutate(streams = as.numeric(streams)) %>%
  group_by(artist.s._name) %>% 
  summarise(all_streams = sum(streams)) %>% 
  arrange(desc(all_streams)) %>%
  head(10)

zad4 %>% 
  ggplot( aes(x = reorder(artist.s._name, desc(all_streams)), y = all_streams)) +
  geom_col(fill = "blue") +
  labs(x = "artysta", y = "liczba odtworzeń", 
       title = "Top 10 artystów pod względem liczby odtworzeń") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +     
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(labels = scales::number_format()) +
  annotate("text", x = zad4$artist.s._name, y = zad4$all_streams, label = zad4$all_streams, vjust = -0.5, size = 3)

#na wykresie widzimy top 10 artystów, których utwory były najczęściej odtwarzane;
#the weekend, taylor, ed sheeran mają bardzo zbliżoną liczbę;
#tak samo na podobny poziomie, lecz ok 2 razy mniej niż top 3 mają bruno mars, arctic monkeys oraz imagine dragons


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


spotify %>% 
  ggplot(aes(x = `danceability_.`, y = `energy_.`, color = as.factor(artist_count))) +  
  geom_point() +
  labs(x = "Taneczność", 
       y = "Energia", 
       color = "Liczba artystów",
       title = "Zależność energetyczności od taneczności patrząc przez pryzmat liczby artystów") +
  theme_bw() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = rainbow(length(unique(spotify$artist_count)))) + 
  scale_y_continuous(limits = c(0, 100))

#znaczna liczba punktów znajduja się w obszarze dla tanecznosci [50,100] oraz energii [50,100];
#znajdują się tutaj przede wszystkim piosenki stworzone przez przynajmniej przez 4 badz wiecej twórcow;
#utwory stworzone przez pojedynczych artystów są dosyć zróznicowane -> charakteryzują się zarówno wysokim poziomem energii, ale niską tanecznosćia,
#badz znajdują sie w kategorii wysokiego poziomu tanecznosci a o niskiej energii;
#znikoma lizba utworów charakteryzuje sie niskim poziomem energii i tanecznosci; głownie dla tych, stworzonych przez
#pojedyncze osoby


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

zad6 <- data.frame(Year = seq(from = 2013, to = 2023, by = 1), 
                   title = rep(NA, 11), 
                   streams = rep(NA, 11))

for (i in 1:11){
  x <- spotify %>% 
    filter(released_year == 2012+i) %>% 
    arrange(desc(as.numeric(streams)))
  zad6[i, 2] = x[1, "track_name"]
  zad6[i, 3] = as.numeric(x[1, "streams"])  # Konwersja do liczby
}

zad6 %>% 
  ggplot(aes(x = factor(Year), y = streams, fill = title)) +
  geom_col(width = 0.8) +  
  geom_text(aes(label = title), vjust = 0.5, angle = 90, color = "black", hjust = 1.5, size = 4) +  
  labs(x = "Rok", 
       y = "Liczba odtworzeń", 
       fill = "Autor piosenki",
       title = "Najpopularniejsza piosenka w każdym roku (2013-2023)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::comma)


#na wykresie znajduje sie zaleznosc najpopularniejszej piosenki odtarzanych w latach 2013-2023;
# na OY widnieje liczba odtworzeń, na OX podane są interesujące nas lata; dla ulatwienia odczytu poszczegolnym piosenkom 
#nadano charakteyrystyczny kolor

#w tym okresie istotnie najczesniej odtwarzana piosenka the weekend 'blinding lights' w 2019 roku, liczba jej 
#odtwarzań jest ok 11 razy wieksza od liczby odtworzeń piosenki "flowers" miley cyrus bedącej najczesciej odtwarzanym 
#utworem w 2023 roku




# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?


zad7 <- spotify %>% 
  filter(released_year == 2022) %>% 
  group_by(released_month) %>% 
  summarise(sum_spotify = sum(in_spotify_playlists), sum_apple = sum(in_apple_playlists))

zad7$month <- factor(zad7$released_month, levels = 1:12,
                              labels = c("Styczeń", "Luty", "Marzec", "Kwiecień",
                                         "Maj", "Czerwiec", "Lipiec", "Sierpień",
                                         "Wrzesień", "Październik", "Listopad", "Grudzień"))

zad7 %>% 
 ggplot( aes(x = sum_spotify, y = sum_apple, color = as.factor(month))) +
  geom_point() +
  labs(x = "suma piosenek na playlistach Spotify", 
       y = "suma piosenek na playlistach Apple",
       color = "miesiąc",
       title = "zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
 po miesiącach w 2022") +
  theme_minimal()


#dla czerwca jest znaczny skok do ponad 2500 piosenek na apple i ok 200 tys na spotify;
#dla pozostałych miesiecy wartosci układaja się w prostą przechodząca przez poczatek ukaldu wspolrzednych;
#gdzie wartosci dla apple nie przekraczaja 2000 a dla spotify 90 tys







