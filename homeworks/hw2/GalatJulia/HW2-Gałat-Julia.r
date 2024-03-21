spotify <- read.csv("spotify-2023.csv")
library("ggplot2")
library(dplyr)
library(tidyr)
options(scipen = 20)

# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 <- spotify %>%
  filter(released_year == "2023" & released_month <= 3)

ggplot(df1, aes(y = as.numeric(streams), x = as.character(artist_count))) + 
  geom_boxplot(fill = "lightpink1") + theme_bw()  + 
  labs(title = "Rozkład liczby odtworzeń piosenek w zależności od liczby wykonawców",
  subtitle = "(Pierwszy kwartał 2023r)",
       x = "Liczba wykonawców",
       y = "Liczba odtworzeń")

# Dla 2 wykonawców mamy najwiekszą różnorodność w liczbie odtworzeń piosenek. Dla 1 wykonawcy mamy najwięcej
# wartości odstajacych od normy.


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 <- spotify %>%
  filter(released_year >= 2019 & released_year <= 2022) %>%
  mutate(weekday = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-"), format="%Y-%m-%d"))) %>% 
  group_by(released_year,weekday) %>% 
  summarise(n = n())


ggplot(df2, aes(x=weekday, y=n, fill=as.factor(released_year))) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  labs(title = "Rozkład liczby wypuszczonych piosenek względem dnia tygodnia",
       subtitle = "Lata 2019-2022",
       y = "Liczba wypuszconych piosenek",
       x = "Dni tygodnia",
       fill = "Lata") 

# W 2022r wydano najwięcej piosenek w bez względu na dzień tygodnia. Jeżeli chodzi o dzień tygodnia z najwiekszą
# liczba wypuszczonych piosenek to jest to piątek.



# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3 <- spotify %>%
  mutate(m = as.numeric(streams)/in_spotify_playlists) %>%
  filter(m >= quantile(m,0.8,na.rm = TRUE))

ggplot(df3, aes(x = mode, y = bpm)) + 
  geom_violin(fill = "plum3") + theme_bw()  + 
  labs(title = "Rozkład tempa względem skali",
       x = "Skala",
       y = "Tempo")

# Rozkład tempa dla obu skal jest podobny. Najwięcej piosenek dla ubu skal ma tempo nieco ponad 125.
# Wykres dla skali Minor jest wyższy, ale bardzo wąski co sugeruje występowanie pojedyńczych wartości 
# odstających. Zakres skali Minor to 75-200 a Major okolo 70-190.


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 <- spotify %>%
  group_by(artist.s._name) %>%
  summarise(suma = sum(as.numeric(streams), na.rm = TRUE)) %>%
  arrange(-suma) %>%
  head(10)


ggplot(df4,aes(x=reorder(artist.s._name,-suma),y=suma)) +
  geom_col(fill = "plum4") +
  labs(title = "Top 10 artystów",
       x = "Artyści",
       y = "Liczba odtworzeń ich piosenek") + theme_bw() +
  scale_x_discrete(guide = guide_axis(angle = 30))

# Najpopularniejszym artystą jest The Weeknd, póżniej Tylor Swift i Ed Sheeran. Liczba odtworzeń dla tych 3
# artystów różni się nieznacznie.


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?


ggplot(spotify, aes(x = danceability_., y = energy_.,color=artist_count)) + 
  geom_point() +
  labs(title = "Zleżność energetyczności od taneczności piosenek",
                    x = "Taneczność",
                    y = "Energetyczność",
                    color = "Liczba artystów") + 
  scale_color_gradient(low="lightpink",high="navyblue") + theme_bw() 

# Wartości krańcowe (najwięszka, najmniejsza taneczność i energetyczność) należą do piosenek z 4-2 artystami.
# Mówiąc ogólnie najwięcej piosenek ma taneczność w przedziale ok. 60-85 i energetyczność w przedziale ok. 50-90.


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?


df6 <- spotify %>%
  filter(released_year >= 2013 & released_year <= 2023) %>%
  group_by(released_year, track_name) %>%
  summarise(max = max(as.numeric(streams)))%>%
  filter(max == max(max))

ggplot(df6, aes(x= as.factor(released_year) ,y=max))+geom_col(fill = 'plum4') + 
  geom_text(aes(label = track_name ), hjust = 1.5, colour = "white")  +
  labs(title = "Najpopularniejsze piosenki w latach 2013 - 2023",
       x = "Lata",
       y = "Liczba odtworzeń") + coord_flip()

# Najpopularniejsze piosenki zaczynając od 2013r to: Take Me To Church, Thinking Out Loud, Love Yourself,
# One Dance, Shape of You, Someone You Loved, Blinding Lights, Heat Waves, STAY, As It Was, Flowers.
# Najpopularniejsza piosenka jest z 2019r. Blinding Lights.



# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

df7 <- spotify %>%
  filter(released_year == "2022") %>%
  group_by(released_month) %>%
  summarise(suma_spotify = sum(in_spotify_playlists), suma_apple = sum(in_apple_playlists)) %>%
  pivot_longer(cols = c(suma_apple, suma_spotify))

ggplot(df7, aes(x=as.factor(released_month), y=value, fill=name)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() + scale_y_log10() +
  labs(title = "Zależność pomiędzy sumą piosenek na playlistach na spotify a apple",
       y = "Liczba piosenek (skala logarytmiczna)",
       x = "Miesiące") + scale_fill_discrete(name = "", labels = c("Apple","Spotify"))

# Piosenek na playlistach na spotify jest wiećej niż na playlistach na apple bez względu na miesiąc. 
# W 5 miesiącu (Maju) było najwięcej piosenek zarówno na spotify jak i na apple.
