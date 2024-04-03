spotify <- read.csv("spotify-2023.csv")
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library('RColorBrewer')
options(scipen = 12)
spotify$streams[575] = NA
spotify$streams = as.integer(spotify$streams)
# Zadanie 1 ---------------------------------------------------------------
# Jak wygląda rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale 
# w zależności od liczby wykonawców?

df1 <- spotify %>% filter(released_year == 2023, released_month<4)

ggplot(df1, aes(x = streams/1000000000, fill = as.factor(artist_count))) +
  geom_density(alpha = 0.5) + labs(title ="Rozkład liczby odtworzeń piosenek opublikowanych w roku 2023 w pierwszym kwartale"
                                   ,fill = "Liczba wykonawców", x = "Liczba odtworzeń(w mld)", y="") +
  xlim(min(as.integer(spotify$streams), na.rm = T), max(as.integer(spotify$streams), na.rm = T)) + 
  theme_minimal()+ theme(axis.text.y=element_blank(),
                          axis.ticks.y=element_blank()) + 
  scale_x_continuous(expand=c(0,0))


# Zadanie 2 ---------------------------------------------------------------
# Jak wygląda rozkład liczby wypuszczonych piosenek względem dnia tygodnia dla poszczególnych lat między
# 2019 a 2022?

df2 <- spotify %>% 
  filter(released_year >= 2019 & released_year <= 2022) %>%
  mutate(day_of_week = weekdays(as.Date(paste(released_year, released_month, released_day, sep = "-")))) 

df2$day_of_week <- factor(df2$day_of_week, levels = c('poniedziałek', 'wtorek', 'środa', 
                                                      'czwartek','piątek','sobota','niedziela'))

ggplot(df2, aes(x = as.factor(released_year), fill = day_of_week)) +
  geom_bar() + scale_y_continuous(expand = c(0, 0)) +
  labs(title = "Rozklad liczby wypuszczonych piosenek względem dnia tygodnia w latach 2019-2022",
       x = "Rok", y = "Liczba wypuszczonych piosenek", 
       fill = "Dzień tygodnia") + theme_minimal() + scale_fill_brewer(palette = "Set3") 
  
# Zadanie 3 ---------------------------------------------------------------
# Jaki jest rozkład tempa względem skali ('mode') dla piosenek, które są w 20% 
# najczęściej odtwarzanych piosenek w przeliczeniu na liczbę playlist spotify?

df3 <- spotify %>% arrange(-in_spotify_playlists) %>% 
  slice_head(prop = 0.2)

ggplot(df3, aes(x = bpm, y = mode)) +
  geom_violin(fill = "#ccffc1") +
  scale_x_continuous(expand = c(0, 0)) +
  coord_flip() + theme_minimal() +
  labs(title = "Rozkład tempa względem skali dla piosenek, które są w 20% 
najczęściej odtwarzanych piosenek", x = "Tempo",
       y = "Skala") 


# Zadanie 4 ---------------------------------------------------------------
# Którzy artyści (osobno) mają najwięcej odtworzeń wszystkich swoich piosenek w sumie (top 10)?

df4 <- spotify %>% filter(artist_count == 1) %>% group_by(artist.s._name) %>% 
  summarise(sum = sum(streams)) %>% arrange(-sum) %>% head(10)

ggplot(df4, aes(x=sum/1000000000, y=fct_reorder(artist.s._name,sum))) + 
  geom_col( fill = '#bea09b') + theme_minimal() +
  labs(title = "Top 10 artystów z największą ilością odtworzeń wszystkich swoich piosenek",
       x = "Odtworzenia(w mld)", 
       y = "Artysta")+ scale_x_continuous(expand=c(0,0))


# Zadanie 5 ---------------------------------------------------------------
# Jak zależy energetyczność od tanecznośći patrząc przez pryzmat liczby artystów?

ggplot(spotify, aes(x = energy_., y = danceability_., color = as.factor(artist_count))) + 
  geom_point()+ theme_minimal() +
  labs(title = "Energetyczność a taneczność w zależności od liczby artystów",
       x = "Energiczność", 
       y = "Taneczność") + facet_wrap(~artist_count)+
  theme(legend.position="none")  


# Zadanie 6 ---------------------------------------------------------------
# Jaka piosenka były najbardziej popularna (liczba odtworzeń) w każdym roku w latach 2013 - 2023?

df6 <- spotify %>% filter(released_year >= 2013, released_year <= 2023) %>% group_by(released_year) %>% 
  arrange(-as.integer(streams)) %>% summarise(track_name=first(track_name), streams=first(streams))

ggplot(df6, aes(x=fct_rev(fct_reorder(track_name,-released_year)), y=streams/1000000000, fill = as.factor(released_year))) + 
  geom_col() +
  labs(title = "Najbardziej popularna piosenka w każdym roku w latach 2013 - 2023",
       x = "Tytuł", 
       y = "Liczba odtworzeń(w mld)", fill = "Rok")+ theme_minimal() +
  scale_y_continuous(expand=c(0,0)) +ylim(0,2.25) +
  scale_x_discrete(guide = guide_axis(n.dodge = 1, angle = -20))+ scale_fill_brewer(palette = "Set3")  

ggplot(df6, aes(x=as.factor(released_year), y=streams/1000000000, fill = fct_rev(fct_reorder(track_name,-released_year)))) + 
  geom_col() +
  labs(title = "Najbardziej popularna piosenka w każdym roku w latach 2013 - 2023",
       x = "Rok", 
       y = "Liczba odtworzeń(w mld)", fill = "Tytuł") + theme_minimal()+
  scale_y_continuous(expand=c(0,0)) +ylim(0,2.25)+ scale_fill_brewer(palette = "Set3") 

# Zadanie 7 ---------------------------------------------------------------
# Jak w 2022 roku wyglądała zależność pomiędzy sumą piosenek na playlistach na spotify a apple patrząc 
# po miesiącach?

 df7 <- spotify %>% filter(released_year == 2022) %>% mutate(
   is.spotify = case_when(in_spotify_playlists>0 ~ 1),
   is.apple = case_when(in_apple_playlists>0 ~ 1))
 
 df7 <- df7 %>% group_by(released_month) %>% 
   summarise(Spotify=sum(is.spotify, na.rm = T), Apple = sum(is.apple, na.rm = T))
 
 df7 <- pivot_longer(df7, cols = c(Spotify, Apple))
 
 ggplot(df7, aes(fill=name, y=value, x=as.factor(released_month))) + 
   geom_bar(position="dodge", stat="identity")+ labs(fill = "Platforma") + 
   scale_fill_manual(values = c('#202242','#a0aaa2'))+ theme_minimal() +
   labs(title = "Suma piosenek na playlistach na spotify i apple w 2022 roku",
        x = "Miesiąc", 
        y = "Suma piosenek")
 
