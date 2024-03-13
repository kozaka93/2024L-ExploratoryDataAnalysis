library(dplyr)
library(ggplot2)
episode_path <- "C:/Users/User/Desktop/wizualizacja/projekt1/himym_episodes.csv"
episode <- read.csv(episode_path)
imdb_path <- "C:/Users/User/Desktop/wizualizacja/projekt1/himym_imdb.csv"
imdb <- read.csv(imdb_path)

merged_df <- merge(episode, imdb, by.x = "title", by.y = "title")
#policzymy ile odcinków zosatlo wyprodukowanych przez każdego scricptwritera
episode_count <- merged_df %>%
  group_by(written_by) %>%
  summarise(episode_count = n())

episode_count <- as.data.frame(episode_count)
filtered_episode_count <- episode_count %>%
  filter(episode_count > 5)

#################################
episode_count <- merged_df %>%
  group_by(written_by) %>%
  summarise(episode_count = n())


ggplot(episode_count, aes(x = written_by, y = episode_count)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Zależność liczby odcinków napisanych przez scenarzystę",
       x = "Written By",
       y = "Liczba Odcinków") +
  geom_text(aes(label = episode_count), vjust = -0.5, size = 3)

unique_writers <- episode_count %>%
  filter(episode_count > 5) %>%
  pull(written_by)
#bierzmey tych co napiali wiecej niz 10 odcinków

# Filtrujemy ramkę danych merged_df, aby zawierała tylko dane dla tych scenarzystów
filtered_merged_df <- merged_df %>%
  filter(written_by %in% unique_writers)

# Grupujemy dane po scenarzyście i obliczamy średnią ocenę z imdb_rating
average_ratings <- filtered_merged_df %>%
  group_by(written_by) %>%
  summarise(average_imdb_rating = mean(imdb_rating))
episode_count_filtered <- episode_count %>%
  filter(episode_count > 5)

# Tworzymy wykres zależności liczby odcinków napisanych przez scenarzystę
ggplot(episode_count_filtered, aes(x = written_by, y = episode_count)) +
  geom_point(aes(size = episode_count)) +  # Ustawienie wielkości kropki na podstawie liczby odcinków
  scale_size_continuous(range = c(3, 10)) +  # Dostosowanie zakresu wielkości kropek
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  labs(title = "Zależność liczby odcinków napisanych przez scenarzystę (gdzie episode_count > 10)",
       x = "Napisane przez:",
       y = "Liczba Odcinków napisanych:")

##wykres gdzie wielkosc kropki to odpowienia ilosc odcinków napisana przez writera

###################################
# Wybieramy tylko te rekordy z ramki danych merged_df, które należą do scenarzystów z episode_count_filtered
filtered_merged_df <- merged_df %>%
  filter(written_by %in% episode_count_filtered$written_by)

# Grupujemy dane po scenarzyście i obliczamy średnią ocenę z imdb_rating
average_ratings_filtered <- filtered_merged_df %>%
  group_by(written_by) %>%
  summarise(average_imdb_rating = mean(imdb_rating))
##informacja jaka jest srednia ocena z imdb dla tych writerów 


merged_data <- merge(episode_count_filtered, average_ratings_filtered, by = "written_by")


# Tworzymy wykres zależności średniej oceny IMDb od scenarzysty (gdzie liczba napisanych odcinków > 10)
# Dodajemy na nim informacje o liczbie odcinków napisanych przez każdego z tych scenarzystów


merged_data %>%
  arrange(episode_count) %>%
  ggplot(aes(x = factor(written_by, levels = unique(written_by)), y = average_imdb_rating, size = episode_count, color = written_by)) +
  geom_point() +
  geom_text(aes(label = episode_count), vjust = -1, size = 4, color = "black") +  
  scale_size_continuous(range = c(3, 10), name = "Liczba napisanych odcinków") +
  scale_color_discrete(name = "Scenarzysta") +  
  theme(axis.text.x = element_blank(),  
        axis.ticks.x = element_blank()) +  
  labs(title = "Zależność ocen na IMDB w zależności od scenarzysty (gdzie liczba napisanych odcinków > 5)",
       y = "Średnia ocena odcinków wg IMDb Rating", x ='')
