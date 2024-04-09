# Wczytanie potrzebnych pakietów

library(readr)
library(jsonlite)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(tidyr)
library(DescTools)
library(RColorBrewer)

# Wczytanie danych

json_data <-
  jsonlite::stream_in(file(
    "C:/Users/magda/Documents/projek/goodreads_book_authors.json"
  ))
df <- as.data.frame(json_data)
data <-
  read.csv("C:/Users/magda/Documents/projek/final_dataset.csv")

# Szukanie najpopularniejszych gatunków

data %>% 
  filter(fan_count > 4370, rating_count > 232673, review_count > 16069 & !is.na(genre)) %>% #wedlug kryteriów od Marka
  separate_rows(genre, sep=',') %>% 
  group_by(genre) %>% 
  summarise(n=n()) %>% 
  arrange(-n) %>% 
  filter(!(genre %in% c('fiction', 'non fiction','literature'))) %>% #usuwam te, które są zbyt ogólne
  top_n(10) %>% 
  ggplot(aes(reorder(genre,n),n))+
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Szukanie top 10 autorów

s <- {
  data %>%
    arrange(-review_count) %>%
    select(name, review_count) %>%
    top_n(20)
}
s <- as.data.frame(s)


k <- {
  data %>%
    arrange(-fan_count) %>%
    select(name, fan_count) %>%
    top_n(20)
}
k <- as.data.frame(k)


a <- {
  df %>%
    mutate(text_reviews_count = as.double(text_reviews_count)) %>%
    arrange(-text_reviews_count) %>%
    select(name, text_reviews_count) %>%
    top_n(20)
}
a <- as.data.frame(a)

autorzy <- c(a$name, k$name, s$name)
autorzy2 <- unique(sort(autorzy))
authors <-
  autorzy2[table(autorzy) == 3] # wektor authors to nasi autorzy

# Znajdowanie indeksow autorow

id <- {
  df %>%
    filter(name %in% authors) %>%
    select(author_id)
} # id to nasza lista id top 10 autorów
id <- as.vector(id)

# Tworzenie wykresu

books <-
  read.csv("C:/Users/magda/Documents/projek/converted_goodreads_books.csv")

r <- merge(books, df, by = "author_id", all.y = TRUE)
par(bg = NA)
r %>%
  filter(author_id %in% id[[1]]) %>%
  group_by(author_id) %>%
  mutate(author_id = as.character(author_id)) %>%
  mutate(sum = sum(text_reviews),
         percentage = sprintf('%.1f', max(text_reviews) / sum * 100)) %>%
  mutate(percentage = paste(percentage, '%', sep = '')) %>%
  mutate(top = max(text_reviews)) %>%
  mutate(rodzaj = if_else(text_reviews == max(text_reviews), "1", "0")) %>%
  ggplot(aes(reorder(name, sum), text_reviews, fill = rodzaj)) +
  geom_bar(position = 'stack',
           stat = 'identity',
           show.legend = FALSE) +
  
  theme(
    axis.text.x = element_text(
      angle = 60,
      hjust = 1,
      colour = 'white'
    ),
    axis.title.x = element_text(colour = 'white'),
    axis.title.y = element_text(colour = 'white'),
    axis.text.y = element_text(colour = 'white'),
    plot.title = element_text(hjust = 0.5, colour = 'white'),
    plot.subtitle = element_text(hjust = 0.5, colour = 'white'),
    panel.grid.major = element_line(colour = "white"),
    panel.grid.minor.x = element_line(colour = "white"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "transparent", colour =
                                      NA),
    plot.background = element_rect(fill = "transparent", colour =
                                     NA)
  ) +
  #rect = element_rect(fill = "transparent"))+
  
  labs(
    title = 'PERCENTAGE OF REVIEWS FOR THE BEST BOOK IN ALL REVIEWS',
    subtitle = 'FOR GOOD AUTHOR CANDIDATES',
    x = 'authors',
    y = 'reviews count'
  ) +
  scale_y_continuous(labels = label_number()) +
  geom_text(aes(y = top + 7000, label = percentage)) +
  scale_fill_manual(values = c('#ff0065',  '#99003C')) -> wykres
ggsave('finalnywykres0.png', wykres, bg = 'transparent', dpi = 1000)
