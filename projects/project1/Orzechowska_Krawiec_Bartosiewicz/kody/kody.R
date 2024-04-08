#pobranie bibliotek

library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
library(stringi)
library(forcats)
library(tidyr)
library(tidyverse)
library(wordcloud2)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(slam)
library(NLP)
library(stringr)

#------------------------  kod do utworzenia chmury slow --------------------------

# zaczytanie odpowiednich zbiorów danych
booksDS <- read.csv("books_1.Best_Books_Ever.csv")
negative <- read.csv("negative_words.csv")
positive <- read.csv("positive_words.csv")
negative_list <- str_to_upper(negative$word)
positive_list <- str_to_upper(positive$words)


# filtrowanie danych, aby zliczyc liczbe wystapien kazdego slowa

bookDS_desc <- booksDS$description

list_of_words <- list()
for (i in 1:length(bookDS_desc)){
  x <- bookDS_desc[i]
  x_cleaned <- gsub("[[:punct:]]", "", x)
  lista_wyrazow <- str_to_upper(str_split(x_cleaned, "\\s+")[[1]])
  list_of_words <- c(list_of_words, lista_wyrazow)
}

words_DS <- data.frame(word = unlist(list_of_words))
# --------------------
stop_words <- str_to_upper(c(
  "a", "about", "above", "after", "again", "against", "all", "am", "an", "and",
  "any", "are", "aren't", "as", "at", "be", "because", "been", "before", "being",
  "below", "between", "both", "but", "by", "can't", "cannot", "could", "couldn't",
  "did", "didn't", "do", "does", "doesn't", "doing", "don't", "down", "during",
  "each", "few", "for", "from", "further", "had", "hadn't", "has", "hasn't", "have",
  "haven't", "having", "the", "of", "I", "you", "he", "she", "it", "we", "they", "me", "you", "him", "her", "us", "them",
  "about", "above", "across", "after", "against", "along", "amid", "among", "around", "as", "at", "before", 
  "behind", "below", "beneath", "beside", "between", "beyond", "by", "down", "during", "except", "for", "from", 
  "in", "inside", "into", "like", "near", "of", "off", "on", "onto", "out", "outside", "over", "past", "through", 
  "to", "toward", "under", "underneath", "until", "up", "upon", "with", "within", "without",
  "I", "you", "he", "she", "it", "we", "they", "me", "you", "him", "her", "us", "them", 
  "my", "your", "his", "her", "its", "our", "their", 
  "mine", "yours", "his", "hers", "ours", "theirs", 
  "myself", "yourself", "himself", "herself", "itself", "ourselves", "yourselves", "themselves",
  "this", "that", "these", "those", 
  "who", "whom", "whose", "which", "what",
  "whoever", "whomever", "whichever", "whatever", "is", "his", "that", "who", "their", "one", "this", "will", 
  "when", "was", "its", "most", "only", "not", "can", "what", "more", "or", "own", "where", "no", "so", "than", "there", 
  "even", "if", "just", "ever", "our", "also", "how", "make", "every", "there", "would", "shes", "once", "yet", "very",
  "whose", "while", "set", "these", "these", "those", "well", "some", "hes", "novel", "book", "two", "editioin", "way",
  "other", "than", "three", "de", "get", "then", "que", " ", "were", "en", "too", "e", "un", "go", "y", "",
  "author", "series", "much", "something", "cant", "doesnt" ))
#----------------------
words_DS_filtered <- words_DS %>% filter(! word %in% stop_words)%>%
  filter(!is.na(word)) %>%
  group_by(word) %>% summarise(times = n()) %>% arrange(desc(times)) %>% mutate(scale = sqrt(times))
words_DS_filtered <- words_DS_filtered[-c(58, 78), ]


# tworzenie podzialu na wyrazy pozytywne i negatywne 

words_DS_positive <- words_DS %>% filter(word %in% positive_list)
words_DS_negative <- words_DS %>% filter(word %in% negative_list)
num_pos = sum(words_DS_positive$num)
num_neg = sum(words_DS_negative$num)
num_positive <- nrow(words_DS_positive)
num_negative <- nrow(words_DS_negative)
words_DS_positive <- words_DS_positive %>% group_by(word) %>% summarise(num = n()) %>% arrange(desc(num)) %>% mutate(scale = log(num))
words_DS_negative <- words_DS_negative %>% group_by(word) %>% summarise(num = n()) %>% arrange(desc(num)) %>% mutate(scale = log(num))


color_palette <- c("#f5c886", "#FBE9CE", "#ab8c5d")


# wybranie 100 najczesciej wystepujacych 
wordsDS_filtered_first_100 <- words_DS_filtered[1:100,]
words_DS_negative_first_100 <- words_DS_negative[1:100,]
words_DS_positive_first_100 <- words_DS_positive[1:100,]

wordcloud2(words_DS_negative_first_100, 
           shape = "diamond", 
           color = rep_len(color_palette, nrow(wordsDS_filtered_first_100)), 
           backgroundColor = "#42354C", size = 0.3)

wordcloud2(words_DS_positive_first_100, 
           shape = "diamond", 
           color = rep_len(color_palette, nrow(wordsDS_filtered_first_100)), 
           backgroundColor = "#42354C", size = 0.6)

color_palette <- c("#9B461F", "#6A1D2F", "#354C3C", "#42354C")

par(bg = "#f5c886")

wordcloud(words = wordsDS_filtered_first_100$word, freq = wordsDS_filtered_first_100$times,
          scale=c(3, 0.5), random.order = FALSE,
          fixed.asp = TRUE, rot.per = 0, family = "Helvetica-Bold", colors = color_palette)

wordcloud(words = words_DS_negative_first_100$word, freq = words_DS_negative_first_100$num,
          scale=c(3, 0.5), random.order = FALSE,
          fixed.asp = TRUE, rot.per = 0, family = "Helvetica-Bold", colors = color_palette)

wordcloud(words = words_DS_positive_first_100$word, freq = words_DS_positive_first_100$num,
          scale=c(3, 0.5), random.order = FALSE,
          fixed.asp = TRUE, rot.per = 0, family = "Helvetica-Bold", colors = color_palette)

write.csv(words_DS, file = "words_df")


#---------------------- kod do utworzenia wykresu słupkowego ----------------------

read.csv("books_1.Best_Books_Ever.csv") -> Best_Books_Ever
select(Best_Books_Ever, -bookId) -> Best_Books_Ever

Best_Books_Ever$genres -> Book_Genres

unlist(stri_extract_all_regex(Book_Genres, "'([[:alnum:]\\s]+)'")) -> Book_Genres

gsub("'", "", Book_Genres) -> Book_Genres

as.data.frame(table(Book_Genres)) -> Book_Genres_Freq

unique(Book_Genres) -> Book_Genres

#Mam już przygotowany wektor, który przechowuje wszystkie unikatowe nazwy gatunków

length(Book_Genres) #Łącznie jest 983 gatunków

# View(Best_Books_Ever)


# View(Book_Genres_Freq)

Book_Genres_Freq %>% 
  arrange(-Freq) %>% 
  head(50) -> Book_Genres_Freq_50

options(scipen = 12)

Book_Genres_Freq_50 %>% 
  mutate(Book_Genres = fct_reorder(Book_Genres, Freq, .desc = TRUE)) %>%
  filter(Book_Genres != "Audiobook") %>% 
  head(15) %>% 
  ggplot(aes(y = Book_Genres, x = Freq))+
  geom_col(fill = "#f5c886")+
  labs(x = "Number of occurrences",
       y = "Book genre",
       title = "15 most popular book genres")+
  theme(panel.background = element_rect(fill = "#42354c", color = "#fbe9ce"),
        panel.grid = element_blank())+
  theme(plot.background = element_rect(fill = "#42354C"))+
  theme(plot.title = element_blank())+
  theme(axis.text.y = element_text(color = "#fbe9ce", size = 22),
        axis.text.x = element_text(color = "#fbe9ce", size = 22),
        axis.title.x = element_text(color = "#fbe9ce", face = "bold", size = 24),
        axis.title.y = element_text(color = "#fbe9ce", face = "bold", size = 24))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, 35000))


#------------------------- kod do utworzenia mapy -----------------------------


dane_ksiazki <- read.csv("books_1.Best_Books_Ever.csv")

swiat_dane <- map_data("world")


#czyszczenie danych

lokalizacje <- dane_ksiazki %>%
  filter(setting != "[]") %>%
  select(setting)

swiat_dane <- swiat_dane %>%
  filter(long<=180) %>%
  mutate(region = tolower(region))

# Funkcja do przetwarzania tekstu
process_text <- function(text) {
  # Usunięcie znaków specjalnych
  text <- gsub("[^[:alnum:]' ]", "", text)
  # Usunięcie pojedynczych apostrofów
  text <- gsub("\\b'\\b", "", text)
  text <- gsub("'", "", text)
  # Podział tekstu na słowa
  words <- unlist(strsplit(text, "\\s+"))
  words <- tolower(words)
  return(words)
}

# zmiana nazwy na bardziej popularne

pomocnicze_kraje <- unique(swiat_dane$region)

pomocnicze_kraje[[238]] <- "United States"

pomocnicze_kraje[[81]] <- "England"


# przeprocesowanie tesktu

lokalizacje$setting <- lapply(lokalizacje$setting, process_text)

pomocnicze_kraje <- lapply(pomocnicze_kraje, process_text)

# Zliczenie liczby wystąpień krajów

liczba_wystapien_kraju = rep(0, length(pomocnicze_kraje))

for (k in 1:length(pomocnicze_kraje)){
  
  liczba_wystapien <- 0;
  
  for (i in 1:length(lokalizacje$setting)){
    
    
    czy_kraj_w_lokacji <- pomocnicze_kraje[[k]] %in% lokalizacje$setting[[i]];
    
    if (all(czy_kraj_w_lokacji)){
      liczba_wystapien = liczba_wystapien +1;
      czy_kraj_wystpil_gdziekolwiek = 1;
    }
    
  }
  liczba_wystapien_kraju[[k]] = liczba_wystapien;
}


# powrot do poczatkowych nazw z ramki danych 

pomocnicze_kraje[[238]] <- "usa"

pomocnicze_kraje[[81]] <- "uk"



for (k in 1:length(pomocnicze_kraje)){
  pomocnicze_kraje[[k]] <- paste(pomocnicze_kraje[[k]], collapse = " ")
}

pomocnicze_kraje <- unlist(pomocnicze_kraje)

pomocnicza_ramka <- data.frame(pomocnicze_kraje, liczba_wystapien_kraju)
colnames(pomocnicza_ramka) <- c("region", "liczba_wystapien_kraju")

dane_do_mapy <- left_join(swiat_dane, pomocnicza_ramka, by="region")

# tworzenie mapy

ggplot() + 
  geom_polygon(data = dane_do_mapy, aes(x = long, y = lat, group = group, fill = liczba_wystapien_kraju),
               color = "#354C3C") +
  geom_polygon(data = subset(dane_do_mapy, is.na(liczba_wystapien_kraju)), 
               aes(x = long, y = lat, group = group), fill = "#F3EEE3", color = "black") +
  coord_map("mollweide") +
  theme_void() +
  scale_fill_fermenter(palette = 8, trans = "log10", direction = 1, na.value = "#F3EEE3") +
  theme(legend.position = "top",
        legend.direction = "vertical",  # Dodanie tego argumentu
        panel.background = element_rect(fill = "#aedeff", color = "black", size = 0.5)) +
  labs(fill = "Liczba wystąpień krajów w najwyżej ocenianych książkach")



