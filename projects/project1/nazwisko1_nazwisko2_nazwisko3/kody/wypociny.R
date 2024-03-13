
library(dplyr)

install.packages(c("slam", "tm","wordcloud","tidyr"))
library(rvest)

matka <- read.csv("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Ćwiczenia/My mother/kod/HIMYM.csv")

slowa <- matka %>%
  pull(Line) %>%
  paste(collapse = " ")
slowa_czyste <- gsub("[[:punct:]]","",slowa)
slowa_czyste <- gsub("You","",slowa_czyste)

zbior_tabel[[2]]
library(tm)
library(slam)
library(wordcloud)
library(tm)

wordcloud(words = unlist(strsplit(slowa_czyste," ")),min.freq = 50 ,random.order = FALSE, colors = brewer.pal(8, "Dark2"))

unlist(strsplit(slowa," "))
slowa


#### TED ####

slowa <- matka %>%
  filter(Character == "Ted") %>%
  pull(Line) %>%
  paste(collapse = " ")
slowa_czyste <- gsub("[[:punct:]]","",slowa)
slowa_czyste <- gsub("You","",slowa_czyste)

wordcloud(words = lista_ted,min.freq = 80 ,random.order = FALSE, colors = brewer.pal(7, "Dark2"))

#### Marschall ####

slowa <- matka %>%
  filter(Character == "Marshall") %>%
  pull(Line) %>%
  paste(collapse = " ")
slowa_czyste <- gsub("[[:punct:]]","",slowa)
slowa_czyste <- gsub("You","",slowa_czyste)

wordcloud(words = unlist(strsplit(slowa_czyste," ")),min.freq = 50 ,random.order = FALSE, colors = brewer.pal(7, "Dark2"))
?wordcloud
