# Wczytanie potrzebnych pakietów -----------------------------------------------

library(readr)
library(jsonlite)
library(dplyr)
library(stringr)

# Wczytanie pliku --------------------------------------------------------------

con <- read_lines("goodreads_books.json.gz")

# Wybór wierszy ----------------------------------------------------------------

# TUTAJ I TYLKO TUTAJ ZMIENIASZ WARTOŚCI
start = 2150000
ending = 2360655

# Utworzenie ramki do konwersji pliku ------------------------------------------

n <- ending - start + 1
frame <- data.frame("0", "0", "0", "0", "0",
                    "0", "0", "0", "0", "0",
                    "0")
colnames(frame) <- c("text_reviews","country_code", "average_rating", "author_id",
                     "num_pages","publication_day", "isbn13","publication_month",
                     "publication_year","book_id","title")

frame[n,]<-c(NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA,
             NA)
frame[1,]<-c(NA, NA, NA, NA, NA,
             NA, NA, NA, NA, NA,
             NA)

# Pętla wyszukująca dla każdego rekordu potrzebne informacje

for (i in start:ending){
  
  k <- i - start + 1
  
  # Podział z grubsza na kolumny
  vec <- str_split_1(con[i], ",")
  
  # Wybór potrzebnych kolumn
  vec <- vec[grepl("\"text_reviews_count\":|\"country_code\":|\"average_rating\":|\"author_id\":|\"num_pages\":|\"publication_day\":|\"isbn13\":|\"publication_month\":|\"publication_year\":|\"book_id\":|\"title\":", vec)]
  
  # rozważam tylko opcję, że wszystkie te kolumny istnieją
  if(length(vec)==11){
    
    # Pozbycie się nazw kolumn/cech z elementów do ramki i zostawienie tylko
    # potrzebncyh wartości
    for (j in 1:11){
      if(j==4){
        
        # kolumna z author_id jest trochę specjalna z uwagi na to, że
        # wynik wcześniejszego wycinania wygląda dla niej trochę inaczej
        numb <- grep("author_id", str_split_1(vec[4], '"'))
        part <- str_split_1(vec[j], '"')[numb + 2]
      }else{
        part <- str_split_1(vec[j], '"')[4]
      }
      vec[j] <- part
    }
    frame[k,] <- vec
  }
  
  # element do kontroli postępu prac
  if(k%%1000 == 0){
    print(paste("Algorytm wykonał już:", k, "iteracji"))
  }
}

# Pozbycie się wierszy z samymi wartościami NA
small_frame <- filter(frame, rowSums(is.na(frame))!=ncol(frame))


# Zapis do csv -----------------------------------------------------------------

nazwa <- paste0("books_", start, "_", ending, ".csv")
write.csv(small_frame, nazwa)
