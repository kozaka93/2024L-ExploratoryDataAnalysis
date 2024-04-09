# Wczytanie bibliotek ----------------------------------------------------------
library(dplyr)
library(hrbrthemes)
library(paletteer)
library(ggplot2)

# Wczytanie plików z danymi i edycja -------------------------------------------

books <- read.csv("converted_goodreads_books.csv")

# Wyrzucenie pierwszej kolumny
# Stara numeracja kolejności wierszy jest nieaktualna, bo się cykluje co plik

books <- books[, -1]

# Zamiana kolumny "author_id", aby nie była liczbą, lecz tekstem
books[,"author_id"] <- as.character(books[,"author_id"])

kaggle_authors <- read.csv("final_dataset.csv")
# Zamiana kolumny "author.id", aby nie była liczbą, lecz tekstem
kaggle_authors[,1] <- as.character(kaggle_authors[,1])

# Generowanie uproszczonych ramek ----------------------------------------------

boookss <- books %>% 
  mutate(title_length=nchar(title)) %>% 
  group_by(author_id) %>% 
  summarise(text_reviews=mean(text_reviews, na.rm = TRUE),
            average_rating=mean(average_rating, na.rm = TRUE),
            num_pages=mean(num_pages, na.rm = TRUE),
            first_publication_year=min(publication_year),
            title_length=mean(title_length))

kag_aut <- kaggle_authors %>% 
  filter(born!="") %>% 
  select(authorid, workcount, fan_count, average_rate, rating_count, review_count,
         born, died) %>% 
  mutate(died = ifelse(died=="", 2017, died)) %>% 
  mutate(born = as.numeric(substr(born, 1, 4)),
         died = as.numeric(substr(died, 1, 4))) %>% 
  mutate(ratepwork = rating_count/workcount,
         czestotliwosc = workcount/(died-born))  

# Wygenerowanie połączonej ramki do stworzenia wykresu -------------------------

fra <- boookss %>% 
  inner_join(kag_aut, by=join_by(author_id==authorid)) %>% 
  filter(rating_count > 1010000, review_count > 10000)


corr <- cor(fra[,-c(1, 5)], method = "spearman")
rownames(corr) <- c("Number of text reviews", "Average book rating", "Average number of pages",
                    "Average title length", "Number of books","Number of fans", 
                    "Average rating", "Number of ratings", "Number of reviews",
                    "Year of birth", "Year of death", 
                    "Rating per number of books", "Frequency of book publishing")
colnames(corr) <- rownames(corr)

# Wybór wierszy i kolumn w pożądanej kolejności i ustawieniu -------------------
corr_2 <-corr[c(1, 2, 6, 7, 8, 9), c(3, 4, 5, 10, 12, 13)] 


data <- expand.grid(X=rownames(corr_2), Y=colnames(corr_2))
data$Z <- unlist(as.list(corr_2))


# CORRELATION CHART FOR TOP 100 AUTHORS ----------------------------------------

# Wykres jak wygląda:
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_gradientn(colours = paletteer_c("grDevices::Blue-Red 2", 30),
                       limits = c(-1, 1),
                       name="Correlation")+
  theme_minimal()+
  labs(title="CORRELATION CHART FOR TOP 100 AUTHORS")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())+
  coord_equal()

# Wykres do Canvy z usuniętym tłem:

p <- ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile() +
  scale_fill_gradientn(colours = paletteer_c("grDevices::Blue-Red 2", 30),
                       limits = c(-1, 1),
                       name="Correlation")+
  labs(title="CORRELATION CHART FOR TOP 100 AUTHORS", color="white")+
  theme(axis.text.x = element_text(angle = 15, hjust = 1, color="white"),
        axis.text.y = element_text(color="white"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'),
        text=element_text(color="white"))+
  coord_equal()


# Zapis wykresu tak aby był przeźroczysty.

ggsave('myplot.png', p, bg='transparent')

# Gwoli ciekawości kim są ci top 100 autorzy -----------------------------------

# fra %>% 
#   left_join(kaggle_authors, by=join_by(author_id == authorid)) %>% 
#   select(name) %>% 
#   print(n=100)
