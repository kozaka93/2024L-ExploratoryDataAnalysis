library(dplyr)
library(scales)

options(scipen=100, digits=4)
movies <- read.csv("rotten_tomatoes_movies.csv") %>% 
  filter(boxOffice != "" & is.na(audienceScore) == FALSE & is.na(tomatoMeter) == FALSE) %>% 
  mutate(
    boxOffice = case_when(
      substring(boxOffice, nchar(boxOffice), nchar(boxOffice)) == "K" ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)-1))*1000,
      substring(boxOffice, nchar(boxOffice), nchar(boxOffice)) == "M" ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)-1))*1000000,
      TRUE ~ as.numeric(substring(boxOffice, 2, nchar(boxOffice)))),
    Year = as.numeric(substring(releaseDateTheaters, 1, 4))
    )

seqs_cleaned <- read.csv("Movies - Cleaned.csv")

ordered_seqs <- seqs_cleaned %>%
  transmute(Title = Title,
            Year = Year,
            Order = Order,
            Previous_Index = if_else(Order == 1, NA, index - 1),
            First_Index = if_else(Order == 1, NA, index - Order + 1)
        ) %>% 
  left_join(seqs_cleaned, by = c("Previous_Index" = "index")) %>%
  transmute(Title = Title.x,
            Year = Year.x,
            Order = Order.x,
            Previous_Title = Title.y,
            Previous_Year = Year.y,
            First_Index = First_Index
            ) %>% 
  left_join(seqs_cleaned, by = c("First_Index" = "index")) %>% 
  transmute(Title = Title.x,
            Year = Year.x,
            Order = Order.x,
            Previous_Title = Previous_Title,
            Previous_Year = Previous_Year,
            First_Title = Title.y,
            First_Year = Year.y
            )

seq_box_office <- ordered_seqs %>%
  left_join(movies, by = c("Title" = "title", "Year" = "Year")) %>% 
  transmute(Title = Title,
            BoxOffice = boxOffice,
            Order = Order,
            Previous_Title = Previous_Title,
            Previous_Year = Previous_Year,
            First_Title = First_Title,
            First_Year = First_Year) %>% 
  left_join(movies, by = c("Previous_Title" = "title", "Previous_Year" = "Year")) %>% 
  transmute(Title = Title,
            BoxOffice = BoxOffice,
            Order = Order,
            Previous_Title = Previous_Title,
            Previous_BoxOffice = boxOffice,
            First_Title = First_Title,
            First_Year = First_Year) %>% 
  left_join(movies, by = c("First_Title" = "title", "First_Year" = "Year")) %>% 
  transmute(Title = Title,
            BoxOffice = BoxOffice,
            Order = Order,
            Previous_Title = Previous_Title,
            Previous_BoxOffice = Previous_BoxOffice,
            First_Title = First_Title,
            First_BoxOffice = boxOffice)


library(ggplot2)
ggplot(seq_box_office, aes(x = Previous_BoxOffice, y = BoxOffice)) +
  geom_point(color="#ffebd1", size = 3) +
  geom_smooth(method=lm, se = FALSE, color="#dfbb7e", size=3) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Oś Y w milionach
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Oś X w milionach
  labs(x = "Box office poprzedniej części", 
       y = "Box office nowej części", 
       title = "Zarobki filmów w zależności od zarobków prequeli") +
  theme(panel.background = element_rect(fill = "#4173ab"),  # Tło w kolorze #4173ab
        plot.background = element_rect(fill = "#4173ab"),   # Tło w kolorze #4173ab
        axis.text = element_text(color = "white"),          # Opisy osi białe
        axis.title = element_text(color = "white"),         # Tytuł osi biały
        plot.title = element_text(color = "white")
  )

ggplot(seq_box_office, aes(x = First_BoxOffice, y = BoxOffice)) +
  geom_point(color="#ffebd1", size = 3) +
  geom_smooth(method=lm, se = FALSE, color="#dfbb7e", size=3) +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Oś Y w milionach
  scale_x_continuous(labels = label_number(scale = 1e-6, suffix = "M")) +  # Oś X w milionach
  labs(x = "Box office pierwszej części", y = "Box office nowej części", title = "Zarobki filmów w zależności od zarobków części pierwszej") +
  theme(panel.background = element_rect(fill = "#4173ab"),  # Tło w kolorze #4173ab
        plot.background = element_rect(fill = "#4173ab"),   # Tło w kolorze #4173ab
        axis.text = element_text(color = "white"),          # Opisy osi białe
        axis.title = element_text(color = "white"),         # Tytuł osi biały
        plot.title = element_text(color = "white")
  )





