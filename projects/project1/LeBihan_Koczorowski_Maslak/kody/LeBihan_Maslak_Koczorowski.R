library(dplyr)
library(ggplot2)
library(readr)
library(reshape2)
library(stringr)

# Ramka dla IMDB
movies <- read.csv("movies.csv")

# Ramka dla Filmwebu
Movies_2023_03_20 <- read.csv("Movies_2023_03_20.csv")
# Najbardziej popularne gatunki na IMDB

movies %>% 
  group_by(genre) %>% 
  summarise(ilosc = length(rating)) %>% 
  arrange(desc(ilosc)) -> tabela2
# Wśród pojedynczych gatunków najbardziej popularne są "Drama", "Comedy" i "Horror"

movies %>% 
  filter(genre == "Drama" | genre == "Comedy" | genre == "Horror") -> dobre_gatunki2

# Przeciągnięcie danych o gatunkach zIMDb dla Flimwebu

filmweb <- inner_join(movies, Movies_2023_03_20, by = c("title" = "Original_title"))

# Najbardziej popularne gatunki dla Filmwebu
filmweb %>% 
  group_by(genre) %>% 
  summarise(ilosc = length(rating)) %>% 
  arrange(desc(ilosc)) -> tabela3
# Wśród pojedynczych gatunków najbardziej popularne są również "Drama", "Comedy" i "Horror"

filmweb %>% 
  filter(genre == "Drama" | genre == "Comedy" | genre == "Horror") -> dobre_gatunki3

# Wybór tylko interesujących nas kolumn

dobre_gatunki2 <- dobre_gatunki2 %>% 
  select(title, genre, rating)

dobre_gatunki3 <- dobre_gatunki3 %>% 
  select(title, genre, rating) 

# Dodanie kolumny z nazwą platformy

kol2 <- rep("IMDB", 754)

dobre_gatunki2 <- cbind(dobre_gatunki2, kol2)

kol3 <- rep("Filmweb", 202)

dobre_gatunki3 <- cbind(dobre_gatunki3, kol3)

# Zmiana nazwy kolumny

dobre_gatunki2 <- dobre_gatunki2 %>% 
  rename("filmweb_czy_imdb" = "kol2")

dobre_gatunki3 <- dobre_gatunki3 %>% 
  rename("filmweb_czy_imdb" = "kol3")

# Połaczenie ramek

data <- rbind(dobre_gatunki2, dobre_gatunki3)

# Wykres

wykres <- ggplot(data, aes(x=genre, y=rating, fill=filmweb_czy_imdb))+
  geom_boxplot(color = "white")+
  theme_minimal()+
  labs(title = "Ratings for different genres") +
  theme(legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        legend.position = "left",
        plot.title = element_text(color = "white", size =14, face = "bold"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white")) +
  scale_fill_manual(values = c("#fcc200", "#806C00"), name="Website")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  xlab("Genre") +
  ylab("Rating")

#ggsave("gatunki4.png", wykres, bg = "transparent")

filmweb %>% 
  filter(Average_rating != "No_infromation") %>% 
  mutate(rating_f = as.numeric(Average_rating)) %>%
  arrange(-rating,-rating_f) %>% 
  select(title, rating, rating_f) %>% 
  mutate(roznica = rating - rating_f) %>%
  mutate(cat = case_when(row_number() <= 1000 ~ "Top 1000 movies", 
                         row_number() <= 2000 ~ "mid", 
                         row_number() <= 3000 ~ "Bottom 1000 movies")) %>% 
  filter(cat != "mid") %>% 
  ggplot() +
  geom_density(mapping = aes(x = rating),adjust = 3, colour = "#806C00",
               fill = "#806C00",linewidth = 2, alpha = 0.6)+
  geom_density(mapping = aes(x = rating_f),adjust = 3, colour = "#FCC200",
               fill = "#FCC200",linewidth = 2, alpha = 0.4) +
  
  labs(title = "Density plot of average movie ratings for each platform",
       subtitle = "Best and worst 1000 movies (out of 3000 movies in total)",
       x = "Average rating",  y = "Density", colour = "Website") +
  facet_wrap(~cat)+
  theme_minimal()+
  theme(
    panel.background = element_rect(fill='transparent', colour = "transparent"), 
    plot.background = element_rect(fill='transparent',colour = "transparent"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent'), 
    legend.box.background = element_rect(fill='transparent'), 
    strip.text = element_text(colour = "white", size = 18),
    title = element_text(colour = "white", size = 20),
    axis.text = element_text(colour = "white",size = 13),
    legend.title = element_text(colour = "white",size = 13),
    legend.text = element_text(colour = "white",size = 12)) +
  guides(fill = guide_legend(override.aes = list(color = "transparent")))-> p

#ggsave('myplot.png', p, bg='transparent')

filmweb %>% 
  select(rating, Average_rating) %>% 
  mutate(miejsce_imdb = rank(-rating)) %>% 
  mutate(miejsce_filmweb = rank(desc(Average_rating))) %>% 
  ggplot(aes(x = miejsce_filmweb, y = miejsce_imdb)) + 
  geom_density2d_filled(contour_var = "ndensity", colour = "black", bins = 9) +
  labs(title = "Relationship between ranking on different websites", subtitle = "By user reviews",
       x = "Filmweb ranking", y = "IMDb ranking")+
  scale_fill_brewer(palette = "YlOrBr") +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill='transparent'), 
    legend.box.background = element_rect(fill='transparent'),
    legend.position = "none",
    title = element_text(colour = "white", size = 20),
    axis.text = element_text(colour = "white",size = 13),
    axis.ticks = element_blank(),
    axis.title.y = element_text(vjust = 2)
  ) -> q

#ggsave('myplot2.png', q, bg='transparent')

years <- filmweb %>% 
  mutate(year = str_replace_all(year, "[\\(\\)\\I\\ \\X]", "")) %>% 
  filter(year != "V2013", year != "V2012", year != "V2015", year != "V2016") %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(dekada = ifelse(year <= 2000, 10 * ((year - 1 - 1900) %/% 10), 10 * ((year - 1 - 2000) %/% 10)))

dec <- years %>% 
  filter(dekada <= 10 | dekada >= 70) %>% 
  mutate(dekada = as.factor(dekada)) %>%
  mutate(dekada = forcats::fct_relevel(dekada, "70", "80", "90", "0", "10")) %>% 
  group_by(dekada) %>% 
  summarize(sredniafilmweb = mean(as.numeric(Average_rating)), sredniaimdb = mean(rating)) %>% 
  melt(id.vars = 1) %>% 
  ggplot(aes(x = dekada, y = value, fill = variable)) +
  geom_col(position = "dodge") +
  labs(x = "Decade", y = "Average rating", title = "Average rating over decades", color = "white") +
  scale_fill_manual(values = c("sredniaimdb" = "#806C00", "sredniafilmweb" = "#FCC200"),
                    name = "Website", labels = c("Filmweb", "IMDb")) + 
  theme_minimal() +
  theme(legend.text = element_text(color = "white"),
        legend.title = element_text(color = "white"),
        plot.title = element_text(color = "white", size = 14, face = "bold"),
        axis.title = element_text(color = "white"), 
        axis.text = element_text(color = "white")) +
  scale_y_continuous(limits = c(0, 8), breaks = seq(6, 8, by = 1)) +
  theme(panel.grid = element_line(size = 0.1)) +
  theme(plot.title = element_text(size = 11),
        legend.text = element_text(size = 6.5),  
        legend.title = element_text(size = 8.5),
        axis.title = element_text(size = 8))

#ggsave("dekada.png", dec, bg = "transparent")