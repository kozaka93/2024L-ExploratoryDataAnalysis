spells <- read.csv("Spells.csv")
places <- read.csv("Places.csv")
movies <- read.csv("Movies.csv")
dialogue <- read.csv("Dialogue.csv")
chapters <- read.csv("Chapters.csv")
characters <- read.csv("Characters.csv")
data_dictionary <- read.csv("Data_Dictionary.csv")

library(dplyr)
library(stringi)
#install.packages("fmsb")
library(fmsb)

# zamieniam kolejność wierszy, ponieważ gdy chcę wyciągnąć zaklęcia z tekstu,
# to gdy trafia np. na Lumos (który jest pierwszy z kolei) to nie sprawdza, czy
# jest coś dalej, tzn. z Lumos Maxima wyciągnełoby tylko Lumos
spells <- spells[c(1:28,30,29,32,33,31,34:43,45,46,44,47:61),]

# zauważyłam, że w tekście Totalum jest z dużej litery, więc przez to nie wyłąpywało tego zaklęcia
spells[spells$Incantation=="Protego totalum", "Incantation"] <- "Protego Totalum"

dialogue_spells <- dialogue %>% 
  mutate(spells_count = ifelse(grepl(paste(spells$Incantation, collapse='|'), dialogue$Dialogue),
                               stri_count_regex(dialogue$Dialogue, paste(spells$Incantation, collapse='|')),
                               0))
dialogue_spells %>% 
  filter(spells_count!=1,spells_count!=0) %>% 
  select(Dialogue,spells_count)

dialogue_spells <- dialogue_spells[rep(1:nrow(dialogue_spells), dialogue_spells$spells_count),]

dialogue_spells <- dialogue_spells %>% 
  mutate(spell = ifelse(stri_detect_fixed(row.names(dialogue_spells),"."),
                        stri_extract_last_regex(dialogue_spells$Dialogue, paste(spells$Incantation, collapse='|')),
                        stri_extract_first_regex(dialogue_spells$Dialogue, paste(spells$Incantation, collapse='|'))))

dialogue_spells <- dialogue_spells %>% 
  left_join(characters, by = join_by(Character.ID)) %>%
  select(-spells_count) %>% 
  left_join(places, by = join_by(Place.ID)) %>% 
  left_join(chapters, by = join_by(Chapter.ID)) %>% 
  left_join(movies, by = join_by(Movie.ID)) %>% 
  left_join(spells, by = join_by("spell" == "Incantation"))


# WYKRES

df <- dialogue_spells %>% 
  group_by(Movie.ID) %>% 
  summarise(n=n())

max_n <-  max(df$n)
data <-  as.data.frame(rbind(rep(max_n,8), rep(0,8), df$n))
colnames(data) <- c("1","2","3","4","5","6","7","8")

#zmiana tła
par(bg = "black")

radarchart(data, 
           pcol="orange", #zmiana koloru linii
           pfcol=rgb(1,0.6,0,0.6), #zmiana koloru wypełnienia
           plwd=5, #zmiana grubości kolorowej linii
           cglcol="white", #zmiana koloru siatki
           cglty=1, #zmiana typu siatki (ciągła, przerywana, itp.)
           cglwd=1, # zmiana grubości siatki
           vlcex=1) #zmiana wielkości napisów

