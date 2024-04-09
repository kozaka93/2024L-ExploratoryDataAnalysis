
library(dplyr)
sciezka <- "data/HIMYM.csv"
data <- read.csv(sciezka)

library(syuzhet)
library(tidytext)
library(tidyverse)
library(NLP)
library(tm)
library(dplyr)  
library(sentimentr)

#### Postawowe emocje rozpoznane za pomocą pakietu ####

emocje <- get_nrc_sentiment(data$Line)

uczucia_dane <- cbind(data, emocje)

#### Rozpoznawanie ręczne dodatkowych emocji ####

love_keywords <- c("love", "romance", "passion", "affection", "devotion")
friendship_keywords <- c("friend", "bff", "mate", "buddy", "pal")
hate_keywords <- c("hate", "dislike", "loathe", "detest", "despise")
betrayal_keywords <- c("betray", "backstab", "deceive", "cheat", "treachery")

#Miłość
Love <- data.frame(sapply(love_keywords, function(keyword) grepl(keyword, data$Line, ignore.case = TRUE))) %>%
  mutate(Love = ifelse(rowSums(.) == TRUE, 1, 0)) %>%
  select(Love)
uczucia_dane <- mutate(uczucia_dane,Love) %>%
  rename(love = Love)

#Przyjaźń
Friendship <- data.frame(sapply(friendship_keywords, function(keyword) grepl(keyword, data$Line, ignore.case = TRUE))) %>%
  mutate(Friendship = ifelse(rowSums(.) == TRUE, 1, 0)) %>%
  select(Friendship)
uczucia_dane <- mutate(uczucia_dane, Friendship) %>%
  rename(friendship = Friendship)

#Nienawiść
Hate <- data.frame(sapply(hate_keywords, function(keyword) grepl(keyword, data$Line, ignore.case = TRUE))) %>%
  mutate(Hate = ifelse(rowSums(.) == TRUE, 1, 0)) %>%
  select(Hate)
uczucia_dane <- mutate(uczucia_dane,Hate) %>%
  rename(hate = Hate)

#### Podsumowanie ####
summary_by_character <- aggregate(. ~ uczucia_dane$Character, data = uczucia_dane[, c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust","love","hate","friendship")], FUN = sum) %>%
  rename("Character" = "uczucia_dane$Character")
#### Liczba wypowiedzi bohaterów ####

wypowiedzi_wg_bohaterow <- data %>%
  group_by(Character) %>%
  summarise(speeches = n()) %>%
  arrange(-speeches)

#### Wyrażane uczucia względem liczby wypowiedzi ####
uczucia_dane_wzgledne <- summary_by_character %>%
  left_join(x = wypowiedzi_wg_bohaterow,y =  summary_by_character, by = join_by(Character)) %>%
  mutate(across(-1,~round(100*(. / speeches), digits = 2)))

#### wykres ####

#Przekształcanie danych:
data_long <- uczucia_dane_wzgledne %>%
  pivot_longer(cols = colnames(uczucia_dane_wzgledne)[-1][-1], names_to = "Emotion", values_to = "Value") %>%
  select(-speeches) %>%
  mutate(Character = forcats::fct_reorder(Character, Value))

ggplot(data_long, aes(x = Emotion, y = Value, fill = Character)) +
  geom_col(position = "dodge") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, color = "yellow"),
        axis.text.y = element_text(size = 12, color = "yellow"),
        axis.title.x = element_text(size = 14, face = "bold", color = "yellow"),
        axis.title.y = element_text(size = 14, face = "bold", color = "yellow"),
        plot.title = element_text(size = 18, face = "bold", color = "yellow"),
        legend.title = element_text(size = 14, face = "bold", color = "red"),
        legend.text = element_text(size = 12, color = "red"),
        axis.line = element_line(size = 1, color = "yellow")) +
  scale_fill_manual(values = c("Robin" = "green", "Lily" = "darkgreen", "Marshall" = "brown", "Ted" = "orange", "Barney" = "red")) +
  labs(title = "Feelings grouped by main characters. Percentage share of sentences expresseing a spcecific feeling",
       x = "Emotion", 
       y = "Percentage share in total number of speeches (%)") +
  theme(panel.background = element_rect(fill = rgb(76/255,
                                               65/255,
                                               136/255), linetype = "solid"))+
  theme(plot.background = element_rect(fill = rgb(76/255,
                                                  65/255,
                                                  136/255)))
