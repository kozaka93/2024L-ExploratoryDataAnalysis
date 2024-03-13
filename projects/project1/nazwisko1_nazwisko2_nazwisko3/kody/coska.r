install.packages("Rtools")
install.packages("wordcloud")
install.packages("tm")
install.packages("slam")

library(dplyr)
library(stringr)
library(ggplot2)
library(wordcloud)
library(tm)
library(slam)
library(ggplot2)

data <- read.csv("HIMYM.csv")



data %>%
  filter(str_detect(tolower(Line), "love")) %>% 
  filter(Season<7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(fill = rgb(122/255,100/255,122/255)) +
  labs(x = "Season", y = "no. of episode", title = "Distribution of lines containing 'love' across seasons")

#########################

data %>%
    filter(str_detect(tolower(Line), "father")) %>% 
  filter(Season<7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(fill = rgb(122/255,100/255,122/255)) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "no. of episode", title = "Distribution of lines containing 'father' across seasons")


###########################
data %>%
  filter(str_detect(tolower(Line), "papa")) %>% 
  filter(Season<7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(fill = rgb(122/255,100/255,122/255)) +
  labs(x = "Season", y = "no. of episode", title = "Distribution of lines containing 'papa' across seasons")

###################################

data %>%
  filter(str_detect(tolower(Line), "five")) %>% 
  filter(Season<7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(fill = rgb(122/255,100/255,122/255)) +
  labs(x = "Season", y = "no. of episode", title = "Distribution of lines containing 'five' across seasons")


##########################

data %>%
  filter(str_detect(tolower(Line), "father")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_boxplot(fill = rgb(122/255,100/255,122/255)) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")
############################

data %>%
  filter(str_detect(tolower(Line), "bride")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "wedding")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'wedding' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "car")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")


############################

data %>%
  filter(str_detect(tolower(Line), "roommate")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "job")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "bar ")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'bar' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "architect")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")


############################

data %>%
  filter(str_detect(tolower(Line), "stinson")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "ted")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "swarley")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")


############################

data %>%
  filter(str_detect(tolower(Line), "Mosby")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), " wait ")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "father")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season)),
              fill = rgb(122/255,100/255,122/255)) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")


############################

data %>%
  filter(str_detect(tolower(Line), "mother")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season)),
              fill = rgb(122/255,100/255,122/255)) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'mother' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "legendary")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "legen...")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "wait for it")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season)),
              fill = rgb(122/255,100/255,122/255)) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'wait for it' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "true story")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "pumpkin")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

############################

data %>%
  filter(str_detect(tolower(Line), "captain")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")


############################

data %>%
  filter(str_detect(tolower(Line), "umbrella")) %>% 
  filter(Season < 7) %>% 
  group_by(Season, Episode) %>%
  summarise(suma = n()) %>%
  ggplot(aes(x = as.factor(Season), y = Episode)) +
  geom_violin(aes(x = as.factor(Season))) +
  scale_x_discrete(drop = FALSE) +
  labs(x = "Season", y = "Episode Number", title = "Distribution of lines containing 'father' across seasons")

