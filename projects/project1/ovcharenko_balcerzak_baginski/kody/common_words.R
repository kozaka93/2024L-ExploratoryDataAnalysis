install.packages("tm")
install.packages("SnowballC") 
install.packages("RColorBrewer")
install.packages("stringr")
install.packages("devtools")
install.packages("pak")

library(tm)
library(SnowballC)
library(RColorBrewer)
library(dplyr)
library(stringr)
library(devtools)
library(pak)

pkg_install("rlang")
install_github("lchiffon/wordcloud2")

library(wordcloud2)

bb_quotes <- read.csv("bb_quotes.csv") 
english_stopwords <- read.delim("stopwords-en.txt") %>% setNames("stopword") %>% 
  pull(stopword)
my_stopwords <- read.delim("my_stopwords.txt") %>% setNames("stopword") %>% 
  pull(stopword)
stopwords <- c(english_stopwords, my_stopwords, gsub("'", "", stopwords("english")))

walter_quotes <- bb_quotes %>% 
  filter(actor == "Walter") %>% 
  pull(text)

walter_quotes <- paste(walter_quotes, sep = '', collapse = '')
walter_quotes <- gsub("'", "", walter_quotes)
walter_quotes <- gsub('"', "", walter_quotes)
walter_quotes <- gsub("[[:punct:]]", " ", walter_quotes)
walter_quotes <- walter_quotes %>% removeNumbers() %>% tolower() %>% str_squish()
walter_quotes <- walter_quotes %>% strsplit(split = " ") %>% as.data.frame() %>% 
  setNames("word")
walter_quotes <- walter_quotes %>% group_by(word) %>% summarise(freq = n()) %>% 
  filter(!(word %in% stopwords)) %>% arrange(-freq)

jesse_quotes <- bb_quotes %>% 
  filter(actor == "Jesse") %>% 
  pull(text)

jesse_quotes <- paste(jesse_quotes, sep = '', collapse = '')
jesse_quotes <- gsub("'", "", jesse_quotes)
jesse_quotes <- gsub('"', "", jesse_quotes)
jesse_quotes <- gsub("[[:punct:]]", " ", jesse_quotes)
jesse_quotes <- jesse_quotes %>% removeNumbers() %>% tolower() %>% str_squish()
jesse_quotes <- jesse_quotes %>% strsplit(split = " ") %>% as.data.frame() %>% 
  setNames("word")
jesse_quotes <- jesse_quotes %>% group_by(word) %>% summarise(freq = n()) %>% 
  filter(!(word %in% stopwords)) %>% arrange(-freq)

saul_quotes <- bb_quotes %>% 
  filter(actor == "Saul") %>% 
  pull(text)

saul_quotes <- paste(saul_quotes, sep = '', collapse = '')
saul_quotes <- gsub("'", "", saul_quotes)
saul_quotes <- gsub('"', "", saul_quotes)
saul_quotes <- gsub("[[:punct:]]", " ", saul_quotes)
saul_quotes <- saul_quotes %>% removeNumbers() %>% tolower() %>% str_squish()
saul_quotes <- saul_quotes %>% strsplit(split = " ") %>% as.data.frame() %>% 
  setNames("word")
saul_quotes <- saul_quotes %>% group_by(word) %>% summarise(freq = n()) %>% 
  filter(!(word %in% stopwords)) %>% arrange(-freq)

set.seed(1111)

wordcloud2(walter_quotes %>% top_n(300), size = 0.34, figPath = "cactus1.png", 
           color = rep_len(c("#0D3E10", "#2f5318", "#8F9C32", "#B2AE21"), 300), 
           backgroundColor = "#906F50")

wordcloud2(jesse_quotes %>% top_n(300), size = 0.65, figPath = "cactus3.png", 
           color = rep_len(c("#0D3E10", "#2f5318", "#8F9C32", "#B2AE21"), 300), 
           backgroundColor = "#906F50")

wordcloud2(saul_quotes %>% top_n(300), size = 0.3, figPath = "cactus4.png", 
           color = rep_len(c("#0D3E10", "#2f5318", "#8F9C32", "#B2AE21"), 300), 
           backgroundColor = "#906F50")


