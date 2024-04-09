spells <- read.csv("Spells.csv")
places <- read.csv("Places.csv")
movies <- read.csv("Movies.csv")
dialogue <- read.csv("Dialogue.csv")
chapters <- read.csv("Chapters.csv")
characters <- read.csv("Characters.csv")
data_dictionary <- read.csv("Data_Dictionary.csv")

library(dplyr)
library(ggplot2)
library(stringi)
library(networkD3)
library(forcats)
#install.packages("extrafont")
library(extrafont)

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

df1 <- dialogue_spells %>% 
  filter(Wand..Core. != "") %>% 
  group_by(Light, Wand..Core.) %>% 
  summarise(n = n())
df1
links <- data.frame(
  source=df1$Wand..Core., 
  target=df1$Light, 
  value=df1$n)

nodes <- data.frame(
  name=c(as.character(links$source), 
         as.character(links$target)) %>% unique())

links$IDsource <- match(links$source, nodes$name)-1 
links$IDtarget <- match(links$target, nodes$name)-1

links$group <- as.factor(c("a","b","c","a","b","d","c","a","a","b","d","a","b","a","b","d",
                           "b","a","a","b","a","b","a","b","c","b","c","b","a","b","d","c"))

nodes$group <- as.factor(c("nodes"))

my_color <- 'd3.scaleOrdinal() .domain(["a","b","c","d","nodes"]) .range(["#7d0804","#ebae21","#7a3638","#f5e102", "grey"])'

p <- sankeyNetwork(Links = links, Nodes = nodes, Source = "IDsource", Target = "IDtarget", 
                   Value = "value", NodeID = "name", 
                   colourScale=my_color, LinkGroup="group", NodeGroup="group", fontSize = 0,
                   nodeWidth = 2)
p
