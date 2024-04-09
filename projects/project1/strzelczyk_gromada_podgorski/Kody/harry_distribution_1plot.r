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
library(forcats)
#install.packages("extrafont")
library(extrafont)
#font_import()
#fonts()
#install.packages("cowplot")
library(cowplot)
#install.packages("gridExtra")
library(gridExtra)
library(grid)

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

dialogue_spells %>% 
  group_by(Character.Name) %>% 
  summarise(n = n()) %>% 
  arrange(-n)

48/nrow(dialogue_spells) * 100

# Harry odpowiada za około 38% zaklęć w filmach

harry_density <- dialogue_spells %>% 
  filter(Character.Name == "Harry Potter") %>% 
  group_by(Movie.Title, Movie.Chapter) %>% 
  ggplot(aes(x=Movie.Chapter, group = fct_reorder(Movie.Title, Movie.ID), color=fct_reorder(Movie.Title,Movie.ID))) +
  geom_density(lwd = 2.5) +
  labs(x = "Movie chapter", y = element_blank(), title = "Distribution of the number of spells used by Harry Potter", color = "") +
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_),
        legend.background = element_rect(fill="transparent",
                                         colour = NA_character_),
        legend.text = element_text(color = "white", size = 13),
        plot.title = element_text(color = "white", size = 23),
        axis.text = element_text(color = "white"),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_),
        panel.grid.minor = element_line(color = "#545454"),
        panel.grid.major = element_line(color = "#545454"),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        text = element_text(family="Times New Roman"),
        axis.title.x = element_text(color = "white", size = 17)) +
  scale_color_manual(values = c("gray","#fcfc00","darkred","orange","#b9f8fa","#488a73"))

harry_density
ggsave(
  plot = harry_density,
  filename = "harry_density1.png",
  bg = "transparent",
  width = 3000, height = 1600, units = "px"
)
