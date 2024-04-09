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
font_import()
fonts()

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


#####################################################################
#obrazki

# wykres 9
#install.packages("ggimage")
library(ggimage)

df_obrazki <- dialogue_spells %>% 
  group_by(Character.Name) %>% 
  summarise(count = n()) %>% 
  arrange(-count) %>% 
  head(7)

# #df_obrazki <- df_obrazki %>% mutate(image = c("https://dzielneswietliki.wordpress.com/wp-content/uploads/2019/11/harry-potter-png-background.png?w=960",
#                                       "https://static.wikia.nocookie.net/deathbattlefanon/images/a/a4/HermioneGranger.png/revision/latest/scale-to-width-down/1000?cb=20220415204756",
#                                       
#                                       "https://static.wikia.nocookie.net/polski-dubbing/images/a/aa/Remus_Lupin.png/revision/latest?cb=20180411120642&path-prefix=pl",
#                               
#                               "https://static.wikia.nocookie.net/polski-dubbing/images/1/10/Ron_Weasley.png/revision/latest?cb=20170811132414&path-prefix=pl",
#                                       "https://www.pngitem.com/pimgs/b/221-2219298_matthew-lewis-as-neville-longbottom-from-harry-potter.png",
#                               "https://static.wikia.nocookie.net/one-minute-meelee-fanon/images/4/48/Dumbledore.png/revision/latest?cb=20221226064038",
#                                       "https://d2bzx2vuetkzse.cloudfront.net/fit-in/0x450/images_without_background/88677f5e-8b7c-4b86-8905-e312f582708b.png"
#                                       ))

obrazki <- ggplot(df_obrazki, aes(x=fct_reorder(Character.Name, count), y=count)) +
  geom_col(width = 0.7, fill = 'orange') +
  coord_flip() +
  #geom_image(aes(image=image), size=c(0.105, 0.075, 0.135, 0.095, 0.08, 0.09, 0.12)) +
  labs( y= "Number of spells in whole series")


obrazki + 
  labs(title = "Top 7 characters in terms of spell casting") +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        plot.title = element_text(color = "white", hjust = 0.5),
        axis.text = element_text(color = "white"),
        panel.background = element_rect(fill = "black"),
        panel.grid.minor = element_line(color = "#545454"),
        panel.grid.major = element_line(color = "#545454")) +
  theme(text=element_text(size=45, family="Times New Roman"), 
        axis.title.x=element_text(color="white", size = 35)) +
  theme(panel.background = element_rect(fill = "transparent",
                                  colour = NA_character_),
plot.background = element_rect(fill = "transparent",
                               colour = NA_character_),
legend.background = element_rect(fill="transparent",
                                 colour = NA_character_)) -> obrazki

ggsave(
  plot = obrazki,
  filename = "obrazki1.png",
  bg = "transparent",
  width = 6500, height = 3200, units = "px"
)