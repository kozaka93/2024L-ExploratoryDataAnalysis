library(ggplot2)
library(dplyr)
library(forcats)
options(scipen=12)

the.office.df <- The.Office.Lines.V3

gadatliwi <- The.Office.Lines.V3 %>% 
    group_by(speaker) %>% 
    summarize(total_words = sum(lengths(strsplit(line, ' ')))) %>% 
    arrange(desc(total_words))
gadatliwi$speaker[1:20]

speaker_levels <- unique(gadatliwi$speaker[order(gadatliwi$total_words, decreasing = TRUE)])[1:5]
speaker_levels

#Kolory:
#6a696e
#d29b58
#39456b
#b3c7e2
#704743
#e9e0bd
#443a39
#fefefe
#aba48f
#6ea4d0
pogadanki

library(stringr)

episodes_per_season <- c(6, 22, 25, 19, 28, 26, 26, 24, 25)

pogadankiv2 <- The.Office.Lines.V3 %>% 
    filter(speaker %in% speaker_levels) %>% 
    group_by(speaker, season) %>% 
    summarize(words = sum(lengths(strsplit(line, ' ')))) %>% 
    mutate(words_per_episode = words / episodes_per_season[as.numeric(factor(season))])%>%
    ggplot(aes(x = factor(season, levels = c(1:9)),
               y = words_per_episode)) +
    geom_col(fill = "#704743")+
    labs(title = "",
         x = "",
         y = "" ) +
    scale_x_discrete(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0))+
    expand_limits(y = c(0, 1500)) +
    theme_minimal(base_size = 16) +
    facet_wrap(~factor(speaker, levels = speaker_levels), ncol = 5) +
    theme(panel.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd')) +
    theme(plot.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd')) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    theme(
        strip.background = element_blank(),
        strip.text.x = element_blank()
    )
    

pogadankiv2




monologi <- talking_head %>%
    group_by(character) %>%
    summarise(ile = n())

ggplot(monologi, aes(x = factor(character, levels = c("Pam", "Jim", "Dwight", "Michael")), y = ile)) +
    geom_col(fill = "#704743") + 
    theme(axis.text.x=element_blank(),
            axis.ticks.x=element_blank())  +
    scale_x_discrete(expand = c(0,0), breaks = NULL)+
    scale_y_continuous(expand = c(0,0), position = "right")+
    labs(x = "", 
         y = "") +
    geom_text(aes(label = character),color = "#FEFBF6",                   
              position = position_stack(vjust = 0.5),
              show.legend = FALSE, hjust = 1.1, size = 8)+
    theme_minimal(base_size = 16) +
    theme(panel.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd')) +
    theme(plot.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd'))+
    expand_limits(y = c(0, 800)) + 
    theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) +
    coord_flip()


# liczba monologow


pomoc <- the.office.df %>%
    mutate(numb = str_count(line, "what she said")) %>%
    group_by(speaker) %>%
    summarise(tot = sum(numb)) %>% 
    arrange(desc(tot)) %>% 
    filter(tot > 0)
pomoc
wss_levels <- unique(pomoc$speaker[order(pomoc$tot, decreasing = TRUE)])[1:9]

wss_levels

wss <- the.office.df %>%
    mutate(number = str_count(line, "what she said")) %>%
    group_by(season, speaker) %>%
    summarise(n = sum(number)) %>% 
    filter(n > 0)

wss

color_group <- c("x", "x", "x", "y", "y", "y", "y", "y", "y")

ggplot(wss, aes(fill = factor(season), y = n, x = speaker))+ #fill = color_group)) + 
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label = season, fill = speaker),color = "#FEFBF6",                   
              position = position_stack(vjust = 0.5),
             show.legend = FALSE, hjust = 0.4, size = 4)+
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,8), breaks = 1:10)
    

wss <- the.office.df %>%
    mutate(wo = str_count(line, "what she said") == TRUE, line, "dupa") %>%
    group_by(season, speaker) %>%
    summarise(n = sum(wo))

ggplot(wss, aes(fill = factor(speaker), y = n, x = season)) + 
    geom_bar(position="stack") +
    xlim(1,10) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0), limits = c(0,10), breaks = 1:10)


dwight_lines <- The.Office.Lines.V3 %>% filter(speaker == "Dwight")

liczba_Michael <- sum(str_count(dwight_lines$line, "ichael!"))

print(liczba_Michael)

andy_lines <- The.Office.Lines.V3 %>% filter(speaker == "Andy")

liczba_Cornell <- sum(str_count(andy_lines$line, "Cornell"))

print(liczba_Cornell)

dwight_lines <- The.Office.Lines.V3 %>% filter(speaker == "Dwight")

liczba_Battlestar <- sum(str_count(dwight_lines$line, "tlestar"))

print(liczba_Battlestar)

dwight_lines <- The.Office.Lines.V3 %>% filter(speaker == "Dwight")

liczba_beet <- sum(str_count(dwight_lines$line, "beet"))

print(liczba_beet)

dwight_lines <- The.Office.Lines.V3 %>% filter(speaker == "Dwight")

liczba_manager <- sum(str_count(dwight_lines$line, "regional manager"))

print(liczba_manager)

mike_lines <- The.Office.Lines.V3 %>% filter(speaker == "Michael")

liczba_wss <- sum(str_count(mike_lines$line, "hat's what she said"))

print(liczba_wss)


vance_lines <- The.Office.Lines.V3 %>% filter(speaker == "Bob Vance")

vance_lines

liczba_ref <- sum(str_count(vance_lines$line,"Vance refrigeration"))

print(liczba_ref)
unique(The.Office.Lines.V3$speaker)
biuro <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/the_office_series.csv")
unique(biuro$GuestStars)

biuro2 <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/archive (1)/parent_reply.csv")
gadanie_do_kamery <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/archive (1)/talking_head.csv")

unique(gadanie_do_kamery$character)

skrypt <- read.csv("C:/Users/kowal/Desktop/Wstep do eksploracyji/the-office-lines.csv", sep = ";")


install.packages("igraph")
install.packages("ggraph")
install.packages("RColorBrewer")

library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(igraph)
library(ggraph)
library(viridis)
library(RColorBrewer)

options(scipen = 5)


{
    
    monologi <- gadanie_do_kamery %>%
        group_by(character) %>%
        summarise(ile = n())
    
    ggplot(monologi, aes(x = character, y = ile)) +
        geom_col() + 
        labs(x = "Character", 
             y = "Number of monologues")
} # liczba monologow



?ggraph
?igraph
?graph_from_data_frame
?geom_node_text
?geom_edge_link
?theme_void

{
    
    df <- skrypt %>% 
        filter(speaker %in% c("Michael", "Pam", "Jim", "Dwight", "Andy")) %>% 
        group_by(season, episode, scene) %>%
        filter(n() >= 2) %>% 
        mutate(combination = list(combn(sort(speaker), 2, FUN = paste, collapse = ","))) %>% 
        unnest(cols = combination) %>% 
        separate(combination, into = c("character1", "character2"), sep = ",") %>% 
        filter(character1 != character2) %>% 
        select(season, episode, scene, character1, character2) %>% 
        slice_head(n = 1) %>% 
        group_by(character1, character2) %>% 
        summarise(number_of_interactions = n())
    
    graph <- graph_from_data_frame(df, directed = FALSE)
    
    
    
    ggraph(graph, layout = "linear", circular = TRUE) +
        geom_edge_arc(aes(width = number_of_interactions), color = "#728AAC") +
        geom_node_point(color = "black", size = 😎 +
                            theme(legend.position = "right", legend.direction = "vertical") +
                            scale_edge_width_continuous(range = c(4, 18), name = "Number of Interactions") + # Dodaj etykietę
                            theme_void() +
                            theme(panel.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd')) +
                            theme(plot.background = element_rect(fill = '#e9e0bd', colour = '#e9e0bd')) +
                            guides(edge_width = guide_legend(override.aes = list(shape = 21, fill = "#728AAC"),
                                                             title.position = "top", title.hjust = 0.5))
                        
                        } # ilosc interakcji miedzy bohaterami