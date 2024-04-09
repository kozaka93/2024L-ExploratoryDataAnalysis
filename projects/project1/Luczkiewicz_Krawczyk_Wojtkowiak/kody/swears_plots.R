library(ggplot2)
library(dplyr)
library(sentimentr)
library(stringi)
library(lexicon)
library(tm)
library(wordcloud2)
library(forcats)
library(showtext)
library(extrafont)

####################################################
####################################################
###                                              ###  
### ZAKOMENTOWANY KOD NIE BYŁ UŻYTY W PLAKACIE   ###
### (szkoda było kasować)                        ###
###                                              ###
####################################################   
####################################################

#czyszczenie danych

data <- read.csv("Game_of_Thrones_Script.csv")
data <- data %>% 
  mutate(Name = capwords(Name),
         Name = stri_replace_all(Name,
                                 regex=pattern,
                                 replace,
                                 vectorize_all = FALSE))

Names <- data$Name

case_when(Names %in% Stark ~ "Stark",
          Names %in% Lannister ~ "Lannister",
          Names %in% Targaryen ~ "Targaryen",
          Names %in% Tyrell ~ "Tyrell",
          Names %in% Greyjoy ~ "Greyjoy",
          Names %in% Baratheon ~ "Baratheon",
          TRUE ~ "Other") -> Houses 

data <- mutate(data, House = Houses)


# data %>%
#   filter(Season %in% paste("Season", 1:6, sep = " ")) %>% 
#   group_by(Season) %>% 
#   summarise(Words = paste(Sentence, collapse = " ")) %>% 
#   mutate(N_Words = how_many(Words)) %>% 
#   ggplot(aes(x = Season,y = N_Words, fill = Season)) +
#   geom_col()+
#   labs(title = "Words said",
#        subtitle = "In each season",
#        x = "Season",
#        y = "",
#        )+
#   scale_fill_manual(values = rep(c("red4", "red3"), times= 3)
#                     , guide = NULL)

#kto mówi najwięcej

# data %>%
#   group_by(Name, Season) %>% 
#   summarise(Words = paste(Sentence, collapse = " ")) %>% 
#   mutate(N_Words = how_many(Words)) %>% 
#   ungroup() %>%
#   slice_max(N_Words, by = Season, n = 5) %>% 
#   ggplot(aes(x = Name, y = N_Words))+
#   geom_col(fill = "#490805")+
#   facet_wrap(~Season, scales = "free_x", nrow = 2)+
#   scale_x_discrete(guide = guide_axis(angle = 30))
  
  

#kto przeklina najwięcej/najczęściej

# data %>%
#   group_by(Name) %>% 
#   summarise(Words = paste(Sentence, collapse = " ")) %>% 
#   mutate(N_Words = how_many(Words),
#          N_Swears = how_many_bad(Words, swears),
#          N_Swears = N_Swears$count,
#          Freq_Swears = 100*N_Swears/N_Words) %>%
#   arrange(-N_Swears) %>% 
#   head(20) -> df
#   
# labels <- c(df$Name[1:3], rep("", times = 17))
# 
# df %>% 
#   ggplot(aes(N_Swears, Freq_Swears))+
#       geom_point(size = 8, color = "#9D0D06", alpha = 0.7)+
#       labs(title = "Top twenty characters",
#           subtitle = "Who swear the most",
#           x = "Number of swears",
#           y = "Frequency of swears")+
#       theme(text = element_text(family = "Game_OT"),
#           axis.text.x = element_text(size = 15, color = "#9f0000", family = "sans"),
#           axis.text.y = element_text(size = 15, color = "#9f0000", family = "sans"),
#           axis.title = element_text(size = 20, color = "#9f0000"),
#           plot.title = element_text(size = 30, color = "#9f0000"),
#           plot.subtitle = element_text(size = 25, color = "#9f0000"),
#           plot.background = element_rect(fill = "#111111", color = "#111111"),
#           panel.background = element_rect(fill = "#efefef"),
#           panel.grid = element_line(color = "#cfcfcf"))+
#     geom_text(label = labels, nudge_x = -4.7,
#               nudge_y = 0.0025, color = "#660000", size = 4, family = "GOT")+
#   scale_x_continuous(breaks = seq(0, 80, 10),
#                      minor_breaks = seq(0, 80, 2.5))+
#   scale_y_continuous(breaks = seq(0, 6, 1),
#                      minor_breaks = seq(0, 6, 0.25)) 

#rozkład przekleństw w miarę czasu
# data %>%
#   filter(Season %in% paste("Season", 1:6, sep = " ")) %>% 
#   group_by(Season, Episode) %>% 
#   summarise(Words = paste(Sentence, collapse = " ")) %>%
#   mutate(N_Swears = how_many_bad(Words, swears),
#          N_Words = how_many(Words),
#          Freq_Swears = 100*N_Swears/N_Words,
#          Freq_Swears = Freq_Swears$count,
#          Episode = as.numeric(stri_extract_first(Episode, regex ="\\s\\d*"))) %>% 
#   ggplot(aes(x = Episode, y = Freq_Swears))+
#     geom_col(fill = "black")+
#     facet_wrap(~Season)+
#     theme(axis.text.x = element_blank())

data %>%
  filter(Season %in% paste("Season", 1:6, sep = " ")) %>% 
  group_by(Season, Episode) %>% 
  summarise(Words = paste(Sentence, collapse = " ")) %>%
  mutate(N_Swears = how_many_bad(Words, swears),
         N_Swears = N_Swears$count,
         Episode = as.numeric(stri_extract_first(Episode, regex ="\\s\\d*"))) %>% 
  ggplot(aes(x = Episode, y = N_Swears, color = Season))+
  geom_line(linewidth = 1.6, alpha = 0.85)+
  theme(text = element_text(family = "GOT"),
        axis.text = element_text(size = 27, color = "#dcdcdc", family = "sans"),
        axis.title = element_text(size = 30, color = "#dcdcdc", face = "bold"),
        plot.title = element_text(size = 40, color = "#dcdcdc", face = "bold"),
        legend.title = element_text(size = 27, color = "#dcdcdc", face = "bold"),
        legend.text = element_text(size = 24, color = "#dcdcdc"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.box.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "#333333"))+
  labs(title = "GOT  swear words",
       x = "Episode",
       y = "Number of swears")+
  scale_y_continuous(breaks = seq(0, 30, 5),
                     minor_breaks = seq(2.5, 30, 1.25))+
  scale_color_manual(values = c("#D98D00", "#B2B2B2", "#D7C626", "#9EE7FF",
                               "#AD2E28","#83BA60"), 
                     labels = paste("Season", c("I", "II", "III", "IV", "V", "VI")))+
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10),
                     minor_breaks = NULL, expand = c(0.01, 0.01))->plot2


#podzial na domy

data %>% 
  mutate(House = Houses) %>% 
  filter(House != "Other") %>%
  group_by(Season, House) %>% 
  summarise(Words = paste(Sentence, collapse = " ")) %>% 
  mutate(N_Words = how_many(Words),
         N_Swears = how_many_bad(Words, swears),
         N_Swears = N_Swears$count,
         Freq_Swears = 100*N_Swears/N_Words) %>% 
  ggplot(aes(Season, House, fill = Freq_Swears)) + 
  geom_tile()+
  scale_fill_gradient2(low = "#a80000",
                       mid = "#400303",
                       high = "#000000",
                       midpoint = 0.4)+
  theme(text = element_text(family = "GOT"), 
        axis.text = element_text(size = 27, color = "#dcdcdc"),
        axis.title = element_text(size = 30, color = "#dcdcdc", face = "bold"),
        plot.title = element_text(size = 40, color = "#dcdcdc", face = "bold"),
        legend.title = element_text(size = 27, color = "#dcdcdc", face = "bold"),
        legend.text = element_text(size = 24, color = "#dcdcdc", family = "sans"),
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent", color = NA),
        panel.grid = element_line(color = "#333333"))+
  labs(title = "The  Great  Houses  swears",
       x = "Season",
       y = "House",
       fill = "Frequency of \n swearing")+
  scale_x_discrete(expand = c(0, 0),
                   labels = c("I", "II", "III", "IV", "V", "VI", "VII", "VIII"))+
  scale_y_discrete(expand = c(0, 0))-> plot3


  
  #najczestsze slowa w got vs najczestcze slowa w angielskim
#fcje sa w pliku fcje

# create_index(GOT_n(100)[, 1], sw_fry_100) -> index
# GOT_n(100)[index, ] -> top_words
# c("word", "count") -> names(top_words) 

  
#dlugosc slow
  # data %>% 
  #   mutate(House = Houses) %>% 
  #   filter(House != "Other") %>%
  #   group_by(House) %>% 
  #   summarise(Words = paste(Sentence, collapse = " "),
  #             N_Words = how_many(Words)) -> df
  #   
  # B <- how_long(df[[1, 2]])
  # G <- how_long(df[[2, 2]])
  # L <- how_long(df[[3, 2]])
  # S <- how_long(df[[4, 2]])
  # Ta <- how_long(df[[5, 2]])
  # Ty <- how_long(df[[6, 2]])
  # 
  # bind_rows(data_frame(House = rep("Baratheon", times = length(B)),
  #                      Words = B,
  #                      N_Words = rep(df[[1, 3]], times = length(B))),
  #           data_frame(House = rep("Greyjoy", times = length(G)),
  #                      Words = G,
  #                      N_Words = rep(df[[2, 3]], times = length(G))),
  #           data_frame(House = rep("Lannister", times = length(L)),
  #                      Words = L,
  #                      N_Words = rep(df[[3, 3]], times = length(L))),
  #           data_frame(House = rep("Stark", times = length(S)),
  #                      Words = S,
  #                      N_Words = rep(df[[4, 3]], times = length(S))),
  #           data_frame(House = rep("Targaryen", times = length(Ta)),
  #                      Words = Ta,
  #                      N_Words = rep(df[[5, 3]], times = length(Ta))),
  #           data_frame(House = rep("Tyrell", times = length(Ty)),
  #                      Words = Ty,
  #                      N_Words = rep(df[[6, 3]], times = length(Ty)))) -> Words_L
  # 
  #   
  # Words_L %>% 
  #   group_by(House, Words) %>% 
  #   summarise(Long_Words = n(),
  #             N_Words = first(N_Words)) %>%
  #   mutate(Frac_Words = Long_Words/N_Words) %>% 
  #   ggplot(aes(x = Words, fill = House)) + 
  #   geom_bar(position = "fill") +
  #   scale_fill_manual(values = c("#7D472A", "#181818", "#D7C626", "#707070",
  #                                 "#AD2E28","#83BA60"))

    

GOT_n(50) -> common_words
data %>% 
  select(Sentence) %>% 
  summarise(Words = paste(Sentence, collapse = " ")) %>% 
  select(Words) %>% 
  stri_replace_all_regex("[^[:alnum:][:space:]']", '') %>% 
  tolower() %>%
  stri_replace_all_regex('\\s+', '@') -> text
  
  text_bez_1 <- stri_replace_first(text, regex = '\\w*\\s', '')
  text_bez_2 <- stri_replace_first(text_bez_1, regex = '\\w*\\s', '')
  
  to_split <- gsub("@([^@]+@[^@]+)@", " \\1|",
                   c(text, text_bez_1, text_bez_2)) %>% 
    stri_replace_all(fixed = "@", " ") %>% 
    paste(collapse = "|")
  
  TROJKI <- stri_split_fixed(to_split, "|")[[1]]
  
  as.data.frame(table(TROJKI)) %>% 
     arrange(-Freq) %>% 
    head(100) -> common_phrases
  
  colnames(common_phrases) <- c("phrase", "count")
  colnames(common_words) <- c("phrase", "count")
  
  To_cloud <- bind_rows(common_words,common_phrases)

  Got_colors = c("#FF6161" , "#B2B2B2", "#D7C626", "#9EE7FF",
                 "#D98D00","#83BA60")
  
  wordcloud2(common_words,
             color = rep_len(Got_colors, nrow(common_words)),
             backgroundColor = "none",
             rotateRatio = 0,
             fontFamily = "Game of Thrones",
             ellipticity = 0.5,
             size = 0.7) 

  
  indeks <- c(4, 5, 7, 11, 14, 18, 26, 27, 28, 29, 30, 32, 35, 40, 47, 48, 51, 52, 62,
              63, 69, 70, 74, 75, 76, 77, 78, 79, 87, 93, 94, 100)
  common_phrases <- common_phrases[indeks, ]
  
  
  wordcloud2(common_phrases,
             color = rep_len(Got_colors, nrow(common_phrases)),
             backgroundColor = "none",
             rotateRatio = 0,
             fontFamily = "Game of Thrones",
             ellipticity = 0.6,
             size = 0.4)
