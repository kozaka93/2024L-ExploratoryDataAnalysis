library(dplyr)
library(stringr)
library(stringi)
library(tidytext)
library(ggplot2)
library(tidyr)
library(wordcloud2)
library(svglite)
library(patchwork)


# density
data1 <- read.csv("himym_episodewise.csv")
data2 <- read.csv("himym_script_data.csv" )

celebrity <- read.csv("celebs_02.csv")
data_new <- left_join(data1, celebrity, by ="Title")

data_new1 <- data_new %>% 
  select(-c(Season.y, Episode.y)) %>% 
  mutate(celebrity_yn = case_when(is.na(Celebrity) == TRUE ~ "No",
                                  is.na(Celebrity) == FALSE ~ "Yes"))


plot1 <- ggplot(data_new1, aes(x=IMDB_Rating, color=celebrity_yn, fill=celebrity_yn)) +
  geom_density(alpha=0.6) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.85)) +
  scale_x_continuous(expand = c(0, 0), limits = c(4,10.3), breaks = c(4,5,6,7,8,9,10)) +
  scale_color_manual(name = ("Celebrity guest\napperance"),
                     values = c("#BF7BD0","#83B4DE"))+
  scale_fill_manual(name = "Celebrity guest\napperance",
                    values = c("#BF7BD0","#83B4DE"))+
  labs(x = "IMDB rating",
       y = "Density",
       title = "Distribution of IMDB rating",
       subtitle = "based on guest celebrity appearances") +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent') 
    
  )+
  
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size = 14 ),
        legend.title = element_text(size=14),
        legend.text =element_text(size=10 ),
        plot.subtitle = element_text(size=12 ),
        legend.position = "right")


plot1

ggsave('celebrity_imdb_03.svg', plot1, bg='transparent')



plot2 <- ggplot(data_new1, aes(x=Viewers, color=celebrity_yn, fill=celebrity_yn)) +
  geom_density(alpha=0.6) +
  theme_classic() +
  scale_y_continuous(expand = c(0, 0), limits=c(0, 0.35)) +
  scale_x_continuous(expand = c(0, 0), limits = c(4,14.5), breaks = c(4, 6, 8, 10,12,14)) +
  scale_color_manual(name = ("Celebrity guest\napperance"),
                     values = c("#BF7BD0","#83B4DE"))+
  scale_fill_manual(name = "Celebrity guest\napperance",
                    values = c("#BF7BD0", "#83B4DE"))+
  labs(x = "Number of viewers in millions",
       y = "Density",
       title = "Distribution of viewers",
       subtitle = "based on guest celebrity appearances") +
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'))+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=12),
        plot.title = element_text(size = 14),
        legend.position = "right",
        legend.title = element_text(size=14),
        legend.text =element_text(size=10 ),
        plot.subtitle = element_text(size=12  ))


plot2

ggsave(plot2, device = svg, filename = "celebrity_views_03.svg")





# heatmapa


data2 <- read.csv('himym_episodewise.csv', sep=",")
celebs <- read.csv("celebs_02.csv",sep = ",")
cel <- celebs %>%
  mutate(znak = "*")

data3 <- left_join(data2,cel, by = c("Title"))


legend_title = expression(atop("* - celebrity guest\n apperance",
                               bold("Rating                ")))

heatmap <- ggplot(data3, aes(x = Episode.x, y = Season.x, fill=cut(IMDB_Rating, c(5,6,7,8,9,10)))) +
  geom_tile(color = "black") +
  geom_text(aes(label = IMDB_Rating), color = "black", size =4) +
  geom_text(aes(label = znak), color = "black", size = 5, vjust = -0.1,hjust=-0.9) +
  scale_fill_manual(values=c("#FDFFFF","#D6E8F7", "#C0DEF7", "#A0C9EC","#83B4DE"),name = legend_title) +
  coord_fixed() + theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(expand = expansion(mult = c(0, 0)),breaks = seq(0,30,1)) + 
  scale_y_continuous(expand = expansion(mult = c(0, 0)),breaks = seq(0,10,1)) +
  labs(title = "Rating of episodes on IMDB",
       x = "Episodes",
       y = "Season",
       fill = "Rating") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title = element_text(size = 16),
        legend.position = "left",
        
        legend.title = element_text(size=16),
        legend.text =element_text(size=14))+
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'))

heatmap
ggsave('heatmap_03.png', heatmap, bg='transparent')



# awards


df2 <- read.csv("jula/awards.csv",sep = ";")

df3 <- df2[c(-1,-3),]

colnames(df3) <- c("Award","Year","Category","Nominee/s","Result","Ref")




t2 <- df3 %>%
  filter(Result == "Won") %>%
  group_by(Year) %>%
  summarise(n = n())



years <-ggplot(t2[-10,],aes(y=Year,x = n)) +
  geom_col(fill = "#83B4DE") +
  theme_light() +
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + labs(title= "Awards won by HIMYM") +
  ylab(" ") +
  xlab("Number of awards") +
  scale_x_continuous(expand =  c(0, 0), limits = c(0,4.2)) +
  theme(axis.text.y = element_blank(),axis.ticks.y=element_blank()) +
  theme(
    panel.background = element_rect(fill='transparent'), 
    plot.background = element_rect(fill='transparent', color=NA), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'), 
    legend.box.background = element_rect(fill='transparent')) +  
  theme(axis.text=element_text(size=10),
    axis.title=element_text(size=12),
    plot.title = element_text(size = 14),
    legend.position = "right",
    legend.title = element_text(size=14),
    legend.text =element_text(size=10 ),
    plot.subtitle = element_text(size=12  ))


years




# wordcloudy

script <- read.csv("himym_script.csv")

Barney_words <- script %>% 
  unnest_tokens(output = word, input = line) %>%
  count(name, word, sort = TRUE) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(-tf_idf) %>%
  filter(word != "change.you") %>% 
  filter(name=="Barney") %>%
  top_n(200) %>% 
  arrange(-n) %>% 
  slice(1:200) %>% 
  select(word, n) %>% 
  mutate(word = ifelse(word=="scherbotsky", "scherbatsky", word))

wordcloud2(Barney_words, figPath="Barney_im_13.jpg", color="black", 
           size=1.1)


Lily_words <- script %>% 
  unnest_tokens(output = word, input = line) %>%
  count(name, word, sort = TRUE) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(-tf_idf) %>%
  filter(name=="Lily") %>%
  top_n(200)%>% 
  arrange(-n) %>% 
  slice(1:200) %>% 
  select(word, n)

wordcloud2(Lily_words, figPath = "Lily_img.png", color="black", size=0.85)




Marshall_words <- script %>% 
  unnest_tokens(output = word, input = line) %>%
  count(name, word, sort = TRUE) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(-tf_idf) %>%
  filter(name=="Marshall") %>%
  top_n(200)%>% 
  select(word, n) %>% 
  add_row(word="lilypad", n=5) %>% 
  arrange(-n) %>% 
  slice(1:200) %>% 
  mutate(word=ifelse(word=="biercules", "beercules", word))

wordcloud2(Marshall_words, figPath = "Marshall_im.jpg", color='black', size=0.85)



Robin_words <- script %>% 
  unnest_tokens(output = word, input = line) %>%
  count(name, word, sort = TRUE) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(-tf_idf) %>%
  filter(name=="Robin") %>%
  top_n(200)%>% 
  arrange(-n) %>% 
  slice(1:200) %>% 
  select(word, n)

wordcloud2(Robin_words, figPath = "Robin_im_04.jpg", color="black", size=0.7)



Ted_words <- script %>% 
  unnest_tokens(output = word, input = line) %>%
  count(name, word, sort = TRUE) %>% 
  bind_tf_idf(word, name, n) %>% 
  arrange(-tf_idf) %>%
  filter(name=="Ted") %>%
  top_n(200)%>% 
  arrange(-n) %>%
  slice(1:200) %>% 
  select(word, n)

wordcloud2(Ted_words, figPath = "Ted_im_09.png", color= "black", size=0.9)

