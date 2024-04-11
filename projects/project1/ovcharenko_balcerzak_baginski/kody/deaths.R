library(dplyr)
library(ggplot2)
library(forcats)
library(tidyr)

deaths <- read.csv("deaths.csv")
#View(deaths)

types <- c((deaths %>% filter(grepl('shot|Shot', cause)) %>%
  summarise(sum = sum(number_of_deaths)))[[1]],

(deaths %>% filter(grepl('oison', cause)) %>%
  summarise(sum = sum(number_of_deaths)))[[1]],

(deaths %>% filter(grepl('air traffic', cause)) %>%
  summarise(sum = sum(number_of_deaths)))[[1]],

(deaths %>% filter(grepl('tabbed', cause)) %>%
  summarise(sum = sum(number_of_deaths)))[[1]],

(deaths %>% filter(!grepl('shot|Shot|oison|air traffic|tabbed', cause)) %>%
   summarise(sum = sum(number_of_deaths)))[[1]]
)
types <- sort(types, decreasing=T)

causes <- ggplot(data = NULL, aes(x = fct_rev(fct_reorder(c('plane crash','gun','other','poison', 'stabbed'), types)), y = types)) +
  geom_point(color="#0d3e10", size=6) + 
  geom_segment( aes(x=fct_rev(fct_reorder(c('plane crash','gun','other','poison', 'stabbed'), types)), 
                    xend=fct_rev(fct_reorder(c('plane crash','gun','other','poison', 'stabbed'), types)),
                    y=0, yend=types), color="#0d3e10", linewidth = 2.5)+
  labs(title = "Causes of deaths in Breaking Bad",
       x = "Cause", 
       y = "Number of deaths") +  coord_flip() +  
  scale_y_continuous(expand=c(0,0), limits = c(0,175))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )+ theme(
    axis.title = element_text(size = 15),  
    axis.text = element_text(size = 13, color = "#2a2a2a"),  
    plot.title = element_text(size = 17)) 
causes

ggsave('myplot2.png', causes, bg='transparent')

deaths2 <- read.csv("deaths.csv")
deaths2[grepl(167, deaths$number_of_deaths),]$number_of_deaths = 0

deaths2[grepl('Walter White', deaths$responsible),]$responsible = 'Walter White'
deaths2[grepl('Gustavo Fring', deaths$responsible),]$responsible = 'Gustavo Fring'
deaths2[grepl('Lydia', deaths$responsible),]$responsible = 'Lydia Rodarte-Quayle'

murderer2 <- deaths2 %>%
  group_by(responsible) %>%
  summarise(sum = sum(number_of_deaths)) %>%
  arrange(-sum) %>% filter(sum >2)

killhero <- ggplot(murderer2, aes(x = fct_rev(fct_reorder(responsible,sum)), y = sum)) +
  geom_col(fill = '#0d3e10')+  
  scale_y_continuous(expand=c(0,0), limits = c(0, 33))+
  labs(title = "Number of killed people by each hero of Breaking Bad",
       x = "Hero", 
       y = "Number of killed people") +
  scale_x_discrete(guide = guide_axis(n.dodge = 1, title = "responsible", angle = 25))+
  theme(
    panel.background = element_rect(fill='transparent'),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill='transparent'),
    legend.box.background = element_rect(fill='transparent')
  )+ theme(
    axis.title = element_text(size = 17),  
    axis.text = element_text(size = 15, color = "#2a2a2a"),  
    plot.title = element_text(size = 19)) 
killhero
ggsave('myplot.png', killhero, bg='transparent')

# ggplot(data = NULL, aes(x = fct_rev(fct_reorder(c('planes crash','gun','other','poison', 'stabbed'), types)), y = types)) +
#   geom_col(fill = '#0d3e10', color = 'white')+
#   labs(title = "Causes of deaths in Breaking Bad",
#        x = "Cause", 
#        y = "Number of deaths")+ theme_minimal()+  
#   scale_y_continuous(expand=c(0,0), limits = c(0,175))

# sea <- deaths %>% filter(!grepl('air traffic', cause)) %>%
#   group_by(season) %>%
#   summarise(sum = sum(number_of_deaths)) %>%
#   arrange(season)
# c(0,167,0,0,0) -> planes_crush
# 
# sea <- data.frame(sea,planes_crush)
# sea <- pivot_longer(sea, cols = c(sum, planes_crush))
# 
# ggplot(sea, aes(fill=name, y=value, x=season)) +
#   geom_bar(position="stack", stat="identity")+
#   labs(title = "Number of deaths in each season of Breaking Bad",
#        x = "Season", 
#        y = "Number of deaths", fill="")+ theme_minimal()+  
#   scale_y_continuous(expand=c(0,0)) + 
#   scale_fill_manual(values = c('#3c7d40','#0d3e10'))