data <- read.csv("game-of-thones-deaths.csv")

library(dplyr)
library(janitor)
library(ggplot2)

    

data <- row_to_names(data, row_number = 1)

data %>%
  group_by(Method) %>%
  summarise(n = n()) %>%
  arrange(-n) %>% 
  head(10) %>% 
  mutate(method = fct_reorder(Method, -n)) %>%
  left_join(data, by = "Method") -> data10
  

# Ten podstawowy, o którym gadałyśmy (metody śmierci)

# ggplot(data10, aes(method))+
#   geom_bar(fill = "darkred", color = "black") +
#   labs(title = "10 most common death methods in Game of Thrones", x = "Method", y = "") +
#   theme_dark() +
#   theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1))
# 
# 
# # jeszcze liczby śmierci w sezonach, chyba nic ciekawego, ale wysyłam
# 
# data %>% 
# ggplot(aes(Season)) +
#   geom_bar(fill = "red4", color = "black") +
#   labs(title = "Number of deaths in particular seasons of Game of Thrones", x = "Season",
#        y = "Number of deaths") +
#   theme_dark()


# a to moje wczorajsze wieczorne natchnienie, żeby uniknąć słupków xd
# szczerze dziwnie to wygląda, więc nwm czy się przyda, ale no jakbyście były ciekawe xD
# tak, wiem że i tak mamy za dużo wykresów, przepraszam, ale nie mogłam się powstrzymać

data %>% 
  filter(Season %in% as.character(1:6)) %>% 
  group_by(Episode, Season) %>% 
  summarise(n_deaths = n()) %>% 
  mutate(Episode = as.numeric(Episode)) %>% 
  ggplot(aes(x = Episode, y = n_deaths, color = Season))+
  geom_line(linewidth = 1.6, alpha = 0.85)+
  theme(text = element_text(family = "GOT"),
        axis.text = element_text(size = 27, color = "#dcdcdc", family = "sans"),
        axis.title = element_text(size = 30, color = "#dcdcdc", face = "bold"),
        plot.title = element_text(size = 40, color = "#dcdcdc", face = "bold"),
        legend.position = "none",
        legend.background = element_rect(fill = "transparent", color = NA),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent"),
        panel.grid = element_line(color = "#333333"))+
  labs(title = "GOT  deaths",
       x = "Episode",
       y = "Number of deaths")+
  scale_y_continuous(breaks = seq(0, 250, 50),
                     minor_breaks = seq(0, 250, 12.5))+
  scale_color_manual(values = c("#D98D00", "#B2B2B2", "#D7C626", "#9EE7FF",
                                "#AD2E28","#83BA60"), 
                     labels = paste("Season", c("I", "II", "III", "IV", "V", "VI")))+
  scale_x_continuous(breaks = 1:10, labels = as.character(1:10),
                     minor_breaks = NULL, expand = c(0.01, 0.01))->plot4



# data %>% 
#   filter(Season == "8") %>% 
#   group_by(Episode) %>% 
#   summarise(n_deaths = n()) %>% 
#   mutate(Episode = as.numeric(Episode)) %>% 
#   ggplot(aes(x = Episode, y = n_deaths))+
#   geom_line(linewidth = 1.6, alpha = 0.85, color = "#9f0000")+
#   theme(text = element_text(family = "GOT"),
#         axis.text = element_text(size = 27, color = "#dcdcdc", family = "sans"),
#         axis.title = element_text(size = 30, color = "#dcdcdc", face = "bold"),
#         plot.title = element_text(size = 40, color = "#dcdcdc", face = "bold"),
#         plot.background = element_rect(fill = "#111111", color = "#111111"),
#         panel.background = element_rect(fill = "#dcdcdc"),
#         panel.grid = element_line(color = "#D2D2D2"))+
#   labs(title = "Deaths in season VIII",
#        x = "Episode",
#        y = "Number of deaths")+
#   scale_y_continuous(breaks = seq(0, 900, 50),
#                      minor_breaks = seq(0, 900, 25))+
#   scale_x_continuous(breaks = 1:10, labels = as.character(1:10),
#                      minor_breaks = NULL, expand = c(0.01, 0.01))


