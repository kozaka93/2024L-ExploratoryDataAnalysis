library(dplyr)
library(ggplot2)
library(tidyr)

data <- read.csv("netflix.csv", sep=";")

data <- data %>% 
  mutate(Date = as.Date(Date, "%d.%m.%Y"))


data %>% 
  mutate(Netflix = as.numeric(Netflix_revenue),
         Amazon_Prime = as.numeric(Amazon_Prime_revenue),
         HBO_Max = as.numeric(HBO_Max_revenue),
         Disney_Plus = as.numeric(Disney_Plus_revenue),
         Hulu = as.numeric(Hulu_revenue)) %>% 
  select(Date, Netflix, Amazon_Prime, HBO_Max, 
         Disney_Plus, Hulu) %>% 
  pivot_longer(!Date, names_to = "Service",
               values_to = "Revenue")%>%
  mutate(Service = case_when(Service == "Netflix" ~ "Netflix",
                             Service == "Amazon_Prime" ~ "Amazon Prime",
                             Service == "Disney_Plus" ~ "Disney Plus",
                             Service == "Hulu" ~ "Hulu",
                             Service == "HBO_Max" ~ "HBO Max")) %>%
  ggplot(aes(x = Date, y = Revenue, color = Service))+
  geom_point(size = 4)+
  scale_color_manual(values = c( "#FF9900", "#BFF5FD", "#006E99", "#3DBB3D", "#B81D24"))+
  labs(x = "", 
       y = "Revenue (mln $)") +
  theme_dark() +
  theme(panel.background = element_rect(fill = "black"), # Czarne tło
        plot.background = element_rect(fill = "black", colour = "black"),  # Czarne tło pod legendą
        legend.background = element_rect(fill = "black"),
        legend.text = element_text(colour = "white", size = 14),
        legend.title = element_text(colour = "#FF3131", size = 20),
        axis.line = element_line(color = "white"),
        axis.text = element_text(color = "white", size = 14),
        axis.title = element_text(color = "#FF3131", size = 20),
        legend.position = "right")

  


