library (dplyr)
library(ggplot2)

#wykres boxplot na plakacie

mojo_jojo<- read.csv("Mojo_budget_update.csv")

mojo_jojo_mod <- mojo_jojo_mod %>% filter(!is.na(genre))

mojo_jojo_mod %>% filter(genre!="") %>% 
  filter(genre %in% c("Drama", "Comedy", "Action", "Thriller", "Adventure", "Romance", "Crime")) %>% 
  ggplot(aes(x=genre, y=worldwide)) +
  geom_boxplot(fill="#ffebd1", outlier.color = "white", color="#dfbb7e") +
  labs(x="Gatunek", y="Zarobki w dolarach amerykańskich", title="Zarobki poszczególnych gatunków") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 1000000000))+
  theme(plot.title = element_text(face = "bold"), axis.text.x = element_text(margin = margin(t = -14))) +
  scale_y_continuous(labels = scales::number_format(scale=1e-6, suffix = " M"))+
  scale_x_discrete(labels = c("Akcja", "Przygodowy", "Komedia", "Kryminał", "Dramat", "Romans", "Thriller"))+
  theme(
    panel.background = element_rect(fill='#4173ab'), 
    plot.background = element_rect(fill='#4173ab', color="white"),
    axis.text.x = element_text(color = "white", size=12, face="bold"),  
    axis.text.y = element_text(color = "white", size=12),  
    axis.title.x = element_text(color = "white", size=17, face="bold", margin = margin(t = 10)),  
    axis.title.y = element_text(color = "white",size=17, face="bold", margin = margin(r = 10)),
    plot.title = element_text(color = "white", size=25),
    panel.border = element_rect(color="white", fill=NA)
  )



