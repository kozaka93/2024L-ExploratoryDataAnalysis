library(dplyr)
library(ggplot2)
library(tidyr)
library(forcats)

partie_polityczne <- c("Bezpartyjni Samorządowcy", "Koalicja Obywatelska", 
                       "Prawo i Sprawiedliwość", "Konfederacja", "Nowa Lewica",
                       "Trzecia Droga", "Inne ugrupowanie", "Nie mam zdania",
                       "Wszystkich oceniam tak samo")
procenty <- c(3.3, 21.7, 14, 9.9, 7.5, 9.1, 0.5, 20.7, 13.3)

data <- data.frame(Partie = partie_polityczne, Procenty = procenty)
View(data)

data %>%
  mutate(Partie = fct_reorder(Partie, Procenty)) %>% 
  ggplot(aes(x = Procenty, y = Partie))+
  geom_col(fill = "#429bf5", color = "black")+
  geom_label(aes(label = data$Procenty), nudge_x = 0.3)+
  scale_x_continuous(expand = c(0,0), limits = c(0, 25))+
  labs(title = "Kto Pani/Pana zdaniem prowadzi najlepszą kampanię 
                wyborczą przed wyborami samorządowymi?",
       y = "", x = "Skala procentowa", 
       caption = "źródło danych: Rzeczpospolita.pl" )+
  theme_bw()
