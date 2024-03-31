library(ggplot2)
library(forcats)
library(dplyr)

partia <- c("Koalicja Obywatelska",
            "Prawo i Sprawiedliwość",
            "Trzecia Droga",
            "Lewica",
            "Konfederacja",
            "Nie wiem/ Trudno powiedzieć")
poparcie <- c(31.9, 30.0, 10.7, 9.5, 8.6, 9.3)
df <- data.frame(partia, poparcie)

plot <- df %>% 
  mutate(partia = as.factor(partia),
         partia = fct_relevel(partia, c("Koalicja Obywatelska",
                                        "Prawo i Sprawiedliwość",
                                        "Trzecia Droga",
                                        "Lewica",
                                        "Konfederacja",
                                        "Nie wiem/ Trudno powiedzieć"))) %>% 
  ggplot(aes(x = partia, y = poparcie)) +
  geom_col(fill = ifelse(partia != "Nie wiem/ Trudno powiedzieć", "lightgreen", "lightgray"),
                         color = "black") +
  labs(x = "Partia",
       y = "Poparcie w procentach",
       title = "Przedwyborczy sondaż",
       subtitle = "Badanie IBRiS dla Rzeczpospolitej") +
  scale_x_discrete(guide = guide_axis(angle = 30))+
  scale_y_continuous(expand = c(0,0), limits = c(0, 35)) + 
  geom_text(label = poparcie, nudge_y = 1.5)


