library(ggplot2)
library(dplyr)
library(forcats)
#link do artykułu: https://wiadomosci.wp.pl/na-kogo-chca-glosowac-polacy-w-wyborach-do-sejmikow-mamy-sondaz-7005284775099168a

#głównym problemem wykresu jest to że nie ma on żadnych osi, ślupki wiszą sobie gdzieś w przestrzeni, nie
#możemy ich za bardzo porównywać ze sobą, też lepiej widać różnice w poziomych wykresach
nazwy <- c("Prawo i Sprawiedliwość/ Zjednoczona Prawica", "Koalicja Obywatelska", "Trzecia Droga - Polska 2050 i PSL","Konfederacja",
           "Lewica i Partia Razem","Inne","Nie Wiem")
procenty <- c(29.0,28.0,13.5,8.6,7.6,4.9,8.4)
kolorki <- c("red","lavender","lightgreen","blue","violet","black","grey")
dane <- data.frame(nazwy,procenty)
dane %>% 
  mutate(procenty_2 = paste(procenty,"%")) %>% 
  ggplot(aes(x = fct_reorder(nazwy,procenty),y=procenty,fill = nazwy)) +geom_bar(stat = "identity") + scale_color_manual(values = kolorki) +
  guides(fill = FALSE) + geom_text(aes(label = procenty_2),hjust = 0.75, size = 5) +
  labs(x = "partie biorące udział w wyborach", title = "Na które z ugrupowań zagłosował(a)by Pan(i) w wyborach do sejmików województw, gdyby lista komitetów wyglądałą następująco?")+
  theme(axis.text.y = element_text(angle = 45, hjust = 1)) +
  coord_flip()

#Na tym wykresie mamy widoczne wszystko, wraz ze skalą łątwiej nam porównać wyniki poszczególnych parti
  