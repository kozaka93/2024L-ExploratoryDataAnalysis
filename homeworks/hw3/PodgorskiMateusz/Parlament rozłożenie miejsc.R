# install.packages("ggparliament")
library(ggparliament)
# install.packages("tidyverse")
library(tidyverse)
library(ggplot2)






# Dane wyborów na podstawie Twojego obrazka
parties <- c("BSW", "SPD", "GRÜNE", "CDU/CSU","AfD")
seats <- c(45, 116, 101, 229, 139)
colours <- c("#E91E63", "#FF0000", "#4CAF50", "#2196F3", "#3F51B5")
parties

# Tworzymy ramkę danych
bundestag <- data.frame(party_long = parties, 
                        seats = seats, 
                        colour = colours,
                        party_short = parties) # Używamy tej samej nazwy dla party_short dla uproszczenia

# Tworzymy dane dla wykresu półkolistego
ger_semicircle <- parliament_data(election_data = bundestag,
                                  type = "semicircle",
                                  parl_rows = 10,
                                  party_seats = bundestag$seats)





ggplot(ger_semicircle, aes(x = x, y = y, colour = party_short)) +
  geom_parliament_seats() + 
  draw_partylabels(type = "semicircle",
                   party_names = party_long,
                   party_seats = seats,
                   party_colours = colour) + 
  draw_totalseats(n = 736, type = "semicircle") +
  theme_ggparliament() +
  labs(title = "Niemcy, przewidywane miejsca w parlamencie", colour = "Partie") +
  scale_colour_manual(values = ger_semicircle$colour, 
                      limits = ger_semicircle$party_short)