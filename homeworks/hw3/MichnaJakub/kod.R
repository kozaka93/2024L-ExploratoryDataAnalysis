library(dplyr)
library(forcats)
library(ggplot2)

# Poprawa wykresu znalezionego w internecie:

wyniki_sondy <- data.frame("odpowiedz" = c("Bezpartyjni Samorządowcy", "Koalicja Obywatelska", "Prawo i Sprawiedliwość", 
                                           "Konfederacja", "Nowa Lewica", "Trzecia Droga", "Inne ugrupowanie", "Nie mam zdania",
                                           "Kampanię wszystkich komitetów oceniam tak samo"), "procent" = c(3.29,21.7,14,9.9,
                                                                                                            7.49,9.08,0.5,20.79,13.3))
wyniki_sondy <- mutate(wyniki_sondy, odpowiedz = fct_reorder(odpowiedz,-procent))
ggplot(wyniki_sondy, aes(x = odpowiedz, y = procent, fill = as.factor(odpowiedz))) + geom_bar(stat = "identity") +
  theme_bw()+
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  geom_text(aes(label = paste0(procent, "%"), vjust = -0.5), size = 4) + 
  scale_x_discrete(labels = c(
    "Bezpartyjni Samorządowcy" = "BS",
    "Koalicja Obywatelska" = "KO",
    "Prawo i Sprawiedliwość" = "PiS",
    "Konfederacja" = "Konf",
    "Nowa Lewica" = "NL",
    "Trzecia Droga" = "TD",
    "Inne ugrupowanie" = "inne",
    "Nie mam zdania" = "Brak zdania",
    "Kampanię wszystkich komitetów oceniam tak samo" = "Ta sama ocena"))+
scale_fill_manual(values = c(
  "Bezpartyjni Samorządowcy" = "darkblue",
  "Koalicja Obywatelska" = "orange",
  "Prawo i Sprawiedliwość" = "red",
  "Konfederacja" = "black",
  "Nowa Lewica" = "yellow",
  "Trzecia Droga" = "green",
  "Inne ugrupowanie" = "violet",
  "Nie mam zdania" = "lightgrey",
  "Kampanię wszystkich komitetów oceniam tak samo" = "brown"),
  name = "Odpowiedź respondenta", 
  labels = c(
    "Bezpartyjni Samorządowcy" = "Bezpartyjni Samorządowcy (BS)",
    "Koalicja Obywatelska" = "Koalicja Obywatelska (KO)",
    "Prawo i Sprawiedliwość" = "PiS",
    "Konfederacja" = "Konfederacja (Konf)",
    "Nowa Lewica" = "Nowa Lewica (NL)",
    "Trzecia Droga" = "Trzecia Droga (TD)",
    "Inne ugrupowanie" = "Inne ugrupowanie (inne)",
    "Nie mam zdania" = "Brak zdania",
    "Kampanię wszystkich komitetów oceniam tak samo" = "Kampanię wszystkich komitetów oceniam tak samo\n(Ta sama ocena)"
  )) +
  labs(title = "Sondaż: Jaki komitet wyborczy najlepiej prowadzi swoją kampanię?",
       x = "Odpowieź",
       y = "Odstek osób, które udzieliły daną odpowiedź")


dane <- data.frame(
  kategoria = c("bardzo długa kategoria 1", "bardzo długa kategoria 2", "krótka1", "krótka2"),
  wartosc = c(10, 20, 30, 40)
)


       