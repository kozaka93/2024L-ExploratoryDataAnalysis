
# Źródło ------------------------------------------------------------------

# https://twitter.com/PremierRP/status/1115195061887741952

# Elementy do poprawy -----------------------------------------------------

# 1) Oś y nie zaczyna się od 0, więc czytelnikowi wydaje się, że wynagrodzenia wzrosną nawet kilkukrotnie, 
# a w rzeczywistości dla Pensum 22 wzrost wynosi ok. 37,5 %, a w przypadku Pensum 24 jest to ok. 44,5 %
#
# 2) Jest to wykres z 2019 roku, więc raczej uważna osoba zoorientowała się, że dla danych lat były to jedynie
# przewidywania, lecz wydaje mi się, że dla mniej uważnych czytelników, jak i osób, które w jakiś sposób zobaczyły 
# ten wykres później warto dodać o tym informację w tytule
#
# 3) Skoro mówimy o pieniądzach to warto napisać, czy jest to pensja netto czy brutto.
library(dplyr)
library(plotly)

lata <- 2019:2023
pensum22 <- c(5603, 6128, 6653, 7179, 7704)
pensum24 <- c(5603, 6335, 7434, 7800, 8100)

dane <- data.frame("Lata" = lata,
                   "Pensum22" = pensum22,
                   "Pensum24" = pensum24)


plot_ly(
  data = dane, 
  x = ~Lata, 
  y = ~Pensum22,
  type = "bar",
  hoverinfo = "y"
) %>% layout(
  title = "Średnie wynagrodzenie brutto nauczyciela dyplomowanego (przewidywania na lata 2019-2023)",
  xaxis = list(title = "Rok"),
  yaxis = list(range = c(0, 8200), title = "Wynagrodzenie (zł)"),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list("y", list(~Pensum22)),
             label = "Pensum22"),
        list(method = "restyle",
             args = list("y", list(~Pensum24)),
             label = "Pensum24")
      ))
  ))
  

# Dlaczego lepsze ---------------------------------------------------------

# 1) Przede wszystkim na nowym wykresie oś zaczyna się od 0, więc nie ma takiej manipulacji danymi.
# 2) Dzięki plot_ly osoba czytająca wykres widzi dokładne wartości oraz może skupić się jedynie na pensum, które go interesuje
# (wydaje mi się, że umieszczenie obu na jednym wykresie nie jest złe, ale chciałem napisać trochę więcej kodu)
# 3) dodana informacja o tym, że były to przewidywania

