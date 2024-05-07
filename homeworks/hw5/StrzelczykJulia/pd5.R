library(plotly)
library(reshape2)
library(tidyverse)

df <- data.frame(
  "2019" = c(827, 816, 777, 757, 746, 725, 679, 671, 660, 652, 663, 681),
  "2020" = c(692, 724, 714, 796, 807, 740, 727, 678, 689, 715, 771, 813),
  "2021" = c(850, 915, 936, 893, 910, 922, 849, 934, 1005, 1060, 1231, 1293),
  "2022" = c(1245, 1205, 1568, 1613, 1659, 1556, 1450, 1508, 1452, 1527, 1491, 1381),
  "2023" = c(1302, 1302, 1075, 1005, 863, 837, 883, 924, 943, 929, 896, 879),
  "2024" = c(863, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA),
  "Miesiac" = c("styczeń", "luty", "marzec" , "kwiecień", "maj", "czerwiec", "lipiec", "sierpień", "wrzesień", "październik", "listopad", "grudzień"),
  check.names = FALSE
)


df <- melt(df, id.vars = "Miesiac")
colnames(df) <- c("Miesiac", "Rok", "Wartosc")

plot_ly(df,
        x = ~fct_inorder(Miesiac), 
        y = ~Wartosc,
        type = "scatter", 
        mode = "lines+markers", 
        color = df$Rok, 
        colors = c("#ffd800", "#48cae4","#fc8eac","red","green", "black"),
        text = ~paste(
          "</br><b>Data:</b> ",
          Miesiac, Rok,
          "</br><b>Cena:</b> ",
          Wartosc,
          "zł"),
        hoverinfo = 'text') %>% 
  layout(title = list(text = "Średnie ceny pszenicy konsumpcyjnej w latach 2019-2024",
                      x = 0.5,
                      y = 0.97),
         xaxis = list(title = list(text = 'Miesiąc'), rangeslider = list(visible = TRUE)), 
         yaxis = list(title = 'Cena [zł]'),
         legend = list(
           title = list(text = "Rok"), 
           bgcolor = "white"
         )
  )

