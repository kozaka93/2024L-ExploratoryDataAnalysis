library(plotly)
library(dplyr)

inflacja <- read.csv("inflacja.csv")
colnames(inflacja) <- c("Miesiąc", "r2021", "r2022", "r2023", "r2024")

plot_ly(
  data = inflacja,
  x = ~Miesiąc,
  y = ~r2021,
  type = "scatter",
  name = "2021",
  mode = "lines+markers",
  marker = list(size = 10, color = '00ccff'),
  line = list(color = '00ccff')
) %>%
  add_trace(y = ~r2022,
            name = "2022",
            marker = list(size = 10, color = 'forestgreen'),
            line = list(color = 'forestgreen')) %>%
  add_trace(y = ~r2023,
            name = "2023",
            marker = list(size = 10, color = '66ff33'),
            line = list(color = '66ff33')) %>%
  add_trace(y = ~r2024,
            name = "2024",
            marker = list(size = 10, color = '660033'),
            line = list(color = '660033')) %>%
  layout(xaxis = list(showgrid = FALSE,
                      categoryorder = "array",
                      categoryarray = c("Styczeń","Luty","Marzec","Kwiecień","Maj",
                                        "Czerwiec","Lipiec","Sierpień","Wrzesień",
                                        "Październik","Listopad","Grudzień")),
         yaxis = list(title = "Inflacja w %"),
         legend = list(x = 100, y = 0.5),
         title = "Inflacja w Polsce w latach 2021 - 2024")
