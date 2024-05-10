library(plotly)
library(dplyr)


years <- c(2009:2025)
percentages <- c(4.8, 4.7, 4.5, 4.4, 4.6, 4.4, 4.4, 4.5, 4.73, 4.8, 4.86, 5, 5.3, 5.5, 5.7, 5.8, 6.0)
data <- data.frame(years, percentages)

data$type <- ifelse(data$years <= 2020, "Historyczne", "Prognoza")


fig <- plot_ly(data, x = ~years, y = ~percentages, color = ~type, colors = c("Light Sky Blue ", "6495ED"), type = 'bar', 
               marker = list(line = list(color = 'black', width = 0.5))) %>%
  layout(
    title = "Publiczne wydatki na ochronę zdrowia jako % PKB (Rzeczywiste i Prognozowane)",
    xaxis = list(title = "Rok", tickangle = 45),
    yaxis = list(title = "Wydatki (% PKB)"),
    barmode = 'group',
    bargap = 0.3,      
    bargroupgap = 0.1  
  )

buttons <- list(
  list(method = "restyle", args = list("type", "bar"), label = "Słupkowy"),
  list(method = "restyle", args = list("type", "scatter", "mode", "lines+markers"), label = "Liniowy")
)

fig <- fig %>%
  layout(
    updatemenus = list(
      list(
        x = 0.0,
        y = 1.2,
        yanchor = "top",
        buttons = buttons
      )
    )
  ) %>%
  layout(
    xaxis = list(fixedrange = TRUE),
    yaxis = list(fixedrange = TRUE)
  ) %>%
  config(displayModeBar = FALSE)

fig