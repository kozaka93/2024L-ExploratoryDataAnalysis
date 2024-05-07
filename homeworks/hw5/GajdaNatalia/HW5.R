library(plotly)
library(dplyr)

# Źródło: https://www.nyc.gov/assets/nypd/downloads/pdf/analysis_and_planning/year-end-2023-enforcement-report.pdf
# Wizualizacja znajdująca się na 4 stronie (Robbery Victim, Suspect and Arrestee Race Ethnicity) oraz w folderze, 
# sama w sobie jest trudna do odczytania, nie widać dokładnych wartości słupków. Do tego jest 3 wymiarowa,
# co dodatkowo utrudnia chociażby porównanie ze sobą konkretnych wartości (a co dopiero dokładny odczyt danych)


# Uważam, że poniższa wizualizacja jest lepsza, ponieważ reprezentuje dane dokładnie - po najechaniu na słupki
# możemy dowiedzieć się jaki (dokładny) procent tyczy konkretnej rasy. Do tego wybór konkretnej grupy
# (ofiary, podejrzani, zatrzymani) ułatwia dokładny odczyt wartości i porównanie ze sobą ras dla konkretnych "kategorii" (czyli ofiara, podejrzany, zatrzymany). 


robbery_data <- data.frame(
  Race_Ethnicity = c("American Indian", "Asian / Pacific Islanders", "Black", "White", "Hispanic"),
  Victim_percentage = c(0.9, 16.8, 26.5, 12.4, 43.5),
  Suspect_percentage = c(0.1, 2.2, 62.5, 4.2, 31.0),
  Arrestee_percentage = c(0.2, 3.0, 59.2, 4.9, 32.7)
)

robbery_plot <- plot_ly(data = robbery_data, type = 'bar', x = ~Race_Ethnicity, y = ~Victim_percentage, name = 'Victim_percentage') 

robbery_plot <- layout(
  robbery_plot,
  title = "Roberry victims, suspects and arrestees <br> by race/ethnicity in % (NYC 2023 data)",
  xaxis = list(title = "Race/Ethnicity"),
  yaxis = list(title = "Percentage"),
  updatemenus = list(
    list(
      x = 1, y = 1,
      buttons = list(
        list(method = "restyle",
             args = list(list(y = list(robbery_data$Victim_percentage), name = "Victim_percentage")),
             label = "Victim Percentage"),
        list(method = "restyle",
             args = list(list(y = list(robbery_data$Suspect_percentage), name = "Suspect_percentage")),
             label = "Suspect Percentage"),
        list(method = "restyle",
             args = list(list(y = list(robbery_data$Arrestee_percentage), name = "Arrestee_percentage")),
             label = "Arrestee Percentage")
      )
    )
  )
)

robbery_plot







