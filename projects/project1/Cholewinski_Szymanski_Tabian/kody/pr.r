data <- read.csv("NFLX.csv")
library(dplyr)
data %>% 
  select("Date", "Close") %>% 
  filter(row_number() >= 3931)-> date_close

# Wczytanie wymaganych pakietów
library(ggplot2)

# Załóżmy, że już masz wczytaną ramkę danych "date_close" zawierającą kolumny "Date" i "Close"

# Konwertowanie kolumny "Date" na format daty, jeśli nie zostało to wcześniej zrobione
date_close$Date <- as.Date(date_close$Date)

  
ggplot(data = date_close, aes(x = Date, y = Close)) +
  geom_line(color = "red") +
  labs(y = "Wartość akcji Netflixa w dolarach", x = "Data", title = "Wpływ poszczególnych wydarzeń na wartość akcji Netflixa") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black"), # Czarne tło
        plot.background = element_rect(fill = "black"),  # Czarne tło pod legendą
        text = element_text(color = "darkred"), plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_blank()) + 
  geom_ribbon(aes(ymin = -Inf, ymax = Close), fill = "red", alpha = 0.3) +  # Zamalowanie obszaru pod krzywą linią ceny akcji na czerwono
  ylim(150,720) +
  geom_hline(yintercept = seq(0, max(date_close$Close), by = 200), color = "gray", alpha = 0.3) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # annotate("text", x = as.Date("2018-01-22"), y = max(date_close$Close) * 0.9, label = "Netflix ogłasza, że planuje wydać 8 miliardów dolarów na treści w 2018 roku", color = "limegreen", angle = 90, vjust = -0.5, hjust = 0.8) +
  # annotate("text", x = as.Date("2018-07-09"), y = max(date_close$Close) * 0.9, label = "Pierwszy spadek liczby subskrybentów od 2011 r.", color = "blue", angle = 90, vjust = -0.5, hjust = 0.7) +
  # annotate("text", x = as.Date("2018-09-17"), y = max(date_close$Close) * 0.9, label = "Pierwszego oscar za najlepszy scenariusz oryginalny)", color = "limegreen", angle = 90, vjust = -0.5, hjust = 0.73) +
  # annotate("text", x = as.Date("2019-01-16"), y = max(date_close$Close) * 0.9, label = "Ogłoszenie ekspansję na ponad 130 krajów", color = "limegreen", angle = 90, vjust = -0.5, hjust = 0.7) +
  # annotate("text", x = as.Date("2019-08-09"), y = max(date_close$Close) * 0.9, label = "Utrata licencji na popularne seriale, takie jak The Office i Friends", color = "blue", angle = 90, vjust = -0.5, hjust = 0.76) +
  # annotate("text", x = as.Date("2020-03-11"), y = max(date_close$Close) * 0.9, label = "Początek pandemi COVID-19", color = "blue", angle = 90, vjust = -0.5, hjust = 0.8) +
  # annotate("text", x = as.Date("2020-07-10"), y = max(date_close$Close) * 0.93, label = "Niższą niż oczekiwano liczbę abonentów w kwartale", color = "blue", angle = 90, vjust = -0.5, hjust = 1.79) +
  # annotate("text", x = as.Date("2021-01-19"), y = max(date_close$Close) * 0.93, label = "Publikacja planu podniesienia cen subskrypcji w USA", color = "blue", angle = 90, vjust = -0.5, hjust = 1.74) +
  # annotate("text", x = as.Date("2021-04-21"), y = max(date_close$Close) * 0.93, label = "Publikacja raportu z słabszym wzrostem liczby subskrybentów", color = "blue", angle = 90, vjust = -0.5, hjust = 1.5) +
  # annotate("text", x = as.Date("2021-08-26"), y = max(date_close$Close) * 0.93, label = "Ogłoszenie zawarcia umowy z Spielbergiem na produkcję filmów", color = "limegreen", angle = 90, vjust = -0.5, hjust = 1.44) +
  # annotate("text", x = as.Date("2022-01-18"), y = max(date_close$Close) * 0.93, label = "Ogłoszenie planu podniesienia cen subskrypcji w Europie", color = "blue", angle = 90, vjust = -1, hjust = 1.62) +
  # annotate("text", x = as.Date("2022-04-20"), y = max(date_close$Close) * 0.9, label = "Publikacja raportu z stratą subsktrybentów", color = "blue", angle = 90, vjust = -0.6, hjust = 0.67) +
  # annotate("text", x = as.Date("2023-05-18"), y = max(date_close$Close) * 0.9, label = "Ujawnienie szczegółów inwestorom o nowym poziomie reklam", color = "limegreen", angle = 90, vjust = -0.5, hjust = 0.74) +
  # annotate("text", x = as.Date("2023-10-17"), y = max(date_close$Close) * 0.9, label = "Publikacja raportu z wzrostem liczby subskrybentó o 70%", color = "limegreen", angle = 90, vjust = -0.5, hjust = 0.7) +
  geom_segment(aes(x = as.Date("2018-01-22"), xend = as.Date("2018-01-22"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2018-01-22")]),
               linetype = "dashed", color = "green") +
  geom_segment(aes(x = as.Date("2018-07-09"), xend = as.Date("2018-07-09"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2018-07-09")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2018-09-17"), xend = as.Date("2018-09-17"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2018-09-17")]),
               linetype = "dashed", color = "green") +
  geom_segment(aes(x = as.Date("2019-01-16"), xend = as.Date("2019-01-16"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2019-01-16")]),
               linetype = "dashed", color = "green") +
  geom_segment(aes(x = as.Date("2019-08-09"), xend = as.Date("2019-08-09"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2019-08-09")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2020-03-11"), xend = as.Date("2020-03-11"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2020-03-11")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2020-07-10"), xend = as.Date("2020-07-10"),
                   yend = 150, y = date_close$Close[date_close$Date == as.Date("2020-07-10")]-3),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2021-01-19"), xend = as.Date("2021-01-19"),
                   yend = 150, y = date_close$Close[date_close$Date == as.Date("2021-01-19")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2021-04-21"), xend = as.Date("2021-04-21"),
                   yend = 150, y = date_close$Close[date_close$Date == as.Date("2021-04-21")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2021-08-26"), xend = as.Date("2021-08-26"),
                   yend = 150, y = date_close$Close[date_close$Date == as.Date("2021-08-26")]),
               linetype = "dashed", color = "green") +
  geom_segment(aes(x = as.Date("2022-01-18"), xend = as.Date("2022-01-18"),
                   yend = 150, y = date_close$Close[date_close$Date == as.Date("2022-01-18")]),
               linetype = "dashed", color = "white") +
  geom_segment(aes(x = as.Date("2022-04-20"), xend = as.Date("2022-04-20"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2022-04-20")]),
               linetype = "dashed", color = "blue") +
  geom_segment(aes(x = as.Date("2023-05-18"), xend = as.Date("2023-05-18"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2023-05-18")]),
               linetype = "dashed", color = "green") +
  geom_segment(aes(x = as.Date("2023-10-17"), xend = as.Date("2023-10-17"),
                   y = 720, yend = date_close$Close[date_close$Date == as.Date("2023-10-17")]),
               linetype = "dashed", color = "green")



data %>%
  mutate(Rok = as.double(format(as.Date(Date), "%Y")),
         Miesiac = as.double(format(as.Date(Date), "%m")),
         Dzien = as.double(format(as.Date(Date), "%d"))) %>% 
  group_by(Miesiac) %>% 
  summarise(Zysk = mean(Close - Open)) %>% 
  arrange(-Zysk) ->miesiac #Lipiec

data %>%
  mutate(Rok = as.double(format(as.Date(Date), "%Y")),
         Miesiac = as.double(format(as.Date(Date), "%m")),
         Dzien = as.double(format(as.Date(Date), "%d"))) %>% 
  group_by(Dzien) %>% 
  summarise(Zysk = mean(Close - Open)) %>% 
  arrange(-Zysk) -> dzien_miesiaca #lipiec

data %>%
  mutate( Dzien_Tygodnia = weekdays(as.Date(Date))) %>% 
  group_by(Dzien_Tygodnia) %>% 
  summarise(Zysk = mean(Close-Open)) %>% 
  arrange(-Zysk) -> dzien_tygodnia

  
# Ustalamy kolejność dni tygodnia
kolejnosc_dni <- c("poniedziałek", "wtorek", "środa", "czwartek", "piątek")

# Przekształcamy kolumnę Dzien_Tygodnia na faktor z ustaloną kolejnością
dzien_tygodnia$Dzien_Tygodnia <- factor(dzien_tygodnia$Dzien_Tygodnia, levels = kolejnosc_dni)


data %>%
  mutate(Rok = as.double(format(as.Date(Date), "%Y")),
         Miesiac = as.double(format(as.Date(Date), "%m")),
         Dzien = as.double(format(as.Date(Date), "%d"))) %>% 
  group_by(Miesiac, Dzien) %>% 
  summarise(Zysk = mean(Close - Open)) %>% 
  arrange(-Miesiac) ->miesiac #Lipiec


ggplot(miesiac, aes(Miesiac, Dzien, fill = Zysk)) + 
  geom_tile() +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black"), 
        plot.background = element_rect(fill = "black"),
        text = element_text(color = "darkred", size = 15), plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(color = "darkgreen", margin = margin(t = -10)),  # Ustawienie marginesów osi X
        axis.text.y = element_text(color = "darkgreen", margin = margin(r = -10))) +
  theme(panel.grid = element_blank()) +
  scale_fill_gradient2(low = "red", high = "darkgreen", mid="white", name = "Różnica\n     w\ndolarach") +
  scale_x_continuous(breaks = 1:12) +
  scale_y_continuous(breaks = 1:31) +
  labs(x = "Miesiąc", y = "Dzień", title = "Średnia różnica wartości akcji Netflixa pomiędzy zamknięciem a otwarciem giełdy")
  













  

]








  
  

