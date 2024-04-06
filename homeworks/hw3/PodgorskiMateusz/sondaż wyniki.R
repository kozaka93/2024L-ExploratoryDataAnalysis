library(ggplot2)

# Dane, które odczytaliśmy z wykresu
parties <- c("LINKE", "BSW", "SPD", "B90/Die Grünen", "FDP", "CDU/CSU", "AfD", "Sonstige")
percentages <- c(3.1, 5.9, 15.4, 13.4, 4.8, 30.4, 18.4, 8.6)
change <- c(-1.8, 5.9, -10.3, -1.4, -6.7, 6.3, 8.1, -0.1)

# Tworzymy ramkę danych
data <- data.frame(parties, percentages, change)

library(ggplot2)
library(dplyr)

# Przygotowanie danych
data_percentages <- data %>%
  mutate(Type = "Procenty",
         Value = percentages)

data_changes <- data %>%
  mutate(Type = "Zmiana",
         Value = change)

# Połączenie danych w jeden dataframe
data_combined <- rbind(data_percentages[, c("parties", "Type", "Value")],
                       data_changes[, c("parties", "Type", "Value")])

# Tworzenie wykresu z facet_grid() dla podziału na 'Procenty' i 'Zmiana', zachowując jedną wspólną legendę
p <- ggplot(data_combined, aes(x = parties, y = Value, fill = parties)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%+.1f%%", Value)), vjust = -0.3, color = "black", position = position_dodge(width = 0.9)) +
  facet_wrap(~Type, scales = "free_y") + # Używamy facet_wrap dla podziału, 'scales = "free_y"' pozwala na różne skale Y dla 'Procenty' i 'Zmiana'
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Procentowy udział i zmiana głosów na partie",
       subtitle = "Na podstawie sondażu z marca 2024",
       x = "Partie",
       y = "Wartość") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.background = element_blank(), # Usuwa tło z tytułów paneli facetów
        strip.text.x = element_text(face = "bold")) # Formatuje tytuły paneli facetów

# Wyświetlenie wykresu
print(p)
