library(dplyr)

# czyszczenie danych

dt <- read.csv("accident.csv")

dt <- dt %>%
  filter(HOUR <= 24) %>%
  mutate(
    time_of_day = case_when(
      HOUR >= 5 & HOUR < 11 ~ "Morning",
      HOUR >= 11 & HOUR < 18 ~ "Afternoon",
      HOUR >= 18 & HOUR < 22 ~ "Evening",
      TRUE ~ "Night")) %>%
  mutate(alc_involved = case_when(
    ALCHL_IM == 1 ~ "YES", 
    .default = "NO")) %>%
  mutate(injury_type = case_when(
    MAX_SEV == 4 | MAX_SEV ==6 ~ "DEATH", 
    MAX_SEV == 8 | MAX_SEV ==0 ~ "NO_HARM",
    .default = "INJURIES"
  ))


data_to_chart <- dt %>%
  select(CASENUM, time_of_day, alc_involved, injury_type, HOUR)

ggplot(data_to_chart, aes(x = time_of_day)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Accidents by Time of Day", x = "Time of Day", y = "Number of Accidents") +
  theme_minimal()


ggplot(data_to_chart, aes(x = time_of_day, fill = injury_type)) +
  geom_bar(position = "stack") +
  labs(title = "Injury Type by Time of Day", x = "Time of Day", y = "Number of Accidents", fill = "Injury Type") +
  theme_minimal()

ggplot(data_to_chart, aes(x = HOUR, fill = alc_involved)) +
  geom_bar(position = "stack") +
  labs(title = "Alcohol Involvement in Accidents by Time of Day", x = "Time of Day", y = "Number of Accidents", fill = "Alcohol Involved") +
  theme_minimal()



ggplot(data_to_chart, aes(x = HOUR)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Accidents by Hour of the Day", x = "Hour of the Day", y = "Number of Accidents") +
  theme_minimal()

write.csv(data_to_chart, file="data_clean_2.csv")


data <- dt %>%
  select(CASENUM, HARM_EVNAME)

write.csv(data, file="data_clean_3.csv")


harm_evname_freq <- data %>%
  count(HARM_EVNAME) %>%
  rename(freq = n)

wordcloud2(data = harm_evname_freq, size = 1, minSize = 0, gridSize = 0,  color = 'random-dark', backgroundColor = 'white')


dane <- read.csv("accident.csv")

wypadki_najwazniejsze <- dane %>%
  select(REGION, URBANICITY, MAX_SEV, NUM_INJ, ALCHL_IM, WKDY_IMNAME, MONTHNAME, HOUR)%>%
  filter(NUM_INJ < 90)

wypadki_najwazniejsze <- wypadki_najwazniejsze %>%
  mutate(czy_byl_alk = case_when(
    ALCHL_IM == 1 ~ "YES", 
    .default = "NO"
  ))

wypadki_najwazniejsze <- wypadki_najwazniejsze %>%
  mutate(rodzaj_wyp = case_when(
    MAX_SEV == 4 | MAX_SEV ==6 ~ "DEATH", 
    MAX_SEV == 8 | MAX_SEV ==0 ~ "NO_HARM",
    .default = "INJURIES"
  ))

wypadki_najwazniejsze <- wypadki_najwazniejsze %>%
  mutate(reg = case_when(
    REGION == 3 ~ "South", 
    REGION == 2 ~ "Midwest",
    REGION == 4 ~ "West",
    REGION ==1 ~ "Northeast"
  ))

wypadki_najwazniejsze <- wypadki_najwazniejsze %>%
  mutate(area_type = case_when(
    URBANICITY == 1 ~ "Urban", 
    .default = "Rural"
  ))

write.csv(wypadki_najwazniejsze, "accidents_clean.csv")

pupa <- read.csv("accidents_clean.csv")
