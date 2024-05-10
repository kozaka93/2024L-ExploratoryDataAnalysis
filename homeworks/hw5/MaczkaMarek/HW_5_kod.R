# Wczytanie potrzebnych pakietów
library(plotly)
library(dplyr)


# Wczytanie danych
dane <- read.csv("lfsa_eoqgan_linear.csv.gz")

# obróbka ramki - wyrzucenie krajów nie występujących na znalezionym wykresie
# i zawężenie danych do pożądanych
all <- dane %>% 
  filter(age == "Y20-64", TIME_PERIOD == 2023, citizen == "TOTAL") %>% 
  filter(!(geo %in% c("TR", "RS", "EA20", "BA"))) %>% 
  select(geo, sex, OBS_VALUE) %>%
  mutate(OBS_VALUE = as.numeric(OBS_VALUE)) %>% 
  arrange(desc(OBS_VALUE), desc(sex))


# pomoc w wyświetleniu przerw między odpowiednimi argumentami 
all[94,] <- list(" ", "T", 0)
all[95,] <- list("  ", "T", 0)
all[96,] <- list(" ", "M", 0)
all[97,] <- list("  ", "M", 0)
all[98,] <- list(" ", "F", 0)
all[99,] <- list("  ", "F", 0)

# nadanie odpowiednich kolejności argumentom
total <- all[all$sex == "T", ]
total <- total %>% 
  arrange(factor(geo,
                 levels = c("EU27_2020", " ", "ES", "EL", "CY", "IE", "AT", "SK",
                            "BE", "IT", "LT", "MT", "BG", "EE", "FR", "PL", "LV",
                            "DE", "SI", "PT", "FI", "RO", "HU", "NL", "SE",
                            "HR", "CZ", "DK", "LU", "  ", "CH", "NO", "IS")))

men <- all[all$sex == "M", ]
men <- men %>% 
  arrange(factor(geo,
                 levels = c("EU27_2020", " ", "ES", "EL", "CY", "IE", "AT", "SK",
                            "BE", "IT", "LT", "MT", "BG", "EE", "FR", "PL", "LV",
                            "DE", "SI", "PT", "FI", "RO", "HU", "NL", "SE",
                            "HR", "CZ", "DK", "LU", "  ", "CH", "NO", "IS")))
women <- all[all$sex == "F", ]
women <- women %>% 
  arrange(factor(geo,
                 levels = c("EU27_2020", " ", "ES", "EL", "CY", "IE", "AT", "SK",
                            "BE", "IT", "LT", "MT", "BG", "EE", "FR", "PL", "LV",
                            "DE", "SI", "PT", "FI", "RO", "HU", "NL", "SE",
                            "HR", "CZ", "DK", "LU", "  ", "CH", "NO", "IS")))

# rozszerzenie skrótów państw członkowskich UE 
total$geo <- list("EU", " ", "SPAIN", "GREECE", "CYPRUS", "IRELAND",
                  "AUSTRIA", "SLOVAKIA", "BELGIUM", "ITALY", "LITHUANIA",
                  "MALTA", "BULGARIA", "ESTONIA", "FRANCE", "POLAND",
                  "LATVIA", "GERMANY", "SLOVENIA", "PORTUGAL", "FINLAND",
                  "ROMANIA", "HUNGARY", "NETHERLANDS", "SWEDEN", "CROATIA",
                  "CZECHIA", "DENMARK", "LUXEMBOURG", "  ", "SWITZERLAND",
                  "NORWAY", "ICELAND")

men$geo <- total$geo
women$geo <- total$geo

# utworzenie zmiennej, która będzie służyła za tytuł wykresu
title <- paste0("<b>Over-qualification rate by sex, 2023</b><br><sub>",
                "% of over-qualified people aged 20 to 64",
                " with tertiary education, relative to all",
                " people with tertiary education",
                "</sub>")

# Wyświetlenie wykresu
plot_ly() %>% 
  add_trace(x = total$geo,
            y = total$OBS_VALUE,
            type = "bar", visible = T,
            marker = list(color = "rgb(62, 79, 189)"),
            name = "Total") %>% 
  add_trace(x = men$geo,
            y = men$OBS_VALUE,
            type = "scatter",
            mode = "markers", visible = T,
            marker = list(color = "red",
                          size = 10*(men$OBS_VALUE>0),
                          symbol = "circle"),
            name = "Men") %>% 
  add_trace(x = women$geo,
            y = women$OBS_VALUE,
            type = "scatter",
            mode = "markers", visible = T,
            marker = list(color = "orange",
                          size = 10*(women$OBS_VALUE>0),
                          symbol = "diamond"),
            name = "Women") %>%
  layout(title = list(text = title,
                      x = 0.03,
                      y = 0.95,
                      font = list(size = 20,
                                  family = "Arial")),
         hovermode = 'x',
         margin = list(l=100, r=20, t=100, b=40),
         xaxis = list(tickangle = 300,
                      categoryorder = "trace"),
         yaxis = list(range = c(0,40)),
         updatemenus = list(
           list(
             buttons = list(
               list(method = "restyle",
                    args = list("visible", list(TRUE, TRUE, TRUE)),
                    label = "All"),
               list(method = "restyle",
                    args = list("visible", list(TRUE, FALSE, FALSE)),
                    label = "Total"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, TRUE, FALSE)),
                    label = "Men"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, FALSE, TRUE)),
                    label = "Women"),
               list(method = "restyle",
                    args = list("visible", list(FALSE, TRUE, TRUE)),
                    label = "Men & Women")
             )
           )
         ))

