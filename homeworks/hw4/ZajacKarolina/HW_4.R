#install.packages('plotly')
install.packages('tidyverse')
# Załadowanie potrzebnych pakietów
library(plotly)
library(dplyr)
library(tidyverse)

sciezka <- "C:/Users/User/Desktop/wizualizacja/pd4/GDP by Country 1999-2022.csv"
dane <- read.csv(sciezka)
sciezka2  <- "C:/Users/User/Desktop/wizualizacja/pd4/national-gdp-constant-usd-wb.csv"
dane2 <- read.csv(sciezka2)
iso <- "C:/Users/User/Desktop/wizualizacja/pd4/wikipedia-iso-country-codes.csv"
isocodes <- read.csv(iso)

#kolumna "Country" staje sie indeksem
dane <- dane %>%
  tibble::column_to_rownames(var = "Country")


isocodes <- isocodes %>%
  rename(Country = "English.short.name.lower.case",
         iso_alpha = "Alpha.3.code") 
#polaczmy tabelki
df <- merge(dane, isocodes, by = "Country", all.x = TRUE)

#wybieram konkretnie rok 2022
year <- "X2022"
ds <- df %>%
  select(Country, iso_alpha, !!year) %>% 
  mutate(log = log(as.numeric(!!sym(year)))) %>% 
  filter(!is.na(log) & log != -Inf)

ds <- ds %>%
  mutate(index = row_number()) %>%
  select(-index)


fig <- plot_geo(ds, locationmode = "ISO-3") %>%
  add_trace(
    type = "choropleth",
    z = ~X2022,  
    locations = ~iso_alpha,
    text = ~paste(Country, ": ", format(X2022, scientific = FALSE)),
    colorscale = "Plasma",
    colorbar = list(title = "Gross Domestic Product (value)")
  ) %>%
  layout(
    title = list(
      text = paste("Gross Domestic Product", substr(year, start = 2, stop = nchar(year))),
      y = 0.9,  
      x = 0.5,  
      xanchor = "center",  
      yanchor = "top",  
      font = list(weight = "bold")  
    ),
    geo = list(
      showframe = TRUE,
      showcoastlines = TRUE
    )
  )
fig

