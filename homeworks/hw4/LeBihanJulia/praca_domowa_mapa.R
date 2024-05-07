library(dplyr)
library(plotly)


# Ramka WorldPopulationData2023 zawiera dane o gęstości zaludnienia w 2023 roku,
# ale nie zawiera kodów krajów
# Ramka world_population_data zawiera inne dane o gęstości zaludnienia i zawiera kody
# krajów.
# Będziemy korzystać z danych o gęstości zaludnienia z pierwszej ramki natomiast 
# kody krajów przeciągniemy z drugiej.

WorldPopulation2023 <- read.csv("C:\\Users\\juhan\\OneDrive\\Desktop\\LeBihanJulia\\WorldPopulation2023.csv")
ramka_z_kodami_krajow <- read.csv("C:\\Users\\juhan\\OneDrive\\Desktop\\LeBihanJulia\\world_population_data.csv")

ramka_z_kodami_krajow <- ramka_z_kodami_krajow %>% 
  rename("Country" = "country")

ramka_z_kodami_krajow <- ramka_z_kodami_krajow %>% 
  select("cca3", "Country")

gest_zalud_swiat <- merge(ramka_z_kodami_krajow, WorldPopulation2023, by = "Country")

plot_ly(gest_zalud_swiat, 
        z = ~Density.P.Km..,
        locations = ~cca3,
        type = "choropleth",
        locationmode = "ISO-3",
        colors = "Blues",
        hoverinfo = "text",
        text = ~paste("Country: ", Country, "<br>Density: ", Density.P.Km..),
        zmin =0,
        zmax = 600) %>%
  layout(title = "Gęstość zaludnienia na świecie (dane z 2023 roku)") %>%
  colorbar(title = "Gęstość (na km^2)")