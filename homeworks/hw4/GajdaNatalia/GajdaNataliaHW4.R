library(geojsonio)
library(dplyr)
library(ggplot2)
library(leaflet)

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
#https://www.kaggle.com/datasets/anandhuh/usa-statewise-latest-covid19-data
covid_daily <- read.csv("/Users/nataliagajda/Downloads/USA Covid Data.csv")

merged <- merge(states, covid_daily, by.x = "name", by.y = "USA.State", all.x = TRUE )
merged$Total.Cases <- as.numeric(merged$Total.Cases)

bins_number <- 5
quantiles <- quantile(merged$Total.Cases, probs = seq(0, 1, length.out = bins_number + 1), na.rm = TRUE)
bins <- unique(quantiles)
pal <- colorBin("YlOrRd", domain = merged$Total.Cases, bins = bins)


######## CASES

#bardzo męczyłam to labels, bo nie działało mi tak jak na zajęciach, stany były kompletnie pomieszane,
#więc niestety nie wiem czy to optymalny sposób

labels <- lapply(1:nrow(merged), function(i) {
  state_name <- merged$name[i]
  cases <- merged$Total.Cases[i]
  label <- paste(state_name, "total COVID-19 cases: ", cases)
  label
})

m <- leaflet(merged) %>%
  setView(-100, 37.8, 3.5) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal(Total.Cases),
    weight = 2,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "black",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )

m

######### DEATHS

quantiles2 <- quantile(merged$Total.Deaths, probs = seq(0, 1, length.out = bins_number + 1), na.rm = TRUE)
bins2 <- unique(quantiles2)

pal2 <- colorBin("YlOrRd", domain = merged$Total.Deaths, bins = bins2)

labels2 <- lapply(1:nrow(merged), function(i) {
  state_name <- merged$name[i]
  deaths <- merged$Total.Deaths[i]
  label <- paste(state_name, "total COVID-19 deaths: ", deaths)
  label
})

m2 <- leaflet(merged) %>%
  setView(-100, 37.8, 3.5) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~pal2(Total.Deaths),
    weight = 2,
    opacity = 1,
    color = "black",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "black",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = labels2,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  )

m2


library(htmlwidgets) 
saveWidget(m, file = "covidcases.html")
saveWidget(m2, file = "coviddeaths.html")


t <- "<!DOCTYPE html>
  <html lang='en'>
  <head>
  <meta charset='UTF-8'>
  <meta name='viewport' content='width=device-width, initial-scale=1.0'>
  <title></title>
  </head>
  <body>
  
  <h1>COVID-19 in the USA </h1>
  
  <p>In the interactive maps below, we can see the number of COVID-19 cases
and the number of deaths related to it for each state in the United States as on September 15, 2023.
</p>
  <p>To create the visualization I used a dataframe from Kaggle.
To explore the data further, you can access the Kaggle dataset <a href='https://www.kaggle.com/datasets/anandhuh/usa-statewise-latest-covid19-data'>here</a>.</p>
  
  <p> It contains information such as state, number of cases, deaths, recovered etc.. Only first three will be relevant for this project.<p>

<p>To read the data, move the cursor over the state you are interested in. The name of the state and the number of cases or deaths will then be shown.
What we can observe is that the number of deaths is rather directly proportional to the number of cases - 
  where were the most cases, there were the most deaths. In both categories California is an absolute leader.
</p>
  
 <h1>COVID-19 cases by state</h1>
  <iframe src='covidcases.html' width='800' height='600'></iframe>
  
  <h1>COVID-19 deaths by state</h1>
  <iframe src='coviddeaths.html' width='800' height='600'></iframe>

  
  </body>
  </html> "



writeLines(t, "covid_usa.html")
