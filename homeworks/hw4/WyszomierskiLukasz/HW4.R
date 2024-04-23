library(plotly)
library(dplyr)
library(stringr)
library(htmlwidgets)
df <- read.csv("owid-covid-data.csv")
world_population <- read.csv("world_population_data.csv")

world_population <- world_population %>% 
  select(country_code, X2021)

df <- df %>% 
  select(iso_code, location, date, total_cases, new_cases) %>% 
  mutate(date_without_day = str_extract(date, "[0-9]{4}-[0-9]{2}")) %>% 
  group_by(location, iso_code, date_without_day) %>% 
  summarise(month_new_cases = sum(new_cases)) %>% 
  left_join(world_population, by = c("iso_code" = "country_code")) %>% 
  mutate(new_cases_per_1000_inhabitants = (month_new_cases/X2021)*1000)

plot <- plot_ly(z = ~df$new_cases_per_1000_inhabitants, 
                locations = ~df$iso_code,
                frame = ~df$date_without_day,
                text = paste("Country: ", df$location, "<br>Infected per 1000 inhabitants: ", round(df$new_cases_per_1000_inhabitants, 4)),
                type = "choropleth",
                colorscale = "Reds",
                hoverinfo = 'text',
                colorbar = list(title = "<b>Infected per 1000 <br> inhabitants</b>",
                                tickfont = list(size = 14, family = "Arial"),
                                visible = TRUE)) %>%
  colorbar(len=1) %>% 
  animation_opts(frame = 700) %>%
  layout(
    title = "<b>Number of Covid-19 infected per 1000 inhabitants in each month</b>",
    titlefont = list(size = 20, family = "Arial"),
    margin = list(t = 100, l = 20, r = 20, b = 100),
    width = 900,
    height = 540
  )

frames <- list()

for (i in 1:length(unique(df$date_without_day))) {
  df_subset <- df[df$date_without_day == unique(df$date_without_day)[i], ]
  frames[[i]] <- list(
    data = list(
      z = ~df_subset$new_cases_per_1000_inhabitants, 
      locations = ~df_subset$iso_code
    ),
    layout = list(sliders = list(
      y = 0.9, 
      len = 0.5
    ),
    coloraxis = list(
      colorbar = list(title = "<b>Infected per 1000 <br> inhabitants</b>",
                      tickfont = list(size = 14, family = "Arial"),
                      visible = TRUE)
    )
    )
  )
}

plot <- plot %>% animation_slider(frames = frames, 
                                  currentvalue = list(prefix = "Date: ",
                                                      font = list(family = "Arial", color = "black", size = 14)),
                                  font = list(size = 14, family = "Arial"))

plot