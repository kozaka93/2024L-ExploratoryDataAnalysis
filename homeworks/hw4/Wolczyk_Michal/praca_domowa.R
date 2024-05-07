library(dplyr)
library(ggplot2)
library(maps)
library(mapdata)
getwd()
df1 <- read.csv("BigmacPrice.csv")
df1$date <- as.Date(df1$date)
df2 <- transform(df1, date = format(date, "%d"), 
          year = format(date, "%Y"))
df2 <- df2 %>% 
  mutate(name = case_when(""))

df3 <- df2 %>% 
  group_by(name) %>%
  filter(year == 2022& month == "01") %>% 
  summarise(index = dollar_price / df2$dollar_price[df2$year == 2019& df2$month == "01" &  df2$name == name])

world <- map_data("world")
unique(world$region)http://127.0.0.1:22509/graphics/plot_zoom_png?width=2560&height=1370
europe <- world %>% filter(region %in% c("Spain", "France", "Germany", "Poland", "Italy", "UK", "Greece", "Portugal", "Norway", "Sweden", "Finland", "Denmark", "Ireland", "Netherlands", "Belgium", "Switzerland", "Austria", "Hungary", "Czech Republic", "Slovakia", "Romania", "Bulgaria", "Croatia", "Serbia", "Bosnia and Herzegovina", "Slovenia", "Montenegro", "Macedonia", "Albania", "Kosovo", "Ukraine", "Belarus", "Russia", "Lithuania", "Latvia", "Estonia", "Moldova", "Cyprus", "Luxembourg", "Malta", "Iceland", "Andorra", "Monaco", "Liechtenstein", "San Marino", "Vatican City")) %>% 
  filter( long <= 45 & lat <= 70)

wyk <- world %>%  
  left_join(df3, by = c("region" = "name")) 

wyk %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = wyk, aes(fill = index), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_gradient(low = 'darkred',high=  'green')

punkty <- wyk1 %>% 
  group_by(region) %>% 
  summarise(long = mean(long),lat = mean(lat),index = round(mean(index),2),group = mean(group))

wyk1 <- europe %>%  
  left_join(df3, by = c("region" = "name")) 
wyk1 %>% 
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(data = wyk1, aes(fill = index), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  labs(x ="",
       y="",
       title = "Ile razy droższy był Big Mac w 2022 roku od Big Maca w 2019 roku? (cena w dolarach)")+
  scale_fill_gradient(low = 'orange',high=  'darkred')+
  geom_text(data = punkty, aes(label = index),size =10, hjust = 0.5, vjust = 0)

