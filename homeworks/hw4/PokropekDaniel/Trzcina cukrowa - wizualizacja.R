trzcina <- read.csv('/home/pop/Pobrane/archive/List of Countries by Sugarcane Production.csv')

library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)


country <- map_data("world")

trzcina <-trzcina%>%
mutate(Country = case_when(Country == "United States of America" ~ "USA",TRUE ~ Country))

df <- country %>%
  filter(region!='Antarctica')%>%
  left_join(trzcina, by = join_by(region==Country))

df%>%
  mutate(Production..Tons. = gsub("\\.", "", Production..Tons.))%>%
  ggplot(aes(x = long, y = lat, group = group))+ 
  geom_polygon(aes(fill = as.numeric(Production..Tons.)), color = "black", linewidth = 0.3)+
  coord_quickmap()+
  scale_fill_gradient(trans = "log", labels = scales::label_number(),
                      low = "white", high = "darkred")+
  labs(fill='liczba ton',
       title='PRODUKCJA ŁĄCZNA')+
  theme(panel.background = element_rect(fill = "#4F81BD"),
        plot.background = element_rect(fill = "#4F81BD"),
        legend.background = element_rect(fill = "#4F81BD"),
        legend.box.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        legend.text = element_text(size = 11),
        legend.title = element_text(size=13))


df%>%
  mutate(Yield..Kg...Hectare. = gsub("\\.", "", Yield..Kg...Hectare.))%>%
  mutate(Yield..Kg...Hectare.=as.numeric(gsub(",", ".",Yield..Kg...Hectare.)))%>%
  ggplot(aes(x = long, y = lat, group = group))+ 
  geom_polygon(aes(fill = as.numeric(Yield..Kg...Hectare.)), color = "black", linewidth = 0.3)+
  coord_quickmap()+
  scale_fill_gradient(low = "white", high = "darkred")+
  labs(fill='liczba kilogramów',
       title='PRODUCKJA NA JEDEN HEKTAR')+
  theme(panel.background = element_rect(fill = "#4F81BD"),
        plot.background = element_rect(fill = "#4F81BD"),
        legend.background = element_rect(fill = "#4F81BD"),
        legend.box.background = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, size=20),
        legend.text = element_text(size = 11),
        legend.title = element_text(size=13))

