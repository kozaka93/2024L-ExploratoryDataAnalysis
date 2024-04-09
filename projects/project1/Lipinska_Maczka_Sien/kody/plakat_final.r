#wczytanie
library(dplyr)
library(ggplot2)
library(mapdata)

data <- read.csv("final_dataset.csv")

#######################################################################################
#filtrowanie

data%>%
  filter(workcount>0,rating_count>10, review_count>10)%>%
  filter(fan_count>=quantile(fan_count,0.75), average_rate>=quantile(average_rate, 0.75))%>%
  mutate(born=as.Date(born), died=as.Date(died))%>%
  mutate(zaangazowanie_spolecznosci=rating_count/workcount)->dobrzy_autorzy
  

##############################################
#filtrowanie częstotliwości wypuszczania prac

dobrzy_autorzy%>%
  filter(!is.na(born))%>%
  mutate(died=ifelse(is.na(died), 2024-03-27, died ))%>%
  mutate(born=as.Date(born), died=as.Date(died,origin = "1970-01-01"))%>%
  mutate(dlugosc_zycia=as.numeric(died-born))%>%
  filter(dlugosc_zycia>5400)%>%
  mutate(czestotliwosc_publikacji=workcount/dlugosc_zycia)%>%
  summarise(kwantyle=quantile(czestotliwosc_publikacji, c(0.2, 0.8)))

# kwantyle
# 1 0.001083123
# 2 0.007962563

###########################################
#średnie ocen dobrych autorów

dobrzy_autorzy%>%
  summarise(kwantyle=quantile(average_rate, c(0.2, 0.8)))

# kwantyle
# 1     4.14
# 2     4.34

###########################################
#średnia ilość fanów dobrych autorów

dobrzy_autorzy%>%
  summarise(kwatnyle=quantile(fan_count, c(0.2,0.8)))

# kwatnyle
# 1     94.0
# 2    594.2

##########################################
#średnie zaangażowanie fanóW dobrych autorów

dobrzy_autorzy%>%
  summarise(kwantyle=quantile(zaangazowanie_spolecznosci, c(0.2,0.8)))

# kwantyle
# 1  60.14708
# 2 930.50833



#######################################################################################################
#tworzenie mapy


world_map <- map_data("world")


#filtrowanie wyników do mapy


data %>%
  filter(!is.na(died),workcount>0,rating_count>10, review_count>10, fan_count>=94,fan_count<=594,average_rate>=4.14, country!='' )%>%
  mutate(died=ifelse(is.na(died), 2024-03-27, died ))%>%
  mutate(born=as.Date(born), died=as.Date(died,origin = "1970-01-01"))%>%
  mutate(dlugosc_zycia=as.numeric(died-born),zaangazowanie_spolecznosci=rating_count/workcount)%>%
  mutate(czestotliwosc_publikacji=workcount/dlugosc_zycia)%>%
  filter(czestotliwosc_publikacji>=0.001083123, czestotliwosc_publikacji<=0.007962563, zaangazowanie_spolecznosci>=60.14708, zaangazowanie_spolecznosci<=930.50833)%>%
  group_by(country) %>%
  mutate(country = ifelse(country == "United States", "USA", country))%>%
  mutate(country = ifelse(country == "United Kingdom", "UK", country))%>%
  summarise(liczba = n())-> do_mapy




#łączenie danych, żeby można było zrobić mapę

merged_data <- left_join(world_map, do_mapy, by = c("region" = "country"), keep = TRUE)

#usunięcie na
merged_data$liczba[is.na(merged_data$liczba)] <- 0

do_mapy$liczba <- factor(do_mapy$liczba)



#rysowanie samego wykresu

ggplot(merged_data, aes(x = long, y = lat, group=group)) +
  geom_polygon(color = 'white') +
  labs(title = '',
       x = '', y = '',
       fill = 'Liczba kandydatów') +
  geom_polygon(data = merged_data[merged_data$liczba %in% '1', ], fill = '#660028')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '2', ], fill = '#99003c')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '3', ], fill = '#cc0050')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '4', ], fill = '#ff0065')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '7', ], fill = '#ff3283')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '8', ], fill = '#ff66a2')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '14', ], fill = '#ff99c1')+
  geom_polygon(data = merged_data[merged_data$liczba %in% '60', ], fill = '#ffffff')+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill='transparent'),
        axis.line = element_line(color = "white"),
        axis.text = element_text(color = "white"),
        plot.title = element_text(color = "white"))+
  coord_fixed()-> wykres

  ggsave('finalna_mapa.png',wykres,bg='transparent')







