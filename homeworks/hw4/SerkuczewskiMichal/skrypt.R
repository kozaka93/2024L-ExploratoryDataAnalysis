library(ggplot2)
library(maps)
library(mapdata)
library(dplyr)

data <- read.csv('FastFoodRestaurants.csv')
states <- map_data("state")
state_full <- c('Alabama', 'Alaska', 'Arizona', 'Arkansas', 'California', 'Colorado', 'Connecticut', 'Delaware', 'Florida', 'Georgia',
                'Hawaii', 'Idaho', 'Illinois', 'Indiana', 'Iowa', 'Kansas', 'Kentucky', 'Louisiana', 'Maine', 'Maryland', 'Massachusetts',
                'Michigan', 'Minnesota', 'Mississippi', 'Missouri', 'Montana', 'Nebraska', 'Nevada', 'New Hampshire', 'New Jersey', 'New Mexico',
                'New York', 'North Carolina','North Dakota', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania', 'Rhode Island', 'South Carolina',
                'South Dakota', 'Tennessee', 'Texas', 'Utah', 'Vermont', 'Virginia', 'Washington', 'West Virginia', 'Wisconsin', 'Wyoming')
state_short <-c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN",
                "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
state_full <- tolower(state_full)
state_dict <- data.frame(
  state_full = state_full,
  state_short = state_short
)

data <- data %>% 
  select(address,city,latitude,longitude,name,province)%>%
  mutate(name = toupper(name))

data$name[data$name=='SONIC DRIVE IN'] <- 'SONIC DRIVE-IN'

states <- left_join(states, state_dict, by = c("region" = "state_full"))


count_rest <- data %>%
  group_by(province, name) %>%
  summarise(count_restaurants = n()) %>%
  ungroup()

top_restaurants_by_state <- count_rest %>%
  group_by(province) %>%
  filter(count_restaurants == max(count_restaurants))%>%
  filter(province != 'Co Spgs')%>%
  group_by(province)%>%
  sample_n(1)

df_for_map <-merge(states,top_restaurants_by_state,
                       by.x = "state_short",
                       by.y = "province")

df_for_labels <- df_for_map%>%
  group_by(state_short)%>%
  summarise(long = mean(long),lat = mean(lat),group = mean(group),count_restaurants = mean(count_restaurants))

df_for_map %>%
  ggplot(mapping = aes(x = long,y=lat,group = group))+
  geom_polygon(aes(fill = name), color = "black", linewidth = 0.3) +
  geom_text(data = df_for_labels,aes(label = count_restaurants),color = 'darkred',size =5, hjust = 0.5, vjust = 0)+
  labs(x='',y='',title = 'W jakich stanach dominuje sieć jakich fast foodów?',fill = 'Nazwa fast food\'u')


  
  
  

  
