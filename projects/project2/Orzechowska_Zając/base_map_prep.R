states <- map_data("state")
ny_df <- subset(states, region == "new york")
counties <- map_data("county")
ny_county <- subset(counties, region == "new york" & subregion == "new york")
ny_base <- ggplot(data = ny_county, mapping = aes(x = long, y = lat, group = group)) + 
  coord_map("albers", 25, 50) +
  geom_polygon(color = "#013d5a", fill = "#708c69")