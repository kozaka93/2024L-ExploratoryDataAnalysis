
# załądowanie potrzebnych pakietów

library(dplyr)
library(igraph)
library(tidyr)
library(lubridate)

# wczytanie danych 
data <- read.csv("Airports2.csv")

# przefiltrowanie danych na odpowiedni rok i miesiąc
data2 <- data %>%
  mutate(date = ymd(Fly_date)) %>%  
  filter(year(date) == 1990 & month(date) == 1)

# oddzielenie kolumn tak, żeby uzyskać stany
samoloty <- separate(data2, Origin_city, into = c("O_City", "O_State"), sep = ", ", remove = FALSE)
samoloty <- separate(samoloty, Destination_city, into = c("D_City", "D_State"), sep = ", ", remove = FALSE)

# zamiana skrótów na pełne nazwy stanów
state_names <- data.frame(
  abbreviation = state.abb,
  full_name = state.name
)

samoloty <- samoloty %>%
  left_join(state_names, by = c("O_State" = "abbreviation")) %>%
  mutate(O_State = full_name) %>%
  left_join(state_names, by = c("D_State" = "abbreviation"), suffix = c("_O", "_D")) %>%
  mutate(D_State = full_name_D) %>%
  select(-full_name_O, -full_name_D)



# grupowanie i sumowanie połączeń
edges <- samoloty %>%
  group_by(O_State, D_State) %>%
  summarise(n = n()) %>%
  filter(O_State != D_State)



# wybranie 25 stanów 
top_states <- edges %>%
  group_by(O_State) %>%
  summarise(total_connections = sum(n)) %>%
  head(25) %>%
  arrange(total_connections)

# usunięcie połączeń w innych stanach
edges <- edges %>%
  filter(O_State %in% top_states$O_State & D_State %in% top_states$O_State)

# tworzenie grafu

g <- graph_from_data_frame(edges, directed = TRUE)

# informacja o stopniu wierzchołków
V(g)$degree <- degree(g)  

# dodanie odpowiednich nazw wierzchołkom
V(g)$label <- paste(V(g)$O_State, V(g)$D_State, sep = " - ")


# narysowanie grafu
plot(
  g, 
  layout = layout_with_kk(g), 
  vertex.label.cex = 0.8, 
  vertex.color='lightpink',
  edge.arrow.size = 0, 
  vertex.size = V(g)$degree/3, #przeskalowanie wielkości wierzchołków
  edge.color = "lightgrey",  
  label.dist = 10,
  vertex.frame.color='white',
  vertex.label.dist = 1
)
title(main = "Ruch lotniczy w stanach w styczniu 1990r.")
