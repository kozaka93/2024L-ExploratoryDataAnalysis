#Wizualizacjia grafu

library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)

setwd("E:/Dokumenty/Studia PW/materiały/Semestr IV/Wstęp do eksporacji/Labiska/Pursko domowskie 4")
nodes_raw <- read.csv("network.csv/nodes.csv")
edges_raw <- read.csv("network.csv/edges.csv")

#Przygotowywanie wierzchołków według opisu danych ze strony internetowej

nodes <- mutate(nodes_raw, Allegiances = case_when(
  Allegiances == 1 ~ "Bloods", 
  Allegiances == 2 ~ "Crips", 
  Allegiances == 3 ~ "Other",
  TRUE ~ "No data")) %>%
  mutate(Ethnicity = case_when(
    Ethnicity == 1 ~ "Hispanic", 
    Ethnicity == 2 ~ "Afro-Canadian", 
    Ethnicity == 3 ~ "Caucassian",
    Ethnicity == 4 ~"Asian",
    Ethnicity == 4 ~"mixed/no ehnicity",
    TRUE ~ "No data")) %>%
  mutate(Territories = case_when(
    Territories == 1 ~ "Downtown", 
    Territories == 2 ~ "East", 
    Territories == 3 ~ "West",
    TRUE ~ "No data")) %>%
  mutate(Pos = gsub("array\\(\\[|\\[|\\]|\\)", "", X_pos)) %>%
  mutate(color = Allegiances) %>%
  separate(Pos, into = c("x", "y"), sep = ",", convert = TRUE)

#Przygotowywanie krawędzi, tak aby te, łączące gangi należące do tej samej frakcji
#miały ten sam kolor

edges <- edges_raw %>%
  rename(X..index = target) %>%
  left_join(nodes) %>%
  select(X..source ,X..index, name, Allegiances) %>%
  rename(target = X..index, X..index = X..source, name_1 = name, Allegiances_1 = Allegiances) %>%
  left_join(nodes)%>%
  select(target,X..index,name_1,Allegiances_1,name, Allegiances) %>%
  rename(X..source = X..index, name_2 = name, Allegiances_2 = Allegiances) %>%
  mutate(Same_fraction = (Allegiances_1 == "Crips" | Allegiances_1 == "Bloods") & (Allegiances_2 == "Crips" | Allegiances_2 == "Bloods")  & (Allegiances_1 == Allegiances_2))%>%
  mutate(Allegiance_net = case_when((Same_fraction == TRUE & Allegiances_1 == "Bloods") ~ "Bloods", (Same_fraction == TRUE & Allegiances_1 == "Crips") ~ "Crips", TRUE ~ "Other links"))%>%
  select(target,X..source,Allegiance_net)

#Tworzenie bazy dla grafu

ig <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)

#Wektory używanych kolorów

color_mapping_allegiances <- c("Bloods" = "red", "Crips" = "green", "Other" = "blue", "No data" = "black")
color_edge_mapping<- c("Bloods" = "orange", "Crips" = "darkgreen", "Other links" = "grey")


ig %>%
  ggraph(layout = "manual", x=nodes$x,y=nodes$y) +
  geom_node_point(aes(size = 4, color = Allegiances), alpha = 0.7) +
  scale_color_manual(values = color_mapping_allegiances) +
  geom_edge_link(aes(color = Allegiance_net),
                 n = 200,
                 alpha = 0.25,
                 width = 1) +
  scale_edge_color_manual(values = color_edge_mapping) +
  geom_node_text(aes(label = name, size = 4, color = color),
                                 repel = TRUE, 
                                 point.padding = unit(5, "mm")) +
  guides(Allegiances = TRUE,
         size = FALSE,
         scale = 'none') +
  labs(title = "Relations between gangs in Montreal")

ig %>%
  ggraph(layout = "hive", axis = Allegiances) +
  geom_node_point(aes(size = 4, color = Allegiances), alpha = 0.7) +
  scale_color_manual(values = color_mapping_allegiances) +
  geom_edge_link(aes(color = Allegiance_net),
                 n = 200,
                 alpha = 0.3,
                 width = 0.9) +
  scale_edge_color_manual(values = color_edge_mapping, guide = "none") +
  geom_node_text(aes(label = name, size = 4, color = color),
                 repel = TRUE, 
                 point.padding = unit(5, "mm")) +
  guides(Allegiance_net = FALSE,
          size = FALSE,
         scale = 'none') +
  labs(title = "Relations between gangs in Montreal")


