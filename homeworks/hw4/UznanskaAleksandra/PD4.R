library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
#install.packages("tidyr")
library(tidyr)

df <- read.csv("Animal_dataset.csv")
df <- na.omit(df)

split_predators <- function(predators) {
  return(unlist(strsplit(as.character(predators), ", ")))
}
df$Predators <- lapply(df$Predators, split_predators)

predators_df <- df[,c("Predators","Animal")]
head(predators_df,30)

predators_df <- predators_df%>%
  unnest(Predators)

predator_pairs <- expand.grid(Predator1 = predators_df$Predators, Predator2 = predators_df$Predators)
predator_pairs <- predator_pairs %>%
  filter(Predator1 != Predator2)

predator_pairs <- predator_pairs %>%
  inner_join(predators_df, by = c("Predator1" = "Predators")) %>%
  inner_join(predators_df, by = c("Predator2" = "Predators")) %>%
  filter(Animal.x == Animal.y) %>%
  select(-Animal.x, -Animal.y)

predators_df_filtered <- predator_pairs %>%
  filter(Predator1 != "Humans" & Predator1 != "Not Applicable" & Predator2 !=  "Humans" & Predator2 != "Not Applicable" )
predators_df_filtered

graph <- predators_df_filtered %>%
  graph_from_data_frame(directed = FALSE) 
?plot.igraph
plot.igraph(graph,
            vertex.size = 5,
            vertex.color = 'lightblue',
            vertex.frame.color = 'lightblue',
            vertex.frame.width = 1,
            vertex.shape = 'circle',
            vertex.label.cex = 0.67,
            width = 0.8,
            vertex.label.color = 'black',
            vertex.label.dist = 0,
            edge.color = 'darkgrey',
            edge.width = 2,
            edge.lty = 1,
            edge.curved = FALSE, 
            layout = layout_with_kk,  #layout_with_kk
            margin = 0,
            rescale = TRUE,
            asp = 0.6,
            frame = FALSE,
            main = 'Competition for food between selected carnivorous animals',
            edge.arrow.size=0.5, 
            vertex.size=5, 
            vertex.label.cex=0.8)
?plot.igraph
title(sub = "In the above graph, two vertices have been connected by an edge if they have at least one food in common. ")

