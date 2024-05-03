install.packages('tidygraph')
install.packages('ggraph')
library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)

pldata <- read.csv("premier-league-matches.csv")

unihome <- pldata %>% 
  group_by(Home) %>% 
  summarise(n = n()) %>% 
  arrange(by = -n) %>% 
  head(25)
unihome <- unihome[, -2]

ramka <- pldata %>% 
  filter(Home == "Arsenal" | Away == "Arsenal") %>% 
  filter((Home %in% unihome$Home & Away %in% unihome$Home)) %>% 
  mutate(Points = case_when(FTR == "D" ~ 1, 
                            ((FTR == "H" & Home == "Arsenal") | (FTR == "A" & Away== "Arsenal")) ~ 3,
                            ((FTR == "A" & Home == "Arsenal") | (FTR == "H" & Away== "Arsenal")) ~ 0)) %>% 
  mutate(Team = case_when(Home == "Arsenal" ~ Away, Home != "Arsenal" ~ Home))
ramka <- ramka[, -c(1,2,3,4,5,6,7,8)] 

sums <- ramka %>% 
  group_by(Team) %>% 
  summarise(Suma = sum(Points)) %>% 
  mutate(Arsenal = "Arsenal") %>% 
  arrange(by = -Suma)
sums <- sums[, c(3,1,2)]
#skróty
sums[3,2] <- "Newcastle"
sums[5,2] <- "Spurs"
sums[7,2] <- "Southam."
sums[8,2] <- "Man City"
sums[10,2] <- "Man Utd"
sums[11,2] <- "Leicester"
sums[18,2] <- "Leeds"
sums[20,2] <- "Norwich"

graph <- graph_from_data_frame(sums, directed = FALSE)

V(graph)$color <- c("red", "royalblue", "brown", "grey", "purple", "white", "royalblue", "red", "lightblue", "white",
                    "red", "royalblue", "red3", "red", "royalblue", "red", "royalblue", "royalblue", "gold", "white", "green",
                    "red", "orange", "lightblue", "royalblue")
E(graph)$color <- c("grey", "darkgrey","grey", "darkgrey","grey", "darkgrey","grey", "darkgrey","grey", "darkgrey",
                    "grey", "darkgrey","grey", "darkgrey", "grey", "darkgrey","grey", "darkgrey","grey", "darkgrey",
                    "grey", "darkgrey","grey", "darkgrey")
E(graph)$weight <- sums$Suma/4
plot(graph, edge.width = E(graph)$weight, main = "Z kim Arsenal zdobył najwięcej punktów?",
     layout = layout_as_star(graph), vertex.label.color = 'black',
     vertex.label.font = 2)

