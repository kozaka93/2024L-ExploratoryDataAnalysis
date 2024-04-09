library(dplyr)
library(tidyr)
library(ggraph)
library(tidygraph)
library(stringr)
library(igraph)

dialogi <- read.csv("data/HIMYM.csv")


#Tworzenie tabeli relacji


#tworzymy tabelke
tabela <- data.frame(matrix(0, nrow = length(unique(dialogi$Character)), ncol = length(unique(dialogi$Character))))
colnames(tabela) <- unique(dialogi$Character)
rownames(tabela) <- unique(dialogi$Character)
#tytaj wektorowo wpisujemy characterystyczne słowa, mozna dodac emocje oraz relacja = 5 razy na przemian 2 osoby sie 
#do siebie zwaracają
for (i in 1:(nrow(dialogi) - 1)) {
  if (any(str_detect(dialogi$Line[i], c("you", "You", "yourself", "Yourself")))) {
    tabela[dialogi$Character[i + 1], dialogi$Character[i]] <- tabela[dialogi$Character[i + 1], dialogi$Character[i]] + 1
    tabela[dialogi$Character[i], dialogi$Character[i+1]] <- tabela[dialogi$Character[i], dialogi$Character[i+1]] + 1
  } else if (i < nrow(dialogi) - 5 && all(dialogi$Character[i:(i+5)] == dialogi$Character[i])) {
    tabela[dialogi$Character[i + 5], dialogi$Character[i]] <- tabela[dialogi$Character[i + 5], dialogi$Character[i]] + 1
    tabela[dialogi$Character[i], dialogi$Character[i+5]] <- tabela[dialogi$Character[i], dialogi$Character[i+5]] + 1
  }
}

#Krawędzie grafu
stopnie_krawdzie_tabelka <- tabela %>%
  mutate(from = rownames(tabela)) %>%
  pivot_longer(cols = c("Ted","Lily","Barney", "Marshall", 'Robin'), names_to = "to", values_to = "degree") %>%
  filter(from != to) %>%
  filter(to != "Ted") %>%
  filter(from == "Ted" | to != "Barney") %>%
  filter(from == "Ted" | from == "Barney" | to != "Marshall") %>%
  filter(from == "Ted" | from == "Barney" | from == "Marshall" | to != "Lily")

edge_list <- stopnie_krawdzie_tabelka


#Węzły grafu
node_list <- data.frame(id = colnames(tabela)) %>%
  mutate(label = id)

#Tworzymy graf
graf <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)

#Dobieramy odopowiednie parametry do wizualizacji

graf %>%
  ggraph(layout = "circle") +
  geom_node_point(aes(size = 100), colour = "yellow") +
  geom_edge_link(aes(width = (degree), colour = degree), 
                 
                 n = 100) +
  geom_node_text(aes(label = label)) +
  scale_edge_colour_gradient2(low = "red", high = "yellow") +
  guides(edge_width = FALSE,
         edge_alpha = FALSE,
         size       = FALSE,
         scale = 'none') +
  labs(title = 'Relation graph')

