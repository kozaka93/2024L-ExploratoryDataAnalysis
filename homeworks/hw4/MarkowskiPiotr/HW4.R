# załadowanie potrzebnych pakietów
library(igraph)
library(tidygraph)
library(ggraph)
library(dplyr)
library(tidyr)
library(lubridate)

# ładujemy dane z githuba
data <- read_graph("netscience.gml", format = "gml")

# bierzemy dane, 28 linijek, zeby nie bylo za duzo wierzcholkow
tg <- tidygraph::as_tbl_graph(data) %>% 
  tidygraph::activate(nodes) %>% 
  slice(1:28)

# tworzymy liste wierzchołków i krawędzi
edge_list <- tg %>%
  activate(edges) %>%
  data.frame()

node_list <- tg %>%
  activate(nodes) %>%
  data.frame()

node_list$degree <- rep(0, nrow(node_list))
node_list$id <- node_list$id + 1
for (i in 1:nrow(node_list)) {
  node_list$degree[i] <- sum(edge_list$from == node_list$id[i]) + sum(edge_list$to == node_list$id[i])
}


# Upewniamy się, że jest wystarczająco
# print(node_list)
# a <- node_list%>%
#   filter(degree>0)
# length(a[[1]])
# wierzchołków jest wiecej  niz 25

# towrzenie grafu
ig <- igraph::graph_from_data_frame(d = edge_list, vertices = node_list, directed = FALSE)

ig %>%
  ggraph(layout = "kk") +
  geom_node_point(aes(color = as.factor(degree)), size = 5, alpha = 0.9) + 
  scale_color_manual(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")) + 
  geom_edge_link(
    color = 'black',
    lineend = 'round',
    n = 25,
    alpha = 0.5
  ) +
  geom_node_text(aes(label = label),
                 repel = TRUE,
                 position =) +
  theme_graph(background = 'white',
              foreground = 'lightgray') +
  labs(title = 'Scientists cooperation network') +
  guides(color = guide_legend(title = "node degree"))  
