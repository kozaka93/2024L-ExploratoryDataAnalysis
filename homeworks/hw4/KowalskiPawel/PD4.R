library(igraph)
library(ggraph)
library(tidygraph)
library(dplyr)


edges <- read.csv("C:/Users/kowal/Desktop/projekt/edges.csv")
nodes <- read.csv("C:/Users/kowal/Desktop/projekt/nodes.csv")



pomoc_do_zliczenia <- c(edges$X..source, edges$target)
pomoc_do_zliczenia <- as.data.frame(pomoc_do_zliczenia)

# zliczenie ile krawędzi idzie od każdego wierzchołka
wielkosc_kuli <- pomoc_do_zliczenia %>% 
  group_by(pomoc_do_zliczenia) %>% 
  summarise(n = n())

dwa_piec <- data.frame(pomoc_do_zliczenia = 25, n = 0.5)

dwa_dziewiec <- data.frame(pomoc_do_zliczenia = 29, n = 0.5)

# ustalenie ostatecznej ramki danych mówiącej ile krąwędzi idzie od każdej kuli
wielkosc_kuli <- rbind(wielkosc_kuli[1:25, ], dwa_piec, wielkosc_kuli[26:nrow(wielkosc_kuli), ])
wielkosc_kuli <- rbind(wielkosc_kuli[1:29, ], dwa_dziewiec, wielkosc_kuli[30:nrow(wielkosc_kuli), ])

# ustalenie sensownych wielkosci kul
min_interactions <- min(wielkosc_kuli$n)
max_interactions <- max(wielkosc_kuli$n)

min_vertex_size <- 5
max_vertex_size <- 20

# normalizacja wielkości kuli
map_size <- function(x) {
  return(min_vertex_size + (max_vertex_size - min_vertex_size) * (x - min_interactions) / (max_interactions - min_interactions))
}

vertex_sizes <- sapply(wielkosc_kuli$n, map_size)


montreal <- read.graph("C:/Users/kowal/Desktop/Wstep do eksploracyji/montreal.gml", format = "gml")

# współrzędne x dla każdego wierzchołka (pewnie dało się to zrobić lepiej, sprytniej i czytelniej nie ręcznie)
x_coords <- c(40, 50, 30, 40, 55, 60, 70, 80, 70, 70,
              20, 65, 30, 40, 50, 60, 65, 80, 90, 45,
              80, 20, 30, 80, 20, 100, 90, 80, 100, 100,
              50, 20, 30, 40, 90)
# Współrzędne y dla każdego wierzchołka
y_coords <- c(70, 50, 10, 10, 0, 70, 10, 10, 65, 80,
              20, 100, 75, 95, 110, 15, 40, 20, 30, 0,
              70, 60, 30, 60, 100, 60, 110, 100, 100, 80,
              20, 40, 50, 40, 90)

custom_layout <- cbind(x_coords, y_coords)

# rysowanie grafu
plot.igraph(montreal,
            vertex.size = vertex_sizes,
            vertex.color = 'lightblue',
            vertex.frame.color = 'blue',
            vertex.frame.width = 1,
            vertex.shape = 'circle',
            vertex.label.cex = 1.5,
            vertex.label.color = 'darkblue',
            vertex.label.dist = 0,
            edge.color = 'darkgrey',
            edge.width = 2,
            edge.arrow.size = 2,
            edge.lty = 1,
            edge.curved = FALSE, 
            layout = custom_layout,
            margin = 0,
            rescale = TRUE,
            asp = 0.5,
            frame = FALSE,
            main = 'Relacje między gangami',
            cex = 3)

