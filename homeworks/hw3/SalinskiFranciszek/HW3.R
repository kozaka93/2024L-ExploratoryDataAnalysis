# Skrypt generujący poprawiony wykres z wynikami wyborów

library(dplyr)
library(ggplot2)
library(forcats)

generate_data <- function() {
  candidates <- c("Wiktor Wypych",
                 "Maria Łapczyńska",
                 "Kacper Gabrychowicz",
                 "Antoni Schmelter",
                 "Konrad Konopa")

  votes <- c(153, 91, 37, 42, 52)

  results <- data.frame(candidate = candidates, votes = votes)
  
  results
}

plot_results <- function(data) {
  data %>% 
    mutate(votes_percents = votes / sum(votes) * 100) %>%
    ggplot(aes(x = fct_reorder(candidate, -votes), y = votes_percents)) +
    geom_col(fill = "skyblue") +
    theme_bw() +
    labs(title = "Wyniki wyborów na przewodniczącego samorządu w IV LO",
         x = "Kandydat",
         y = "Odesetek głosów") +
    geom_text(aes(label = paste0(round(votes_percents, 0), "%")), vjust = -0.5) +
    theme(axis.text.x = element_text(size = 12),
          plot.title = element_text(size = 18)) -> plot
    
  plot
}

data <- generate_data()
p <- plot_results(data)
p
