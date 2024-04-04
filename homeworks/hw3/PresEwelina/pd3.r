
library(dplyr)
library(ggplot2)
library(forcats)

ocena_rzadow <- c("Zdecydowanie źle", "Raczej źle", "Ani dobrze, ani źle", "Raczej dobrze", "Zdecydowanie dobrze")
procent <- c(19.4, 15, 28.3, 26.2, 11)
df <- data.frame(ocena_rzadow, procent)

df %>% 
  mutate(procent_ = paste(as.character(procent), "%")) %>% 
  ggplot(aes(y = fct_relevel(ocena_rzadow, ocena_rzadow), x = procent)) +
  geom_col(fill = "navyblue") +
  theme_bw() +
  geom_text(aes(label = procent_), hjust = -0.2, size = 4) +
  labs(title = "Jak Pani/Pan ocenia 100 dni rządu Donalda Tuska?",
       y = "",
       x = "Procent osób") +
  coord_cartesian(xlim = c(0, 30)) +
  theme(axis.text.y = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 13, hjust = 1),
        plot.title = element_text(size = 15, vjust = 2))
