library(dplyr)
library(ggplot2)
library(stringr)
library(patchwork)

file <- "https://raw.githubusercontent.com/jdegene/steamHWsurvey/master/shs_platform.csv"
df <- read.csv(file) %>% 
  filter(date == "2022-05-01") %>% # Dane kwietniowe zostaly upublicznione 1. maja
  filter(platform == "pc" & category == "Physical CPUs (Windows)") %>% 
  mutate(amount = as.numeric(str_extract(name, "\\d+"))) %>% 
  mutate(fin = ifelse(percentage >= 0.01, amount, "Others")) %>% 
  select(fin, amount, percentage)
  # Zakładam, że autor robił dane specyficznie dla windowsa,
  # bo nie jest to jasno sprecyzowane, ale nie ma to większego wpływu na wykres

df1 <- df %>% 
  group_by(fin) %>% 
  summarise(percentage = sum(percentage)) %>% 
  arrange(as.numeric(fin))

df2 <- filter(df, fin == "Others")

p1 <- ggplot(df1, aes(x = factor(fin,c(1:12,"Others")), y = percentage*100,
                      fill = (fin == "Others"))) +
        geom_col() +
        theme_bw() +
        theme(legend.position = "None") +
        scale_fill_manual(values = c("skyblue", "red")) +
        labs(x = "# of cores",
             y = "% of the total systems",
             title = "Distribution of Windows systems by amount of CPU cores",
             subtitle = "As of April 2022 Steam Survey")

p2 <- ggplot(df2, aes(x = as.factor(amount), y = percentage*100)) +
        geom_col(fill = "red") +
        theme_bw() +
        labs(x = "# of cores",
             y = "% of the total systems",
             title = "Distribution among the \"Others\"",
             subtitle = "\"Others\" classified as each being <1% of the total")

p1 / p2

