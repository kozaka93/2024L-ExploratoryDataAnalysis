# Niezbędne bibilioteki
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)

# Tworzenie wektorów dla kolumn
name <- c("Joe Biden", "Donald Trump")
topics <-
  rep(
    c(
      "Climate change?",
      "Abortion?",
      "Election integrity?",
      "Health care?",
      "Foreign policy?",
      "The economy?",
      "Immigration and \n border security"
    ),
    each = 2
  )
trust <- c(52, 40, 50, 44, 49, 46, 48, 46, 44, 52, 41, 55, 38, 58)
# Tworzenie ramki danych
df <- data.frame(name = name,
                 topic = topics,
                 trust = trust)

df %>%
  # Najpierw zadbajmy o kolejność
  mutate(topic = fct_inorder(topic)) %>%
  ggplot(aes(
    x = fct_reorder(topic, trust),
    y = trust,
    fill = name
  )) +
  # Zeby słupki stały obok
  geom_col(position = "dodge") +
  # Piękna prezentacja opisów
  geom_text(
    aes(label = percent(trust / 100)),
    position = position_dodge(width = 0.9),
    size = 4,
    vjust = 1.5,
    color = "white"
  ) +
  theme_minimal() +
  # Kolorki
  scale_fill_manual(values = c("Joe Biden" = "blue", "Donald Trump" = "red")) +
  # Upodobadnianie wykresu az do konca
  labs(
    x = "",
    y = "Trust",
    fill = "",
    title = "ARIZONA \n TRUST TO DO A BETTER JOB ON THE ISSUES",
    subtitle = "Regardless of how you might vote, who do you trust to do a better job on each of the following -- Joe Biden or \n Donald Trump?",
    label = "Trust"
  ) +
  scale_x_discrete(position = "top") +
  theme(
    axis.text.x = element_text(size = 9, color = "black"),
    legend.position = "top",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title.y = element_text(color = "black"),
    axis.title.x = element_blank()
  )


# Dziekuje za uwage, miłego dnia