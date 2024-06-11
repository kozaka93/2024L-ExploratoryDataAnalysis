### CUSTOMER PREFERNECES

## violin plots

ggplot(data1.1.1, aes(x = gender, y = tripduration)) + 
  geom_violin(fill = "#013d5a", color = "#013d5a") + 
  labs(x = "gender", y = "trip duration") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.1.2, aes(x = season, y = tripduration)) + 
  geom_violin(fill = "#013d5a", color = "#013d5a") + 
  labs(x = "season", y = "trip duration") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.1.3, aes(x = age_group, y = tripduration)) + 
  geom_violin(fill = "#013d5a", color = "#013d5a") + 
  labs(x = "age group", y = "trip duration") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

## num of spent minutes

ggplot(data1.2.1, aes(x = gender, y = mean_td)) +
  geom_col(fill = "#bdd3ce") +
  labs(x = "gender", y = "mean trip duration") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.2.2, aes(x = age_group, y = mean_td)) +
  geom_col(fill = "#bdd3ce") +
  labs(x = "age group", y = "mean trip duration") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.2.3, aes(x = usertype, y = mean_td)) +
  geom_col(fill = "#bdd3ce") +
  labs(x = "user type", y = "mean trip duration") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.2.4, aes(x = season, y = mean_td)) +
  geom_col(fill = "#bdd3ce") +
  labs(x = "season", y = "mean trip duration") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

## times of travel

ggplot(data1.3.1[data1.3.1$gender == "female", ], aes(x = hour, y = n)) +
  geom_col(fill = "#708c69") +
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )
ggplot(data1.3.1[data1.3.1$gender == "male", ], aes(x = hour, y = n)) +
  geom_col(fill = "#708c69") +
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.3.2[data1.3.2$age_group == "20-29", ], aes(x = hour, y = n, group = 1)) + 
  geom_col(fill = "#708c69") + 
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") + 
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )
ggplot(data1.3.2[data1.3.2$age_group == "80+", ], aes(x = hour, y = n, group = 1)) +
  geom_col(fill = "#708c69") +
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )
ggplot(data1.3.2[data1.3.2$age_group == "30-39", ], aes(x = hour, y = n, group = 1)) +
  geom_col(fill = "#708c69") +
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.3.2[data1.3.2$age_group == "30-39", ], aes(x = hour, y = n, group = 1)) +
  geom_col(fill = "#708c69") +
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

ggplot(data1.3.3[data1.3.3$usertype == "Customer", ], aes(x = hour, y = n, group = 1)) +
  geom_col(fill = "#708c69") +
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )
ggplot(data1.3.3[data1.3.3$usertype == "Subscriber", ], aes(x = hour, y = n, group = 1)) +
  geom_col(fill = "#708c69") +
  geom_line(color = "#43543f", linewidth = 1) + 
  labs(x = "hour", y = "num of ...") +
  theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "#fcf3e3", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#fcf3e3", color = NA)
  )

## num of rental

ggplot(data1.4.1, aes(x=gender, y=n)) +
  geom_segment( aes(x=gender, xend=gender, y=0, yend=n), 
                linewidth = 1, color = "#f4a258") +
  geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
  geom_text(aes(label=n), vjust=-2.5, hjust=0.5, 
            color="#013d5a", size = 2.3, fontface = "bold") +
  labs(x = "gender", y = "number of trips") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#fcf3e3"),
        panel.background = element_rect(fill = "#fcf3e3"),
        axis.text.x = element_text(size = 5.5, face = "bold"))

ggplot(data1.4.2, aes(x=age_group, y=n)) +
  geom_segment( aes(x=age_group, xend=age_group, y=0, yend=n), 
                linewidth = 1, color = "#f4a258") +
  geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
  geom_text(aes(label=n), vjust=-2.5, hjust=0.5, 
            color="#013d5a", size = 2.3, fontface = "bold") +
  labs(x = "age group", y = "number of trips") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#fcf3e3"),
        panel.background = element_rect(fill = "#fcf3e3"))

ggplot(data1.4.3, aes(x=usertype, y=n)) +
  geom_segment( aes(x=usertype, xend=usertype, y=0, yend=n), 
                linewidth = 1, color = "#f4a258") +
  geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
  geom_text(aes(label=n), vjust=-2.5, hjust=0.5, 
            color="#013d5a", size = 2.3, fontface = "bold") +
  labs(x = "age group", y = "number of trips") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#fcf3e3"),
        panel.background = element_rect(fill = "#fcf3e3"))

ggplot(data1.4.4, aes(x=season, y=n)) +
  geom_segment( aes(x=season, xend=season, y=0, yend=n), 
                linewidth = 1, color = "#f4a258") +
  geom_point( size=6, color="#f4a258", shape=19, stroke=2) +
  geom_text(aes(label=n), vjust=-2.5, hjust=0.5, 
            color="#013d5a", size = 2.3, fontface = "bold") +
  labs(x = "age group", y = "number of trips") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#fcf3e3"),
        panel.background = element_rect(fill = "#fcf3e3"))
