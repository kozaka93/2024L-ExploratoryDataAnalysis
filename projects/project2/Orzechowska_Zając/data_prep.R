data <- data %>% filter(!is.na(birth.year))
data <- data %>% filter(!is.na(usertype))
data <- data %>% filter(!is.na(gender)) %>% filter(gender != 0)

data <- data %>% mutate(age_group = ifelse(birth.year <= 1944, "80+", 
                                    ifelse(birth.year <= 1954, "70-79",
                                    ifelse(birth.year <= 1964, "60-69",
                                    ifelse(birth.year <= 1974, "50-59",
                                    ifelse(birth.year <= 1984, "40-49",
                                    ifelse(birth.year <= 1994, "30-39",
                                    ifelse(birth.year <= 2004, "20-29",
                                    ifelse(birth.year <= 2014, "<20")))))))))
data <- data %>%
  mutate(month = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
         month = as.numeric(format(month, "%m")))
data <- data %>%
  mutate(hour = as.POSIXct(starttime, format="%Y-%m-%d %H:%M:%OS"),
         hour = as.numeric(format(hour, "%H")))
data <- data %>%
  mutate(season = ifelse(month %in% c(12, 1, 2), "winter", 
                         ifelse(month %in% c(3:5), "spring",
                                ifelse(month %in% c(6:8), "summer", "autumn"))))

### CUSTOMER PREFERNECES

## data for violin plots
data1.1.1 <- data %>% group_by(gender) %>% 
  select(gender,tripduration) %>% 
  mutate(tripduration = tripduration/60)
data1.1.1 <- data1.1.1 %>% mutate(gender = ifelse(gender == 2, "female", "male"))
data1.1.1 <- data1.1.1 %>% mutate(gneder = as.factor(gender))
data1.1.1 <- data1.1.1 %>% filter(tripduration <= 80)
write.csv(data1.1.1, file = "data111.csv")

data1.1.2 <- data %>% group_by(season) %>% 
  select(season,tripduration) %>% 
  mutate(tripduration = tripduration/60)
data1.1.2 <- data1.1.2 %>% mutate(season = as.factor(season))
data1.1.2 <- data1.1.2 %>% filter(tripduration <= 80)
write.csv(data1.1.2, file = "data112.csv")

data1.1.3 <- data %>% group_by(age_group) %>% 
  select(age_group,tripduration) %>% 
  mutate(tripduration = tripduration/60)
data1.1.3 <- data1.1.3 %>% mutate(age_group = as.factor(age_group))
data1.1.3 <- data1.1.3 %>% filter(tripduration <= 80)
write.csv(data1.1.3, file = "data113.csv")

## num of spent minutes
data1.2.1 <- data %>% group_by(gender) %>% summarise(mean_td = mean(tripduration)/60)
data1.2.1 <- data1.2.1 %>% mutate(gender = ifelse(gender == 2, "female", "male"))
data1.2.1 <- data1.2.1 %>% mutate(gender = as.factor(gender))
data1.2.2 <- data %>% group_by(age_group) %>% summarise(mean_td = mean(tripduration)/60)
data1.2.2 <- data1.2.2 %>% mutate(age_group = as.factor(age_group))
data1.2.3 <- data %>% group_by(usertype) %>% summarise(mean_td = mean(tripduration)/60)
data1.2.3 <- data1.2.3 %>% mutate(usertype = as.factor(usertype))
data1.2.4 <- data %>% group_by(season) %>% summarise(mean_td = mean(tripduration)/60)
data1.2.4 <- data1.2.4 %>% mutate(season = as.factor(season))
write.csv(data1.2.1, file = "data121.csv")
write.csv(data1.2.2, file = "data122.csv")
write.csv(data1.2.3, file = "data123.csv")
write.csv(data1.2.4, file = "data124.csv")
## times of travel
data1.3.1 <- data %>% group_by(hour, gender) %>% summarise(n = n())
data1.3.1 <- data1.3.1 %>% mutate(gender = ifelse(gender == 2, "female", "male"))
data1.3.1 <- data1.3.1 %>% mutate(hour = as.factor(hour)) %>% 
  mutate(gender = as.factor(gender))
data1.3.2 <- data %>% group_by(hour, age_group) %>% summarise(n = n())
data1.3.2 <- data1.3.2 %>% mutate(hour = as.factor(hour)) %>% 
  mutate(age_group = as.factor(age_group))
data1.3.3 <- data %>% group_by(hour, usertype) %>% summarise(n = n())
data1.3.3 <- data1.3.3 %>% mutate(hour = as.factor(hour)) %>% 
  mutate(usertype = as.factor(usertype))
write.csv(data1.3.1, file = "data131.csv")
write.csv(data1.3.2, file = "data132.csv")
write.csv(data1.3.3, file = "data133.csv")

## num of rentals
data1.4.1 <- data %>% group_by(gender) %>% summarise(n = n())
data1.4.1 <- data1.4.1 %>% mutate(gender = ifelse(gender == 2, "female", "male"))
data1.4.1 <- data1.4.1 %>% mutate(gender = as.factor(gender))
data1.4.2 <- data %>% group_by(age_group) %>% summarise(n = n())
data1.4.2 <- data1.4.2 %>% mutate(age_group = as.factor(age_group))
data1.4.3 <- data %>% group_by(usertype) %>% summarise(n = n())
data1.4.3 <- data1.4.3 %>% mutate(usertype = as.factor(usertype))
data1.4.4 <- data %>% group_by(season) %>% summarise(n = n())
data1.4.4 <- data1.4.4 %>% mutate(season = as.factor(season))
write.csv(data1.4.1, file = "data141.csv")
write.csv(data1.4.2, file = "data142.csv")
write.csv(data1.4.3, file = "data143.csv")
write.csv(data1.4.4, file = "data144.csv")

## TRIPS

#start station

data2.1.1 <- data %>% 
  group_by(hour, start.station.latitude, start.station.longitude, usertype) %>% summarise(count = n())
help <- data2.1.1 %>% filter(usertype == "Subscriber") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data2.1.1 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data2.1.1 <- help

data2.1.2 <- data %>% 
  group_by(hour, start.station.latitude, start.station.longitude, usertype) %>% summarise(count = n())
help <- data2.1.2 %>% filter(usertype == "Subscriber") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data2.1.2 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data2.1.2 <- help

data2.2.1 <- data %>% 
  group_by(hour, start.station.latitude, start.station.longitude, usertype) %>% summarise(count = n())
help <- data2.2.1 %>% filter(usertype == "Customer") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data2.2.1 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data2.2.1 <- help

data2.2.2 <- data %>% 
  group_by(hour, start.station.latitude, start.station.longitude, usertype) %>% summarise(count = n())
help <- data2.2.2 %>% filter(usertype == "Customer") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data2.2.2 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data2.2.2 <- help

write.csv(data2.1.1, file = "data211.csv")
write.csv(data2.1.2, file = "data212.csv")
write.csv(data2.2.1, file = "data221.csv")
write.csv(data2.2.2, file = "data222.csv")

## end station

data3.1.1 <- data %>% 
  group_by(hour, end.station.latitude, end.station.longitude, usertype) %>% summarise(count = n())
help <- data3.1.1 %>% filter(usertype == "Subscriber") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data3.1.1 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data3.1.1 <- help

data3.1.2 <- data %>% 
  group_by(hour, end.station.latitude, end.station.longitude, usertype) %>% summarise(count = n())
help <- data3.1.2 %>% filter(usertype == "Subscriber") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data3.1.2 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data3.1.2 <- help

data3.2.1 <- data %>% 
  group_by(hour, end.station.latitude, end.station.longitude, usertype) %>% summarise(count = n())
help <- data3.2.1 %>% filter(usertype == "Customer") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data3.2.1 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data3.2.1 <- help

data3.2.2 <- data %>% 
  group_by(hour, end.station.latitude, end.station.longitude, usertype) %>% summarise(count = n())
help <- data3.2.2 %>% filter(usertype == "Customer") %>% filter(hour == 0) %>% arrange(desc(count)) %>% head(20)
for (i in 1:23){
  x <- data3.2.2 %>% filter(hour == i) %>% arrange(desc(count)) %>% head(20)
  help <- rbind(help, x)
}
data3.2.2 <- help

write.csv(data3.1.1, file = "data311.csv")
write.csv(data3.1.2, file = "data312.csv")
write.csv(data3.2.1, file = "data321.csv")
write.csv(data3.2.2, file = "data322.csv")