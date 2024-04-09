#Projekt 1 
#Alicja Żebiałowicz, Bruno Podbielski, Łukasz Wyszomierski


library(scales)
library(xml2)
library(rvest)
library(dplyr)
library(ggplot2)
library(maps)
library(stringr)
library(ggimage)
library(ggthemes)
library(stringi)
library(srt)
library(png)
library(cropcircles)
library(igraph)


#################
# Mapa dochodów #
#################


url <- "https://www.boxofficemojo.com/title/tt0126029/?fbclid=IwAR2DXubskhlYNknR2_rwk5_2G5q3s73BaRnoF7w7dd5pKa4PKW7XPYPj-Qs"
page <- read_html(url)
table <- html_table(page, fill = TRUE)
df0 <- as.data.frame(table[2])
df1 <- as.data.frame(table[3])
df2 <- as.data.frame(table[4])
df3 <- as.data.frame(table[5])
df4 <- as.data.frame(table[6])
names(df0)[which(names(df0)=="Domestic")] <- "Country"
names(df1)[which(names(df1)=="EMEA")] <- "Country"
names(df2)[which(names(df2)=="APAC")] <- "Country"
names(df3)[which(names(df3)=="LATAM")] <- "Country"
names(df4)[which(names(df4)=="China")] <- "Country"
df <- rbind(df0, df1, df2, df3, df4)
df$Country[which(df$Country == "Domestic")] <- "USA"
df$Country[which(df$Country == "United Kingdom")] <- "UK"
df$Country[which(df$Country == "Türkiye")] <- "Turkey"
df$Lifetime.Gross <- str_replace_all(df$Lifetime.Gross, "\\$", "")
df$Lifetime.Gross <- str_replace_all(df$Lifetime.Gross, ",", "")
df$Lifetime.Gross <- as.numeric(df$Lifetime.Gross)
df <- map_data("world") %>% 
  filter(region != "Antarctica") %>% 
  left_join(df, join_by(region == Country)) 
gdp <- read.csv("C:/PDU/Pdu_projekt/WdED/PD1/gdp.csv")
gdp$Country.Name[which(gdp$Country.Name == "United States")] <- "USA"
gdp$Country.Name[which(gdp$Country.Name == "United Kingdom")] <- "UK"
df5 <- gdp %>% 
  select(Country.Name, X2001) %>% 
  right_join(df, join_by(Country.Name == region))%>% 
  mutate(income_per_gdp = case_when(is.na(Lifetime.Gross) ~ NA,
                                    !is.na(Lifetime.Gross) ~ Lifetime.Gross/X2001)) %>% 
  select(long, lat, group, income_per_gdp)


world_map <- ggplot(data = df5, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = income_per_gdp)) +
  scale_fill_gradient(low = "#B8BF33", high = "#303906", labels = scales::comma)+
  ggtitle("Income in dollars divided by GDP")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.title = element_blank(),
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_),
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_),
    legend.background = element_rect(fill="transparent",
                                     colour = NA_character_),
    text = element_text(size = 15)
  )

world_map
ggsave(
  plot = world_map,
  filename = "world_map.png",
  bg = "transparent"
)


#############################
# Wykres prędkości mówienia #
#############################

srt_shrek <- read_srt("Shrek.2001.1080p.BluRay.x264.Ganool.srt")
csv_shrek2 <- read.csv("shrek2.csv")
srt_shrek2 <- read_srt("Shrek2.srt")
csv_shrek3 <- read.csv("shrek3.csv") 

srt_shrek2 <- srt_shrek2 %>% 
  mutate(timestamp = round((end - start) / 2 + start, 4))
shrek2_A <- full_join(csv_shrek2, srt_shrek2, by = "timestamp")
shrek2_A <- na.omit(shrek2_A) %>%
  mutate(p=str_extract_all(subtitle, "[A-Z]+[A-Z]")) %>%
  mutate(m=str_extract_all(subtitle, "[:space:]+&+[:space:]")) %>%
  mutate(time = end - start, number_of_words = (stri_count_words(subtitle) 
                                                - stri_count_regex(subtitle, '<i>') - stri_count_regex(subtitle, '</i>')
                                                - ifelse(p=="character(0)", 0, ifelse(stri_count_words(p)==1, 1, stri_count_words(p)-1))
                                                - ifelse(m=="character(0)", 0, ifelse(stri_count_words(m)==1, 1, stri_count_words(m)-1)))) %>%
  select("name", "time", "number_of_words") %>%
  group_by(name) %>% 
  summarise(total_number_of_words = sum(number_of_words),
            total_time = sum(time),
            speed = (total_number_of_words / total_time) * 60) %>%
  filter(name %in% c("SHREK","DONKEY","FIONA","PUSS","FAIRY GODMOTHER","PRINCE CHARMING",
                     "PINOCHHIO","GINGY","QUEEN")) %>% 
  arrange(-speed)


fiona <- circle_crop("fiona.png", border_size = 4)
kot <- circle_crop("kot.png", border_size = 4)
krol <- circle_crop("krol.png", border_size = 4)
ksiaze <- circle_crop("ksiaze.png", border_size = 4)
osiol <- circle_crop("osiol.png", border_size = 4)
shrek <- circle_crop("shrek.png", border_size = 4)
wrozka <- circle_crop("wrozka.png", border_size = 4)
krolowa <- circle_crop("krolowa.png", border_size = 4)
pinokio <- circle_crop("pinokio.png", border_size = 4)
ciastek <- circle_crop("ciastek.png", border_size = 4)
images <- c(osiol, krolowa, kot, wrozka, ksiaze, fiona, shrek, ciastek)

p <- ggplot(shrek2_A, aes(x = reorder(name, speed), y = speed)) +
  geom_bar(stat = "identity", position = "dodge", fill = "#435914") +
  geom_image(aes(image = images, y = speed*0.8), size = 0.2)+
  labs(title = "Character's speaking speed",
       x = "Character",
       y = "Speed (words / min)") +
  theme_minimal()+
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_),
        plot.background = element_rect(fill = 'transparent',
                                       colour = NA_character_),
        text = element_text(size = 15))
p
ggsave(
  plot = p,
  filename = "Ala_ze_zdjeciami.png",
  bg = "transparent"
)

#####################################
# Graf interakcji między bohaterami #
#####################################

shrek2 <- read.csv("shrek2.csv")
shrek2 <- shrek2 %>% 
  mutate(scene = case_when(timestamp <= 151 ~ "Scene01",
                           151 < timestamp & timestamp <= 295 ~ "Scene03",
                           295 < timestamp & timestamp <= 521 ~ "Scene03",
                           521 < timestamp & timestamp <= 562 ~ "Scene04",
                           562 < timestamp & timestamp <= 670 ~ "Scene05",
                           670 < timestamp & timestamp <= 894 ~ "Scene06",
                           894 < timestamp & timestamp <= 1101 ~ "Scene07",
                           1101 < timestamp & timestamp <= 1410 ~ "Scene08",
                           1410 < timestamp & timestamp <= 1510 ~ "Scene09",
                           1510 < timestamp & timestamp <= 1622 ~ "Scene10",
                           1622 < timestamp & timestamp <= 1723 ~ "Scene11",
                           1723 < timestamp & timestamp <= 1760 ~ "Scene12",
                           1760 < timestamp & timestamp <= 1892 ~ "Scene14",
                           1892 < timestamp & timestamp <= 1955 ~ "Scene14",
                           1955 < timestamp & timestamp <= 2003 ~ "Scene15",
                           2003 < timestamp & timestamp <= 2240 ~ "Scene16",
                           2240 < timestamp & timestamp <= 2289 ~ "Scene17",
                           2289 < timestamp & timestamp <= 2340 ~ "Scene18",
                           2340 < timestamp & timestamp <= 2445 ~ "Scene19",
                           2445 < timestamp & timestamp <= 2498 ~ "Scene20",
                           2498 < timestamp & timestamp <= 2621 ~ "Scene21",
                           2621 < timestamp & timestamp <= 2830 ~ "Scene22",
                           2830 < timestamp & timestamp <= 2885 ~ "Scene23",
                           2885 < timestamp & timestamp <= 3100 ~ "Scene24",
                           3100 < timestamp & timestamp <= 3140 ~ "Scene25",
                           3140 < timestamp & timestamp <= 3289 ~ "Scene26",
                           3289 < timestamp & timestamp <= 3380 ~ "Scene27",
                           3380 < timestamp & timestamp <= 3465 ~ "Scene28",
                           3465 < timestamp & timestamp <= 3499 ~ "Scene29",
                           3499 < timestamp & timestamp <= 3511 ~ "Scene30",
                           3511 < timestamp & timestamp <= 3550 ~ "Scene31",
                           3550 < timestamp & timestamp <= 3616 ~ "Scene32",
                           3616 < timestamp & timestamp <= 3625 ~ "Scene33",
                           3625 < timestamp & timestamp <= 3723 ~ "Scene34",
                           3723 < timestamp & timestamp <= 3815 ~ "Scene35",
                           3815 < timestamp & timestamp <= 3876 ~ "Scene36",
                           3876 < timestamp & timestamp <= 3948 ~ "Scene37",
                           3948 < timestamp & timestamp <= 4041 ~ "Scene38",
                           4041 < timestamp & timestamp <= 4184 ~ "Scene39",
                           4184 < timestamp & timestamp <= 4193 ~ "Scene40",
                           4193 < timestamp & timestamp <= 4249 ~ "Scene41",
                           4249 < timestamp & timestamp <= 4340 ~ "Scene42",
                           4340 < timestamp & timestamp <= 4512 ~ "Scene43",
                           4512 < timestamp ~ "Scene44",)) %>% 
  filter(!name %in% c("SOUND FX", "MUSIC", "GUARD", "JEROME")) %>% 
  select(name, scene)


data <-shrek2

df1 <- data %>% 
  group_by(name, scene) %>% 
  summarise(n = n()) %>% 
  arrange(scene)


x <- list()
scene <- data$scene[1]
j <- 1
for (i in 1:44){
  if(j == length(df1$scene)){
    break
  }
  a <- list()
  while(df1$scene[j] == scene){
    if(!df1$name[j] %in% a){
      a <- append(a, df1$name[j])
    }
    if(j < length(df1$scene)){
      j <- j+1
    }
    else{
      break
    }
  }
  x <- append(x, list(a))
  if(j <= length(df1$scene)){
    scene <- df1$scene[j]
  }
}

fun <- function(x) combn(x, 2)
x <- lapply(x, fun)


df2 <- data.frame()
for (i in 1:41){
  n <- length(x[[i]])
  for(j in seq(from=1,to=n,by=2)){
    df2 <- rbind(df2, x[[i]][c(j, j+1)])
  }
}

names(df2) <- c("person1", "person2")
df2 <- df2 %>% 
  group_by(person1, person2) %>% 
  summarise(n = n()) %>% 
  filter(n >= 3)

net <- graph.data.frame(df2 %>% select(person1, person2), directed = FALSE)

fiona <- readPNG(circle_crop("fiona.png", border_size = 4))
kot <- readPNG(circle_crop("kot.png", border_size = 4))
krol <- readPNG(circle_crop("krol.png", border_size = 4))
ksiaze <- readPNG(circle_crop("ksiaze.png", border_size = 4))
osiol <- readPNG(circle_crop("osiol.png", border_size = 4))
shrek <- readPNG(circle_crop("shrek.png", border_size = 4))
wrozka <- readPNG(circle_crop("wrozka.png", border_size = 4))
krolowa <- readPNG(circle_crop("krolowa.png", border_size = 4))
pinokio <- readPNG(circle_crop("pinokio.png", border_size = 4))
ciastek <- readPNG(circle_crop("ciastek.png", border_size = 4))

img <- list(osiol, wrozka, fiona, ciastek, krol, pinokio, kot, krolowa, shrek, ksiaze)

set.seed(1)    
l <- layout_in_circle(net)
par(mar=rep(0,4))
plot(net, layout=l, vertex.label="", edge.color = adjustcolor("#435914", 0.7), edge.width= df2$n)
for(i in 1:nrow(l)) {  
  rasterImage(img[[i]], l[i, 1]-0.15, l[i, 2]-0.15, l[i, 1]+0.15, l[i, 2]+0.15)
}

####################################
# Rozkład liczby słów w wypowiedzi #
####################################

srt_shrek <- read_srt("Shrek.2001.1080p.BluRay.x264.Ganool.srt")
csv_shrek2 <- read.csv("shrek2.csv")
srt_shrek2 <- read_srt("Shrek2.srt")
csv_shrek3 <- read.csv("shrek3.csv") 

shrek <- na.omit(srt_shrek) %>%
  mutate(p=str_extract_all(subtitle, "\\b[A-Z]+[A-Z]")) %>%
  mutate(m=str_extract_all(subtitle, "[:space:]+&+[:space:]")) %>%
  mutate(number_of_words=(stri_count_words(subtitle)
                          - ifelse(p=="character(0)", 0, ifelse(stri_count_words(p)==1, 1, stri_count_words(p)-1))
                          - ifelse(m=="character(0)", 0, ifelse(stri_count_words(m)==1, 1, stri_count_words(m)-1)))) %>%
  mutate(timestamp = round((end - start) / 2 + start, 4)) %>%
  mutate(scene = case_when(timestamp <= 0 ~ "Scene01",
                           timestamp > 0 ~ "Scene02"), conc_words=number_of_words) %>%
  select("subtitle", "p", "m", "number_of_words", "timestamp", "scene", "conc_words")
shrek <- cbind(shrek, "Shrek 1")
colnames(shrek)[8] <- "movie"

shrek2 <- na.omit(srt_shrek2) %>%
  mutate(p=str_extract_all(subtitle, "\\b[A-Z]+[A-Z]")) %>%
  mutate(m=str_extract_all(subtitle, "[:space:]+&+[:space:]")) %>%
  mutate(number_of_words=(stri_count_words(subtitle)
                          - stri_count_regex(subtitle, '<i>') - stri_count_regex(subtitle, '</i>')
                          - ifelse(p=="character(0)", 0, ifelse(stri_count_words(p)==1, 1, stri_count_words(p)-1))
                          - ifelse(m=="character(0)", 0, ifelse(stri_count_words(m)==1, 1, stri_count_words(m)-1)))) %>%
  mutate(timestamp = round((end - start) / 2 + start, 4)) %>%
  filter(!p %in% c("character(0)", "SOUND FX"), number_of_words!=0) %>%
  mutate(scene = case_when(timestamp <= 151 ~ "Scene01",
                           151 < timestamp & timestamp <= 295 ~ "Scene02",
                           295 < timestamp & timestamp <= 521 ~ "Scene03",
                           521 < timestamp & timestamp <= 562 ~ "Scene04",
                           562 < timestamp & timestamp <= 670 ~ "Scene05",
                           670 < timestamp & timestamp <= 894 ~ "Scene06",
                           894 < timestamp & timestamp <= 1101 ~ "Scene07",
                           1101 < timestamp & timestamp <= 1410 ~ "Scene08",
                           1410 < timestamp & timestamp <= 1510 ~ "Scene09",
                           1510 < timestamp & timestamp <= 1622 ~ "Scene10",
                           1622 < timestamp & timestamp <= 1723 ~ "Scene11",
                           1723 < timestamp & timestamp <= 1760 ~ "Scene12",
                           1760 < timestamp & timestamp <= 1892 ~ "Scene14",
                           1892 < timestamp & timestamp <= 1955 ~ "Scene14",
                           1955 < timestamp & timestamp <= 2003 ~ "Scene15",
                           2003 < timestamp & timestamp <= 2240 ~ "Scene16",
                           2240 < timestamp & timestamp <= 2289 ~ "Scene17",
                           2289 < timestamp & timestamp <= 2340 ~ "Scene18",
                           2340 < timestamp & timestamp <= 2445 ~ "Scene19",
                           2445 < timestamp & timestamp <= 2498 ~ "Scene20",
                           2498 < timestamp & timestamp <= 2621 ~ "Scene21",
                           2621 < timestamp & timestamp <= 2830 ~ "Scene22",
                           2830 < timestamp & timestamp <= 2885 ~ "Scene23",
                           2885 < timestamp & timestamp <= 3100 ~ "Scene24",
                           3100 < timestamp & timestamp <= 3140 ~ "Scene25",
                           3140 < timestamp & timestamp <= 3289 ~ "Scene26",
                           3289 < timestamp & timestamp <= 3380 ~ "Scene27",
                           3380 < timestamp & timestamp <= 3465 ~ "Scene28",
                           3465 < timestamp & timestamp <= 3499 ~ "Scene29",
                           3499 < timestamp & timestamp <= 3511 ~ "Scene30",
                           3511 < timestamp & timestamp <= 3550 ~ "Scene31",
                           3550 < timestamp & timestamp <= 3616 ~ "Scene32",
                           3616 < timestamp & timestamp <= 3625 ~ "Scene33",
                           3625 < timestamp & timestamp <= 3723 ~ "Scene34",
                           3723 < timestamp & timestamp <= 3815 ~ "Scene35",
                           3815 < timestamp & timestamp <= 3876 ~ "Scene36",
                           3876 < timestamp & timestamp <= 3948 ~ "Scene37",
                           3948 < timestamp & timestamp <= 4041 ~ "Scene38",
                           4041 < timestamp & timestamp <= 4184 ~ "Scene39",
                           4184 < timestamp & timestamp <= 4193 ~ "Scene40",
                           4193 < timestamp & timestamp <= 4249 ~ "Scene41",
                           4249 < timestamp & timestamp <= 4340 ~ "Scene42",
                           4340 < timestamp & timestamp <= 4512 ~ "Scene43",
                           4512 < timestamp ~ "Scene44",), conc_words=number_of_words) %>%
  select("subtitle", "p", "m", "number_of_words", "timestamp", "scene", "conc_words")
shrek2 <- cbind(shrek2, "Shrek 2")
colnames(shrek2)[8] <- "movie"

shrek3 <- na.omit(csv_shrek3) %>%
  mutate(p=str_extract_all(Text, "\\b[A-Z]+[A-Z]")) %>%
  mutate(m=str_extract_all(Text, "[:space:]+&+[:space:]")) %>%
  mutate(number_of_words=(stri_count_words(Text)
                          - ifelse(p=="character(0)", 0, ifelse(stri_count_words(p)==1, 1, stri_count_words(p)-1))
                          - ifelse(m=="character(0)", 0, ifelse(stri_count_words(m)==1, 1, stri_count_words(m)-1)))) %>%
  mutate(timestamp = round((End - Start) / 2 + Start, 4)) %>%
  mutate(scene = case_when(timestamp <= 197.1230 ~ "Scene01",
                           197.1230 < timestamp & timestamp <= 449.8940 ~ "Scene02",
                           449.8940 < timestamp & timestamp <= 562.8875 ~ "Scene03",
                           562.8875 < timestamp & timestamp <= 744.8190 ~ "Scene04",
                           744.8190 < timestamp & timestamp <= 1048.6905 ~ "Scene05",
                           1048.6905 < timestamp & timestamp <= 1170.5110 ~ "Scene06",
                           1170.5110 < timestamp & timestamp <= 1232.0720 ~ "Scene07",
                           1232.0720 < timestamp & timestamp <= 1429.8225 ~ "Scene08",
                           1429.8225 < timestamp & timestamp <= 1493.7660 ~ "Scene8",
                           timestamp > 1493.7660 ~ "Scene10",), conc_words=number_of_words) %>%
  select("Text", "p", "m", "number_of_words", "timestamp", "scene", "conc_words")
shrek3 <- cbind(shrek3, "Shrek 3")
colnames(shrek3)[c(1,8)] <- c("subtitle", "movie")

shrek_total <- rbind(shrek, shrek2)
shrek_total <- rbind(shrek_total, shrek3)

# Listy z imionami 2 i 3- elementowymi.
l <- list(c("PRINCE","CHARMING"), c("FAIRY","GODMOTHER"), c("GINGERBREAD","MAN"), c("OLD","LADY"), c("KING","HAROLD"), c("CAPTAIN","HOOK"), c("MUFFIN","MAN"),
          c("EVIL","QUEEN"), c("PUPPET","MASTER"), c("BLIND","MOUSE"), c("SHIP","CAPTAIN"), c("SNOW","WHITE"), c("OLD","WOMAN"), c("LITTLE", "PIG"), c("ROBIN", "HOOD"),
          c("SLEEPING","BEAUTY"), c("EVIL","DWARF"), c("EVIL","TREE"), c("HEADLESS","HORSEMAN"), c("EVIL","KNIGHT"))
l_t <- list(c("MASTER","OF","CEREMONIES"), c("BIG", "BAD", "WOLF"))

# Sklejanie wypowiedzi na zasadzie: imię w poprzedniej linijce == w tej.
# Nie brane są pod uwagę linijki z wypowiedziami >1 postaci.
previous <- shrek_total$number_of_words[[1]]
for (i in 2:length(shrek_total$p)){
  if (shrek_total$scene[[i-1]]==shrek_total$scene[[i]] &&
      shrek_total$movie[[i-1]]==shrek_total$movie[[i]] &&
      (first(shrek_total$p[[i-1]])==first(shrek_total$p[[i]]) && length(shrek_total$p[[i-1]])==1 && length(shrek_total$p[[i]])==1)){
    shrek_total$conc_words[[i]] <- previous+shrek_total$number_of_words[[i]]
    previous <- shrek_total$conc_words[[i]]
    shrek_total$conc_words[[i-1]] <- 0
  } else {
    w <- 0
    for (k in 1:length(l)){
      if (shrek_total$scene[[i-1]]==shrek_total$scene[[i]] &&
          shrek_total$movie[[i-1]]==shrek_total$movie[[i]] &&
          length(unlist(shrek_total$p[[i-1]]))==2 && length(unlist(shrek_total$p[[i]]))==2 &&
          l[[k]][1]==shrek_total$p[[i-1]][1] && l[[k]][1]==shrek_total$p[[i]][1] &&
          l[[k]][2]==shrek_total$p[[i-1]][2] && l[[k]][2]==shrek_total$p[[i]][2]){
        shrek_total$conc_words[[i]] <- previous+shrek_total$number_of_words[[i]]
        previous <- shrek_total$conc_words[[i]]
        shrek_total$conc_words[[i-1]] <- 0
        w <- 1
        break
      }
    }
    for (k in 1:length(l_t)){
      if (shrek_total$scene[[i-1]]==shrek_total$scene[[i]] &&
          shrek_total$movie[[i-1]]==shrek_total$movie[[i]] &&
          length(unlist(shrek_total$p[[i-1]]))==3 && length(unlist(shrek_total$p[[i]]))==3 &&
          l_t[[k]][1]==shrek_total$p[[i-1]][1] && l_t[[k]][1]==shrek_total$p[[i]][1] &&
          l_t[[k]][2]==shrek_total$p[[i-1]][2] && l_t[[k]][2]==shrek_total$p[[i]][2] &&
          l_t[[k]][3]==shrek_total$p[[i-1]][3] && l_t[[k]][3]==shrek_total$p[[i]][3]){
        shrek_total$conc_words[[i]] <- previous+shrek_total$number_of_words[[i]]
        previous <- shrek_total$conc_words[[i]]
        shrek_total$conc_words[[i-1]] <- 0
        w <- 1
        break
      }
    }
    if (w==0){previous <- shrek_total$number_of_words[[i]]}
  }
}

shrek_total <- shrek_total %>%
  filter(p!="MUSIC", conc_words>0) %>%
  select("conc_words", "movie")%>%
  filter(conc_words < quantile(conc_words, 0.98))

p <- ggplot(shrek_total, aes(x=movie, y=conc_words)) +
  geom_violin(fill="#435914", color="darkgreen") +
  geom_boxplot(width=0.3, fill="#bdbe21", color="black") +
  labs(x = "Movie",
       y = "Number of words in a speech",
       title = "Distribution of the number of words in a single speech")  +
  scale_y_continuous(limits = c(0,50)) +
  theme_minimal()+
  theme(panel.background = element_rect(fill = "transparent",
                                        colour = NA_character_),
        plot.background = element_rect(fill = 'transparent',
                                       colour = NA_character_),
        text = element_text(size = 15))

p
ggsave(
  plot = p,
  filename = "Bruno_bez_tla.png",
  bg = "transparent"
)

