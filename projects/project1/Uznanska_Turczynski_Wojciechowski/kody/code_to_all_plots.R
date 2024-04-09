library(dplyr)
library(ggplot2)
library(rjson)
library(jsonlite)
library(forcats)

skrypt_calosci <- read.csv('Game_of_Thrones_Script.csv')
odcinki_statystyki <- read.csv('odcinki.csv')
smierci_wszystkich <- read.csv('deaths.csv')
smierci_wszystkich <-  smierci_wszystkich[-1,]

colnames(smierci_wszystkich) <- c('Index','Victim_House','Death_Number','Episode','Killer','Killers_House','Location','Method','Name','Season')

views <- odcinki_statystyki %>% 
  select(Season, No..of.Episode..Season.,No..of.Episode..Overall., Original.Air.Date, U.S..Viewers..Millions., IMDb.Rating)

ggplot(views, aes(x = No..of.Episode..Overall., y = U.S..Viewers..Millions.)) + geom_point() 

smierci_wszystkich$Episode <- as.integer(smierci_wszystkich$Episode)
smierci_wszystkich$Season <- as.integer(smierci_wszystkich$Season)

smierci_episode <- smierci_wszystkich %>% 
  group_by(Season,Episode) %>% 
  summarize(n=n()) %>% 
  arrange(Season, Episode)
  
ocena_smierci <- merge(views, smierci_episode, by.x=c('Season', 'No..of.Episode..Season.'), by.y = c('Season','Episode'), all = TRUE)
ocena_smierci[c(3,18,27,56),"n"] <- 0

ocena_smierci_rating <- ocena_smierci %>% 
  filter(IMDb.Rating >= 6.5, n <= 800)

wykres1 <- ggplot(ocena_smierci, aes(x = IMDb.Rating, y = n)) + geom_point() 

ocena_smierci_rating_2 <- ocena_smierci_rating %>% 
  group_by(IMDb.Rating) %>% 
  summarize(srednia_liczba_smierci = mean(n))

ggplot(ocena_smierci_rating_2, aes(x = IMDb.Rating, y = srednia_liczba_smierci)) + geom_point() 




ocena_smierci_viewsy_rating <- ocena_smierci %>% 
  group_by(IMDb.Rating) %>% 
  summarize(srednia_ogladalnosc = mean(U.S..Viewers..Millions.))

ggplot(ocena_smierci_viewsy_rating, aes(x = IMDb.Rating, y = srednia_ogladalnosc)) + geom_point() 



ocena_smierci_viewsy <- ocena_smierci
ocena_smierci_viewsy[,'U.S..Viewers..Millions.'] <-  round(ocena_smierci_viewsy[,'U.S..Viewers..Millions.'],1)
ocena_smierci_viewsy <- ocena_smierci_viewsy%>%
  group_by(U.S..Viewers..Millions.) %>% 
  summarize(srednia_liczba_smierci = mean(n))

ggplot(ocena_smierci_viewsy, aes(y = srednia_liczba_smierci, x = U.S..Viewers..Millions.)) + geom_point()




book1 <- read.csv('book1.csv')
book2 <- read.csv('book2.csv')
book3 <- read.csv('book3.csv')
book4 <- read.csv('book4.csv')
book5 <- read.csv('book5.csv')

books <- rbind(book1,book2,book3,book4,book5)

books <- books%>% 
  group_by(Source, Target) %>% 
  summarize(suma_rozmow = sum(weight)) %>% 
  arrange(-suma_rozmow)

postacie <- read.csv('wszystkie_postacie.csv')

books_2 <- merge(books, postacie, by.x = 'Source', by.y = 'Id')
books_3 <- merge(books_2, postacie, by.x = 'Target', by.y = 'Id')

books_3 <- books_3[, c('Label.x', 'Label.y', 'suma_rozmow')]

books_3$is_dead_x <- books_3$Label.x %in% smierci_wszystkich$Name
books_3$is_dead_y <- books_3$Label.y %in% smierci_wszystkich$Name


conv <- books_3
conv$is_dead_x <- as.numeric(conv$is_dead_x)
conv$is_dead_y <- as.numeric(conv$is_dead_y)




source_death_sum <- conv%>%
  group_by(Label.x)%>%
  summarise(death_sum = sum(is_dead_y, na.rm = TRUE))


source_all_sum <- conv%>%
  group_by(Label.x)%>%
  summarise(all_sum = n())

source_total <- merge(source_death_sum, source_all_sum, by = 'Label.x')
source_total<- source_total %>%
  rename( Name = Label.x)

target_death_sum <- conv%>%
  group_by(Label.y)%>%
  summarise(death_sum = sum(is_dead_x, na.rm = TRUE))


target_all_sum <- conv%>%
  group_by(Label.y)%>%
  summarise(all_sum = n())

target_total <- merge(target_death_sum, target_all_sum, by = 'Label.y')
target_total<- target_total %>%
  rename( Name = Label.y)


final <- merge(target_total, source_total, by = 'Name')

final$all <- final$all_sum.x + final$all_sum.y
final$deaths <- final$death_sum.x + final$death_sum.y
final <- final[, c('Name','all','deaths')]
final$proc_death <- (final$deaths / final$all) * 100
final <- final%>%
  arrange(desc(proc_death))
  
final_1 <- final%>%
  arrange(desc(proc_death))%>%
  filter(proc_death != 0 & proc_death != 100)%>%  
  filter(all >= 25) 

final_1$Name <- factor(final_1$Name, levels = final_1[order(-final_1$proc_death),]$Name)

ggplot(final_1, aes(x = Name, y = proc_death)) +
  geom_bar(stat = "identity") +
  labs(x = 'Names', y = 'Deaths_percantage') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


final_2 <- final%>%
  arrange(desc(proc_death))%>%
  filter(proc_death != 0 & proc_death != 100)%>% 
  filter(all >= 55)

final_2$Name <- factor(final_2$Name, levels = final_2[order(final_2$proc_death),]$Name)

ggplot(final_2, aes(y = Name, x = proc_death, label = Name)) +
  geom_bar(stat = "identity") +
  labs(x = 'Deaths percentage', y = 'Name', title = 'Who is the most dangerous character in Westeros') +
  theme(axis.text.x = element_text(angle = 0, vjust = 1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


ggplot(final_2, aes(y = Name, x = proc_death, label = Name)) +
  geom_bar(stat = 'identity', color = '#570000', fill = '#570000') +
  labs(x = 'Percentage of people that died after speaking to the character', y = 'Name of character', title = 'Most dangerous characters in Westeros') + 
  theme(legend.position = "bottom",
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = 'black'),
        axis.text.x = element_text(color = 'white', angle = 0, vjust = 1),
        axis.text.y = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white'),
        legend.key = element_rect(fill = 'black'),
        axis.line.x = element_line(color = 'white'),
        axis.line.y = element_line(color = 'white')
  )
  


json_data <- fromJSON(
  "character_gender.json"
)
character_gender_male <- data.frame(json_data$male)
character_gender_male$gender <- "male"
colnames(character_gender_male) <- c('Name', 'gender')
character_gender_female <- data.frame(json_data$female)
character_gender_female$gender <- "female"
colnames(character_gender_female) <- c('Name', 'gender')
character_gender <- rbind(character_gender_male, character_gender_female)

dead_male <- smierci_wszystkich %>% 
  select("Name") %>% 
    inner_join(character_gender_male, by = "Name")
percent_male <- length(dead_male$Name) / length(character_gender_male$Name)

dead_female <- smierci_wszystkich %>% 
  select("Name") %>% 
  inner_join(character_gender_female, by = "Name")

percent_female <- length(dead_female$Name) / length(character_gender_female$Name)



ggplot(smierci_episode, aes(x = Episode, y = log(n))) +
  geom_col(fill = 'green') +
  labs(x = 'Episode',
       y = 'Count') +
  scale_x_continuous(breaks = seq(min(smierci_episode$Episode), max(smierci_episode$Episode), by = 1)) +
  facet_wrap(~as.character(Season), nrow = 1)


smierci_episode2 <- smierci_episode %>% 
  mutate(in_series = paste("S", Season, "E", Episode, sep = "")) %>% 
  mutate(Episode = as.numeric(Episode) + as.numeric(Season) * 100) %>% 
  mutate(in_series = forcats::fct_reorder(in_series, Episode)) %>% 
  mutate(Episode = Episode - as.numeric(Season) * 100)


smierci_episode2 %>% 
  ggplot(aes(x = in_series, y = log(n, base = 2)))+
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5))



ggplot(smierci_episode2 , aes(x = in_series, y = n, group = Season, color = factor(Season)))+
  geom_line(size = 1.5)+
  geom_point(size = 3) +
  labs(x = 'Episode', y = 'Number of deaths in log scale', color = 'Season') + 
  theme(legend.position = "right",
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = 'black'),
        axis.text.x = element_text(color = 'white', angle = 90, vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white'),
        legend.key = element_rect(fill = 'black'),
        axis.line.x = element_line(color = 'white'),
        panel.grid.major.x = element_blank()
  ) +
  scale_y_continuous(trans = 'log10') + 
  coord_fixed(5) + 
  scale_color_manual(values = c('#570000','#970202','#E00000','#006EAD','#011F48','#14458A','#195C66','#77ACA9'))

ggplot(smierci_episode, aes(x = factor(Season), y = n, group = Season, color = factor(Season))) +
  geom_violin(trim = FALSE) +
  labs(x = "Season", y = "Number of Deaths") + 
  scale_y_continuous(trans = 'log10') +
  theme(legend.position = "right",
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = 'black'),
        axis.text.x = element_text(color = 'white', vjust = 0.5, hjust = 0.5),
        axis.text.y = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white'),
        legend.key = element_rect(fill = 'black'),
        axis.line.x = element_line(color = 'white'),
        panel.grid.major.x = element_blank()
  ) +
  scale_color_manual(values = c('#570000','#970202','#E00000','#006EAD','#011F48','#14458A','#195C66','#77ACA9'))


colnames(views) <- c('Season', 'Episode','Episode_overall','Air Date','Viewers_Millions','IMDb rating')
head(views)

ggplot(views, aes(x = Episode, y = Viewers_Millions, color = factor(Season))) +
  geom_line(size = 1.5) +
  geom_point(size = 3) + 
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  scale_y_continuous(breaks = c(3,6,9,12,15)) +
  labs(x = 'Episode', y = 'Viewers in Millions', color = 'Season') + 
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = 'black'),
        panel.background = element_rect(fill = 'black'),
        legend.background = element_rect(fill = 'black'),
        axis.text.x = element_text(color = 'white'),
        axis.text.y = element_text(color = 'white'),
        legend.title = element_text(color = 'white'),
        legend.text = element_text(color = 'white'),
        axis.title.x = element_text(color = 'white'),
        axis.title.y = element_text(color = 'white'),
        legend.key = element_rect(fill = 'black'),
        axis.line.x = element_line(color = 'white'),
        axis.line.y = element_line(color = 'white')
        )

season_colors <- c('#570000', '#970202', '#E00000', '#006EAD', '#011F48', '#14458A', '#195C66', '#77ACA9')


ggplot(smierci_episode, aes(x = factor(Season), y = n, fill = factor(Season))) +
  geom_violin(trim = FALSE, fill = '#570000') + 
  labs(x = "Season", y = "Number of Deaths") + 
  scale_y_continuous(trans = 'log10') + 
  theme(
    legend.position = "none",  
    plot.background = element_rect(fill = 'black'),  
    panel.background = element_rect(fill = 'black'),  
    axis.text.x = element_text(color = 'white', vjust = 0.5, hjust = 0.5),
    axis.text.y = element_text(color = 'white'),
    axis.title.x = element_text(color = 'white'),
    axis.title.y = element_text(color = 'white'),
    axis.line = element_line(color = 'white'),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank()
  )

summary_morderstwa <-  smierci_wszystkich%>%
  group_by(Killer) %>%
  summarise(Total_Kills = n(),
            Most_Common_Method = names(which.max(table(Method))))%>%
  arrange(desc(Total_Kills))

head(skrypt_calosci)

words_df <- skrypt_calosci %>%
  unnest_tokens(word, Sentence) %>%
  anti_join(stop_words)

words_df$word <- tolower(words_df$word)
words_df$word <- removePunctuation(words_df$word)


word_count <- words_df %>%
  count(word, sort = TRUE)


top_words <- head(word_count, 40)



print(top_words)

summary_morderstwa <- summary_morderstwa %>% 
  filter( !(Killer %in% c("Wight", "Stark Soldier", "Bolton soldier", 
                          "Sons of the Harpy agent","Dothraki man",
                          "Baratheon of King's Landing soldier","Night's Watch brother",
                          "Frey soldier", "Lannister soldier", "Baratheon of Dragonstone soldier", "Unsullied Soldier"))) %>% 
  head(15)

head(summary_morderstwa)


data_subset <- head(summary_morderstwa, 10) %>%
  arrange(desc(Total_Kills))



ggplot(data_subset, aes(y = reorder(Killer, Total_Kills), x = Total_Kills ,label = Most_Common_Method)) +
  geom_bar(stat = "identity", fill = "#570000") +
  scale_x_log10() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(face = "bold", color = "lightgrey"),   
    axis.text.y = element_text(color = "lightgrey"),                  
    panel.grid = element_blank(),                                 
    panel.grid.major = element_blank(),                           
    panel.grid.minor = element_blank(),                           
    plot.background = element_rect(fill = "black", color = "black"),
    panel.background = element_rect(fill = "black", color = "black"),
    text = element_text(color = "lightgrey"),
    axis.line.x = element_line(color = 'white')) +
  labs(x = "Killer", y = "Total Kills in log scale")+
  geom_text(position = position_stack(vjust = 0.5), size = 5, color = "white",
            family="Times", fontface="italic")

