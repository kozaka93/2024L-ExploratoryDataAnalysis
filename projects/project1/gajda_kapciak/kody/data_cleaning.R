library(tidyverse)

# Kaggle datasets ----
encoding = "UTF-8"
house_episodes <- read_csv("data/house_episodes.csv", locale = locale(encoding = encoding))
house_imdb <- read_csv("data/house_imdb.csv", locale = locale(encoding = encoding))


# Processing ICD codes data ----

# Reading diagnoses data for each season ----
diagnoses_list = list()
for (i in 1:8) {
  file_name <- paste0("data/diagnoses", i, ".csv")
  diagnoses <- read_csv(file_name, locale = locale(encoding = encoding)) %>% 
    mutate(season = i)
  
  diagnoses_list[[i]] <- diagnoses
}

# Reading diseases and disease groups data ----
icd_codes = read.csv("data/icd_codes.csv", sep = ";") %>% 
  rename(name = field.0.__text) %>% 
  select(name, X_id)

# Renaming columns and processing disease groups data
new_names = c(category = "X_id[, 1]", X_id = "X_id[, 2]")
icd_categories = read.csv("data/icd_categories_raw.csv", sep = ",") %>% 
  select(X_id)
new_cols = str_split_fixed(icd_categories$X_id, "-", n=2)
icd_categories = icd_categories %>% 
  mutate(X_id = new_cols[,2], category = new_cols[,1])


# Joining diseases and disease groups data to obtain ICD codes
icd_codes_categories = left_join(icd_codes, icd_categories) %>% 
  filter(category != "NOTIFIABLE" ) %>% 
  rename(icd_code = X_id)


# Loading data frame containing disease codes, categories, names and organs affected ----
# (necessary for the anatogram), then tidying the data


organs = read_csv("data/organs.csv")


# FINAL DATAFRAME ----
# Merging diagnoses, episodes, and IMDb data to form a unified dataframe
todo <- do.call(rbind, diagnoses_list) %>% 
  right_join(house_episodes, by = join_by(Episode == title)) %>% 
  right_join(house_imdb, by = join_by(Episode == title)) %>% 
  select(-c(N, season.x, season.y, episode_num, original_air_date.x)) %>% 
  rename(original_air_date = original_air_date.y, 
         medical_diagnosis = `Medical diagnose(s)`,
         patients_name = `Patient(s)`,
         description = desc) %>% 
  left_join(icd_codes_categories, by = join_by(medical_diagnosis == name)) %>% 
  left_join(organs) %>% 
  relocate(c(season, episode_num_in_season, episode_num_overall)) %>% 
  relocate(c(icd_code, category, organs_affected), .after = medical_diagnosis) %>% 
  distinct()

# 
# write.csv(icd_categories, "data/icd_categories.csv")
write.csv(todo, "data/house_md_data.csv")

