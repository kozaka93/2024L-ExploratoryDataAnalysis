how_many <- function(x){
  # zlicza ile słów jest w danym fragmencie tekstu
  stri_replace_all_regex(x, "[^[:alnum:][:space:]']", " ") %>% 
    stri_split_regex("\\s+") %>% 
    lapply(length) %>% 
    unlist()
}

how_many_bad <- function(x, swears){
  # zlicza ile przekleństw jest w danym fragmencie tekstu
  profanity(get_sentences(x), profanity_list = swears) %>%
    group_by(element_id) %>% 
    summarise(count = sum(profanity_count)) %>% 
    select(count)
}

capwords <- function(s, strict = FALSE) {
  # Zmienia pierwsze litery słów na wielkie
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

GOT_n <- function(n){
  # wyciąga n najczęstszych słów nie licząc najczęstszych angielskich słów
  # np. me, you itd.
  stopwords <- paste("^", c(stopwords("en"), stopwords("SMART")), "$", sep = "")
  
  data %>% 
    summarise(Words = paste(Sentence, collapse = " ")) %>% 
    tolower() %>% 
    stri_replace_all_regex("[^[:alnum:][:space:]']", " ") %>% 
    stri_split(regex = "\\s+") -> lista 
    
    lista[[1]] %>% 
      stri_replace_all_regex(stopwords, "", vectorize_all = FALSE) %>% 
      stri_replace_all_fixed(" ", "") %>% 
      stri_remove_empty() %>% 
      table() %>% 
      as.data.frame() %>% 
      arrange(-Freq) %>% 
      head(n)
}


how_long <- function(x){
  # podaje jakiej długości są słowa w danym fragmencie tekstu
  stri_replace_all_regex(x,"[:punct:]", "") %>%
    stri_split(regex = "\\s+") %>%
    first() %>% 
    stri_length()
} 




