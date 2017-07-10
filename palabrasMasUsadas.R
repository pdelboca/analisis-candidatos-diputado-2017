library(dplyr)
library(tidytext)

# TidyText no tiene por defecto un diccionario de Stop Words en Español -.-"
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
stopWords <- c(stopWords, c("san", "<NA>"))

postsLlaryora <- readr::read_csv("./data/martinllaryoraoficial/posts.csv")
postsBaldassi <- readr::read_csv("./data/hectorbaldassi/posts.csv")
postsOliviero <- readr::read_csv("./data/liliolivero//posts.csv")

topN <- 15

topLlaryora <- postsLlaryora %>% 
  select(from_name, message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  filter(!(is.na(palabra))) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

topBaldassi <- postsBaldassi %>% 
  select(from_name, message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  filter(!(is.na(palabra))) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

topOliviero <- postsOliviero %>% 
  select(from_name, message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

rankingPalabrasCandidatos <- data.frame(ranking = 1:topN, 
           Llaryora = topLlaryora$palabra,
           Baldassi = topBaldassi$palabra,
           Oliviero = topOliviero$palabra)
