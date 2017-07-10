library(dplyr)
library(tidytext)

# TidyText no tiene por defecto un diccionario de Stop Words en Español -.-"
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
stopWords <- c(stopWords, c("san", "<NA>"))

commentsLlaryora <- readr::read_csv("./data/martinllaryoraoficial/comments.csv")
commentsBaldassi <- readr::read_csv("./data/hectorbaldassi/comments.csv")
commentsOliviero <- readr::read_csv("./data/liliolivero/comments.csv")

topN <- 15

topLlaryora <- commentsLlaryora %>% 
  select(message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  filter(!(is.na(palabra))) %>%
  mutate(palabra=replace(palabra, palabra == "martin", "martín")) %>%
  mutate(palabra=replace(palabra, palabra == "amen", "amén")) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

topBaldassi <- commentsBaldassi %>% 
  select(message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  mutate(palabra=replace(palabra, palabra == "hector", "héctor")) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

topOliviero <- commentsOliviero %>% 
  select(message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  filter(!(is.na(palabra))) %>%
  mutate(palabra=replace(palabra, palabra == "lili", "liliana")) %>%
  count(palabra, sort = TRUE) %>%
  head(topN)

rankingPalabrasComentarios <- data.frame(ranking = 1:topN, 
                                        Llaryora = topLlaryora$palabra,
                                        Baldassi = topBaldassi$palabra,
                                        Oliviero = topOliviero$palabra)
