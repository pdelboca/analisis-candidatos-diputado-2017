library(dplyr)
library(tidytext)

# TidyText no tiene por defecto un diccionario de Stop Words en Español -.-"
stopWords <- scan("http://www.webmining.cl/wp-content/uploads/2011/03/stopwords.es.txt", character())
stopWords <- c(stopWords, c("san", "<NA>"))

postsLlaryora <- readr::read_csv("./data/martinllaryoraoficial/posts.csv")
postsBaldassi <- readr::read_csv("./data/hectorbaldassi/posts.csv")
postsOliviero <- readr::read_csv("./data/liliolivero//posts.csv")

topN <- 15

bind_rows(postsBaldassi, postsLlaryora, postsOliviero) %>%
  select(from_name, message) %>%
  unnest_tokens(palabra, message) %>%
  filter(!(palabra %in% stopWords)) %>%
  filter(!(is.na(palabra))) %>%
  group_by(from_name) %>%
  count(palabra) %>%
  arrange(from_name, desc(n)) %>%
  slice(1:topN) %>%
  mutate(ranking = row_number()) %>%
  select(from_name, palabra, ranking) %>%
  tidyr::spread(from_name, palabra) -> rankingPalabrasCandidatos
