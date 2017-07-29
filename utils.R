library(tidyverse)
library(stringr)

CargarPosts <- function(){
  data <- dir(".", pattern = "posts.csv", recursive = TRUE) %>%
    map(read_csv) %>%
    reduce(rbind)
  data
}

CargarComentarios <- function(){
  idCandidatos <- read_csv("./data/candidatos.csv")
  colnames(idCandidatos) <- c("from_id_candidato", "from_name_candidato")
  
  dataComments <- dir(".", pattern = "comments.csv", recursive = TRUE) %>%
    map(read_csv) %>%
    reduce(rbind) %>%
    mutate(from_id_candidato = str_split(post_id, "_", simplify = TRUE)[,1])
  
  dataComments <- merge(dataComments, idCandidatos, by.x = "from_id_candidato", by.y = "from_id_candidato")
  dataComments
}