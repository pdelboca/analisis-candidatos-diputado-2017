# TODO: Hay una discrepancia del 1% entre los likes que traen el post y la lista de Likes individuales de getPosts. Ejemplo: "148528951956339_917191005090126"
library(Rfacebook)

# Token temporal que caduca cada 1 hora. TODO: Automatizar token
fbToken <- "EAACEdEose0cBAOyFoWvxuCnFPxAT3FjxX7FJrALiIFTAV3zN77ZBqhLdxQgmNLKKZBw7q8rAa8dieuKJhaLZA57sMBgT6ZAjJqe6A9ZCARqXHNLaDle9HLQ4zhYB2PQnTQDoLrQDeFk7YItelmGkcYpDAXnnAlKZCsGQlZAZB8Err8HWopFdlDEN3Ph5xHGXa3EZD"

# Funcion para extraer comments, likes y posts de la lista que traer getPost.
ExtraerElementos <- function(index, listaPosts, elemento){
  tmpData <- as.data.frame(listaPosts[index][[1]][elemento])
  colnames(tmpData) <- str_split_fixed(colnames(tmpData),"\\.",2)[,2]
  
  if(nrow(tmpData) > 0){
    # Trazabilidad al post_id en post, comment, likes
    tmpData$post_id <- listaPosts[index][[1]]$post$id  
  }
  tmpData
}

# Funcion para Extraer Datos de Facebook
ExtraerDatosFacebook <- function(candidato, fbToken){
  print(sprintf("Extrayendo datos candidato: %s", candidato))
  paginaCandidato <- getPage(candidato, token = fbToken, n=5000, since = '2015/06/01')
  
  print("Ejecutando getPosts")
  paginaCandidato$id %>% 
    map(~ getPost(., token=fbToken)) -> tmpPosts
  
  print("Extrayendo Posts")
  seq_along(tmpPosts) %>%
    map_df(~ ExtraerElementos(.,tmpPosts, "post")) -> posts
  
  print("Extrayendo Comments")
  seq_along(tmpPosts) %>%
    map_df(~ ExtraerElementos(.,tmpPosts, "comments")) -> comments
  
  print("Extrayendo Likes")
  seq_along(tmpPosts) %>%
    map_df(~ ExtraerElementos(.,tmpPosts, "likes")) -> likes
  
  # Escribir a csv
  # TODO: Crear carpetas con el script
  print(sprintf("Escribiendo datos a disco del candidato %s", candidato))
  dir.create("./data")
  dir.create(paste0("./data/",candidato))
  
  readr::write_csv(comments, paste0("./data/",candidato,"/comments.csv"))
  readr::write_csv(likes, paste0("./data/",candidato,"/likes.csv"))
  readr::write_csv(posts, paste0("./data/",candidato,"/posts.csv"))
}

ExtraerDatosFacebook("hectorbaldassi", fbToken)
ExtraerDatosFacebook("martinllaryoraoficial", fbToken)
ExtraerDatosFacebook("liliolivero", fbToken)
ExtraerDatosFacebook("pablocarrook", fbToken)
ExtraerDatosFacebook("eduardofernandez2017", fbToken)
ExtraerDatosFacebook("253898394637965", fbToken) # Luciana Echevarria
ExtraerDatosFacebook("dantevrossi", fbToken)