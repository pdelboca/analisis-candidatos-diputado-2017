library(dplyr)
library(ggplot2)
library(readr)

likesLlaryora <- read_csv("./data/martinllaryoraoficial/likes.csv")
likesBaldassi <- read_csv("./data/hectorbaldassi/likes.csv")
likesOliviero <- read_csv("./data/liliolivero/likes.csv")

likesLlaryora$candidato <- "Llaryora"
likesBaldassi$candidato <- "Baldassi"
likesOliviero$candidato <- "Oliviero"

datosLikes <- rbind(likesLlaryora, likesBaldassi, likesOliviero)

datosLikes %>%
  group_by(candidato, from_id) %>%
  count(candidato, from_id) %>%
  arrange(candidato, desc(n)) %>%
  mutate(cumsum = cumsum(n),
         proporcion_likes = cumsum / sum(n),
         proporcion_usuarios = row_number() / n()) -> cumLikes

ggplot(cumLikes, aes(x=proporcion_usuarios, y=proporcion_likes, color=candidato)) +
  geom_line(size=1.5) +
  geom_vline(mapping=aes(xintercept=.1), color="red", linetype=2) +
  geom_hline(mapping=aes(yintercept=.6), color="red", linetype=2) +
  labs(y =  "Porcentaje de Likes", 
       x =  "Porcentaje de Seguidores que dieron Like",
       title = "Likes vs Seguidores",
       subtitle = "Del total de usuarios que dieron like, el 10% de ellos explica 60% total de Likes.") + 
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  scale_x_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  theme_minimal()
