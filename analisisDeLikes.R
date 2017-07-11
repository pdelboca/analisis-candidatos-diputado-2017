library(dplyr)
library(ggplot2)
library(readr)

likesLlaryora <- read_csv("./data/martinllaryoraoficial/likes.csv")
likesBaldassi <- readr::read_csv("./data/hectorbaldassi/likes.csv")
likesOliviero <- readr::read_csv("./data/liliolivero/likes.csv")


x1 <- likesLlaryora %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         proporcion_likes = cumsum / nrow(likesLlaryora),
         proporcion_usuarios = row_number() / length(unique(likesLlaryora$from_id)))

x2 <- likesBaldassi %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         proporcion_likes = cumsum / nrow(likesBaldassi),
         proporcion_usuarios = row_number() / length(unique(likesBaldassi$from_id)))

x3 <- likesOliviero %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         proporcion_likes = cumsum / nrow(likesOliviero),
         proporcion_usuarios = row_number() / length(unique(likesOliviero$from_id)))

ggplot() + 
  geom_line(data = x1, aes(x=proporcion_usuarios, y=proporcion_likes), color='steelblue', size=1.5) +
  geom_line(data = x2, aes(x=proporcion_usuarios, y=proporcion_likes), color='green', size=1.5) +
  geom_line(data = x3, aes(x=proporcion_usuarios, y=proporcion_likes), color='black', size=1.5) +
  geom_vline(mapping=aes(xintercept=.1), color="red", linetype=2) +
  geom_hline(mapping=aes(yintercept=.6), color="red", linetype=2) +
  labs(y =  "Porcentaje de Likes", 
       x =  "Porcentaje de Seguidores",
       title = "Likes vs Seguidores (para tres Candidatos)",
       subtitle = "El 10% de los seguidores en Facebook explica aproximadamente el 60% de los likes.") + 
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  scale_x_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  theme_minimal()
