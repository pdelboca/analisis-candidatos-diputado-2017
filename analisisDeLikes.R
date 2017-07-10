library(dplyr)
library(tidytext)
library(ggplot2)

likesLlaryora <- readr::read_csv("./data/martinllaryoraoficial/likes.csv")
likesBaldassi <- readr::read_csv("./data/hectorbaldassi/likes.csv")
likesOliviero <- readr::read_csv("./data/liliolivero/likes.csv")


x1 <- likesLlaryora %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         porcentaje = cumsum / nrow(likesLlaryora),
         fila = row_number() / length(unique(likesLlaryora$from_id)))

x2 <- likesBaldassi %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         porcentaje = cumsum / nrow(likesBaldassi),
         fila = row_number() / length(unique(likesBaldassi$from_id)))

x3 <- likesOliviero %>% 
  group_by(from_id) %>%
  count(from_id, sort = TRUE) %>%
  ungroup() %>%
  mutate(cumsum = cumsum(n),
         porcentaje = cumsum / nrow(likesOliviero),
         fila = row_number() / length(unique(likesOliviero$from_id)))

ggplot() + 
  geom_line(data = x1, aes(x=fila, y=porcentaje), color='steelblue', size=1.5) +
  geom_line(data = x2, aes(x=fila, y=porcentaje), color='green', size=1.5) +
  geom_line(data = x3, aes(x=fila, y=porcentaje), color='black', size=1.5) +
  geom_vline(mapping=aes(xintercept=.1), color="red", linetype=2) +
  geom_hline(mapping=aes(yintercept=.6), color="red", linetype=2) +
  labs(y = "Porcentaje de Likes", 
       x = "Porcentaje de Seguidores",
       title = "Likes vs Seguidores (para tres Candidatos)",
       subtitle = "El 10% de los seguidores en Facebook explica aproximadamente el 60% de los likes.") + 
  scale_y_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  scale_x_continuous(labels = scales::percent, 
                     breaks = seq(0,1,.1),
                     minor_breaks = FALSE) +
  theme_minimal()

length(unique(likesLlaryora$from_id))
length(unique(likesBaldassi$from_id))
length(unique(likesOliviero$from_id))
