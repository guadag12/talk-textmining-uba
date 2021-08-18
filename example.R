library(politicxsentwitteR)
library(tidyverse)
data <- get_timeline_data_all(category = "senators")
data_aborto <- data %>% filter(created_at >= "2020-12-29" | created_at >= "2020-12-30")
saveRDS(data, "data_senators.rds")

saveRDS(data_aborto, "data_aborto.rds")


```{r}
library(quanteda)
data_aborto <- readRDS("data_aborto.rds")
data_aborto <- data_aborto %>% left_join(politicxs_data)
data_aborto <- data_aborto[,c("text", "gender")]
#Vemos como quedó el dataset que descargamos recientemente
View(timelines)

#Vamos a transformar ese dataset en un corpus que podamos trabajar
corpus_timelines <- corpus(timelines)
head(corpus_timelines)

#Ahora vamos a transformalo en una matriz de palabras y tweets
dfm_timelines <- dfm(corpus_timelines, remove_punct = TRUE, remove = stopwords("spa"), 
                     groups = "screen_name")
head(dfm_timelines)

#Nube de palabras
textplot_wordcloud(dfm_timelines, rotation = 0.25,
                   color = rev(RColorBrewer::brewer.pal(10, "RdBu")))

#Nube de palabras comparación
textplot_wordcloud(dfm_timelines, comparison = TRUE, max_words = 300,
                   color = c("blue", "red", "green"))

#Top Users
user_timelines <- dfm_select(dfm_timelines, pattern = "@*")
top_user_timelines <- names(topfeatures(user_timelines, 50))
View(top_user_timelines)

#Hacemos una matriz de palabras (palabras que aparecen juntas en el mismo texto/tweet)
user_timelines_fcm <- fcm(user_timelines)
head(user_fcm)

#Visualizamos las relaciones
user_fcm <- fcm_select(user_timelines_fcm, pattern = top_user_timelines)
textplot_network(user_fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)

#Top hashtags
user_fcm <- fcm_select(user_fcm, pattern = topuser)
textplot_network(user_fcm, min_freq = 0.1, edge_color = "orange", edge_alpha = 0.8, edge_size = 5)
```


```{r}
data_aborto <- readRDS("data_aborto.rds")
data_aborto <- data_aborto %>% left_join(politicxs_data)
data_aborto <- data_aborto[,c("text", "gender")]
stop_words_es <- read.table("https://github.com/Alir3z4/stop-words/raw/master/spanish.txt")
names(stop_words_es)[1] <- "word"
data_limpia <- data_aborto %>% 
  mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;"),
         text = str_remove_all(text, "\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)"),
         text = str_remove_all(text, "[^\x01-\x7F]"), 
         text = str_remove_all(text, "^#"), 
         text = str_remove_all(text, "@\\S+"), 
         text = str_remove_all(text, stop_words_es$word)
         
  ) %>%
  drop_na(gender) %>%
  unnest_tokens(input = "text", output = "bigrama", token = "ngrams", n = 2)
```
```{r}
data_bigrama <- 
  data_limpia %>% 
  separate(bigrama, into = c("uno", "dos"), sep = " ") %>% 
  filter(!uno %in% stopwords(kind = "es")) %>% 
  filter(!dos %in% stopwords(kind = "es")) %>% 
  count(uno, dos) %>%
  filter(!uno %in% stop_words_es$word,
         !uno %in% str_remove_all(stop_words_es$word, "'"),
         str_detect(uno, "[a-z]"),
         !str_detect(uno, "^#"),         
         !str_detect(uno, "@\\S+"), 
         !nchar(as.character(uno)) <= 3) %>%
  filter(!dos %in% stop_words_es$word,
         !dos %in% str_remove_all(stop_words_es$word, "'"),
         str_detect(dos, "[a-z]"),
         !str_detect(dos, "^#"),         
         !str_detect(dos, "@\\S+"), 
         !nchar(as.character(dos)) <= 3) 


data_bigrama %>% 
  filter(n >= 50) %>% 
  graph_from_data_frame() %>% 
  ggraph() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(.075, "inches"))) +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) + 
  theme_void()
```