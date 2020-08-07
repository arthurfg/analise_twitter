### Arthur de Figueiredo Gusmão
## arthurgusmao@id.uff.br

## Instalando os pacotes
install.packages("rtweet")
install.packages("tidyverse")
install.packages("httpuv")
install.packages('tm')
install.packages("tidytext") 

## Carregando os pacotes
library(tidyverse)
library(rtweet)
library(httpuv)
library(tm)
library(tidytext)

## Testando com a hashtag #econtwitter

econ_twitter <- search_tweets("#econtwitter",
                              n=100,
                              include_rts = F)
head(econ_twitter)



## Base de dados

bolsonaro <- search_tweets("#OPovoElegeuBolsonaro",
                           n=18000,
                           include_rts = T,
                           type = "recent",
                           lang = "pt")

## Plotando a TS

ts_plot(bolsonaro, "mins", color='#08a0e9', size=1) +
  labs(x = NULL, y = NULL,
       title = "Frequência da hashtag #OPovoELegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=14, face="bold"))


## WordCloud

## Selecionando somente o Nome do usuário e o texto do tweet
df <- bolsonaro %>%
  select(screen_name, text)
df_corpus <- Corpus(VectorSource(df$text)) #Transformando em corpus


## Função que remove HTML 
removeHTML = function(text){
  text = gsub(pattern = '<.+\\">', '', text)
  text = gsub(pattern = '</.+>', '', text)
  return(text)
}


## Removendo HTML, Números, Pontuação, Stopwords e etc...
df_corpus = df_corpus %>%
  tm_map(content_transformer(removeHTML)) %>%
  tm_map(removeNumbers) %>% #Removendo Números
  tm_map(removePunctuation) %>% #Removendo Pontuação
  tm_map(stripWhitespace) %>% #Removendo espaço em branco
  tm_map(content_transformer(tolower)) %>% #Caixa baixa
  tm_map(removeWords, stopwords("portuguese")) %>% #Removendo stopwords
  tm_map(removeWords, stopwords("SMART"))


## Criando um Term Document Matrix
tdm = TermDocumentMatrix(df_corpus) %>%
  as.matrix()


## Somando as frequências e transformando em data frame
words = sort(rowSums(tdm), decreasing = TRUE) 
df = data.frame(word = names(words), freq = words)


## Instala e carrega o pacote WordCloud2
install.packages("wordcloud2")
library(wordcloud2)

## Escolhi essas cores para uma melhor visualização
cores = c("#fefefe", "#f4f2a8", "#030303")
cor_fundo = "#00ccff"

## WordCloud
wordcloud2(df,
           color = rep_len(cores, nrow(df)),
           backgroundColor = cor_fundo,
           fontFamily = "DM Sans",
           size = 2.3,
           minSize = 8,
           rotateRatio = 0)


## Top n palavras
df %>%
  top_n(n=30, freq) %>%
  ggplot(aes(x=reorder(word, -freq),y=freq)) +
  geom_col(fill="#08a0e9") +
  labs(x = NULL, y = NULL,
       title = "Top 30 palavras da hashtag #OPovoELegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=14, face="bold"))+
  coord_flip()


## Top n tweets por localização
bolsonaro %>% 
  filter(!is.na(place_full_name)) %>% 
  count(place_full_name, sort = TRUE) %>% 
  top_n(10)%>%
  ggplot(aes(x=reorder(place_full_name, -n),y=n))+
  geom_col(fill="#08a0e9") +
  labs(x = NULL, y = NULL,
       title = "Top 10 tweets da #OPovoElegeuBolsonaro por localização",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=14, face="bold"))+
  coord_flip()


## Outras Hashtags
a <- bolsonaro %>% 
  unnest_tokens(hashtag, text, "tweets", to_lower = FALSE) %>%
  filter(str_detect(hashtag, "^#"),
         hashtag != "#OPovoElegeuBolsonaro") %>%
  count(hashtag, sort = TRUE) %>%
  top_n(10)
a %>%
  filter(n<947)%>%
  ggplot(aes(x=reorder(hashtag, -n),y=n)) +
  geom_col(fill="#08a0e9") +
  labs(x = NULL, y = NULL,
       title = "Outras hashtags nos tweets da #OPovoElegeuBolsonaro",
       subtitle = "05 de agosto de 2020",
       caption = "Fonte: Twitter's REST API")+
  theme_minimal()+
  theme(plot.title = element_text(size=12,face="bold"))+
  coord_flip()
