#QUESTION 3

#libraries
library(rvest)
library(dplyr)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(tidytext)

web_url <- "https://quotes.toscrape.com/"

web_page <- read_html(web_url)



quotes <- web_page %>%
  html_nodes(".text") %>%
  html_text(trim= TRUE)

auth_names <- web_page %>%
  html_nodes(".author") %>%
  html_text(trim =TRUE)

quotes_dframe <- data.frame(
  Quotes = quotes,
  Author = auth_names,
  stringsAsFactors = FALSE
)


View(quotes_dframe)

#creating a corpus
corpus <- Corpus(VectorSource(quotes_dframe))

#textual data preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, content_transformer(function(quotes_dframe) gsub("\\\\n", " ", quotes_dframe)))
corpus <- tm_map(corpus, content_transformer(function(quotes_dframe) gsub('[""""]', "",quotes_dframe)))
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
inspect(corpus)

#creating data matrix
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)

word_freq <- sort(colSums(dtm_matrix), decreasing = TRUE)


df_word <- data.frame(
  word = names(word_freq),
  Frequency = word_freq
)

#summarization 1
head.matrix(word_freq, 50)

#summarization 2
tidy_dtm <- tidy(dtm)
tidf_words <- tidy_dtm%>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))
head(tidf_words)


#first visualization
top_words <- head(df_word, 20)
ggplot(top_words, aes(x=reorder(word, Frequency),y= Frequency))+
  
  geom_bar(stat= "identity", fill = "pink")+
  coord_flip()+
  labs(title = "Top 20 Words in the Collected Information", x=words, y="Frequency")

#second visualization
wordcloud(
  words= names(freq),
  freq = freq,
  min.freq = 1,
  max.words =100,
  random.order = FALSE,
  color = brewer.pal(8, "Dark2")
)  
  




