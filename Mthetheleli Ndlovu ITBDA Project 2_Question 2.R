#QUESTION 2

library(dplyr)
library(purrr)
library(SnowballC)
library(wordcloud)
library(tm)
library(stringr)
library (ggplot2)

zip_file <- "C:/Users/Mthetheleli Ndlovu/OneDrive/Pictures/news.zip"
file_list <- unzip(zip_file, list = TRUE) %>%
  filter(!endsWith(Name, "/")) %>%
  pull(Name)

df <- tibble (
  file_name = basename(file_list),
  content = map_chr(file_list, ~ {
    con <- unz(zip_file, .x)
    on.exit(close(con))
    readLines(con,warn = FALSE) %>% paste(collapse = "\n")
    
  })
  
)

df_text <- paste(df, collapse = " ")
df_text
#creating a corpus
corpus <- Corpus(VectorSource(df_text))

# preprocessing
corpus <- tm_map(corpus, content_transformer(tolower)) 
corpus <- tm_map(corpus,removeNumbers)
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(function(df_text) gsub("\\\\n", " ", df_text)))
corpus <- tm_map(corpus, content_transformer(function(df_text) gsub('[""""]', "",df_text)))
corpus <- tm_map(corpus, removeWords, stopwords("en"))

#Inspecting cleaned adata
inspect(corpus)


#creating Document-Term Matrix
dtm <- DocumentTermMatrix(corpus)
dtm_matrix <- as.matrix(dtm)


word_freq <- sort(colSums(dtm_matrix), decreasing = TRUE)

df_word <- data.frame(
  word = names(word_freq),
  Frequency = word_freq
)

#first visualization
top_words <- head(df_word, 20)
ggplot(top_words, aes(x=reorder(word, Frequency),y= Frequency))+
  
  geom_bar(stat= "identity", fill = "cyan")+
  coord_flip()+
  labs(title = "Top 20 Words in the Content", x=words, y="Frequency")

#second visualization- wordcloud
wordcloud(words = df_word$word, freq = df_word$Frequency,
          max.words = 20, colors = brewer.pal(8, "Dark2"))








