library(wordcloud)
library(tm)



# create a corpus of titles in bib_social 
corpus_social <- Corpus(VectorSource(bib_social$TI))
# make lowercase
corpus_social2 <- tm_map(corpus_social, content_transformer(tolower))
# remove punctuations
corpus_social3 <- tm_map(corpus_social2, removePunctuation)
# remove stopwords
corpus_social4 <- tm_map(corpus_social3, removeWords, stopwords())
# generate TF-IDF matrix
social_dtm <- DocumentTermMatrix(corpus_social4)
# inspect to TF-IDF
inspect(social_dtm)

# generate a frequency data frame
word_frequency <- sort(colSums(as.matrix(social_dtm)),
                       decreasing=TRUE)
df_frequency<- data.frame(word = names(word_frequency),
                          freq=word_frequency)


# simple wordcloud (All the words don't do this)
# wordcloud(df_frequency$word, df_frequency$freq)

# top 50 words
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=50, min.freq = 10)


# make it pretty 
# font and order
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=40, min.freq = 10,
          random.order=FALSE,
          family = "Arial", font = 3)

# color palatte

library(RColorBrewer)

word_pal <- brewer.pal(10,"Dark2")

wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=40, min.freq = 10,
          random.order=FALSE,
          colors=word_pal, font = 3)






