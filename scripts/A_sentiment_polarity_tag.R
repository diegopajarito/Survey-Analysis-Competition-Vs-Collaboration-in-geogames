

# Setup
library(tm)
library(SnowballC)
library(wordcloud)


table_tags <- read.csv('data/Cyclist_Tag.csv', sep = '\t')
table_tags_polarity <- read.csv('data/Tags_polarity.csv')
table_tags_sense_joined <- merge(table_tags, table_tags_polarity, all = TRUE)

new_tags



docs <- Corpus( VectorSource( as.String(table_tags$text) ) )

inspect(docs)

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")


dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)


set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
