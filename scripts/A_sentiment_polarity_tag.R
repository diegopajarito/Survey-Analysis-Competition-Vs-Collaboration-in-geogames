# Description: This script generates the graphs for visualizing 
# the patterns of tags reported by participants, specially 
# regarding the polarity associated to 
#
# Day of campaign is the difference in days between the first day of the 
# campaign at each city and the day of the record
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 


# Setup
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)

table_participants = read.csv('data/Cyclist_Experiment.csv')
table_tags <- read.csv('data/Cyclist_Tag.csv', sep = '\t')
table_tags_polarity <- read.csv('data/Tags_polarity.csv')
tags_participants = merge(table_tags,table_participants)
tags_participants$campaign_day = as.Date(tags_participants$X_created_at) - as.Date(tags_participants$questionnaire1) +1
table_tags_polarity_joined <- merge(tags_participants[!is.na(tags_participants$campaign_day),], table_tags_polarity, all = TRUE)

table_tags_polarity_joined$participant[table_tags_polarity_joined$city == "Castelló" & !is.na(table_tags_polarity_joined$participant)] <- 
  table_tags_polarity_joined$participant[table_tags_polarity_joined$city == "Castelló"& !is.na(table_tags_polarity_joined$participant)] - 20
table_tags_polarity_joined$participant[table_tags_polarity_joined$city == "Malta" & !is.na(table_tags_polarity_joined$participant)] <- 
  table_tags_polarity_joined$participant[table_tags_polarity_joined$city == "Malta"& !is.na(table_tags_polarity_joined$participant)] - 40


new_tags <- table_tags_polarity_joined[is.na(table_tags_sense_joined$sentiment_polarity),]
polarised_tags <- table_tags_polarity_joined[!is.na(table_tags_sense_joined$sentiment_polarity),]


# Cloud 1 - All Tags
docs <- Corpus( VectorSource( as.String(table_tags_polarity_joined$text_en) ) )
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


# Cloud 2 - New Tags
docs <- Corpus( VectorSource( as.String(new_tags$text) ) )
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


# Cloud 3 - Just Polarised Tags
docs <- Corpus( VectorSource( as.String(polarised_tags$text) ) )
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


# Plot for showing the amount of tags that each participant had provided 
# during the experiment, 
ggplot(table_tags_polarity_joined[!is.na(table_tags_polarity_joined$city),], aes(participant, tag_coung )) +
  geom_point(aes(colour = factor(sentiment_polarity))) +
  theme_bw() + 
  facet_grid(city ~ .) +
  theme(legend.position="bottom")

ggplot(table_tags_polarity_joined[!is.na(table_tags_polarity_joined$city),], aes(participant )) +
  geom_bar(aes(fill=sentiment_polarity)) + 
  theme_bw() + 
  facet_grid(city ~ .) +
  theme(legend.position="bottom")

ggplot(table_tags_polarity_joined, aes(x=sentiment_polarity)) + geom_bar(stat="count") + geom_label(stat="count", aes(label=..count..,vjust=2 ))

