# Description: This script describes the tags recorded by participants
# it draws graphs showing the quantity and polarity of tags as well as the
# group that recorded them
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(plyr)
# Setup 
source("scripts/setup.R")

tags_experiment <- tags_joined[!is.na(tags_joined$participant),]
tags_experiment <- tags_experiment[which(!is.na(tags_experiment$group) & tags_experiment$group != 'none'),]
tags_experiment <- tags_experiment[which(!is.na(tags_experiment$sentiment_polarity)),]
x_title <- 'Day of the Campaign'
y_title <- 'Number of tags'
l_title <- 'Sentiment Polarity'
colorpalete <- colorRampPalette(c("red","gray","green"))
plot(rep(1,50),col=(colfunc(50)), pch=19,cex=2)


# Bar chart showing the number of tags per city
q_plot <- ggplot(tags_experiment, aes(campaign_day, fill=sentiment_polarity))
q_plot + geom_bar(stat = 'bin') + xlim(0,20) +
  scale_fill_manual(l_title, values = c("#f46d43", "#DCDCDC", "#66bd63")) +
  xlab(x_title) + ylab(y_title) + theme_bw() + theme(legend.position = 'bottom') +
  facet_grid(group ~ .)

# Tags recorded during the first two weeks by sentimen polarity and group
count(tags_experiment[which( tags_experiment$campaign_day <= 13),], c('sentiment_polarity') )
count(tags_experiment[which( tags_experiment$campaign_day <= 13),], c('sentiment_polarity', 'group') )

tags_city <- ggplot(tags_experiment, aes(city))
tags_city + geom_bar(stat = 'count') + geom_label(stat = 'count', aes(label=..count..), vjust=-.1)

# Table with the list of tags and its frequency
docs <- Corpus( VectorSource( as.String(tags_experiment$text_en) ) )
inspect(docs)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
# Tags frequency in a data frame
top_words <- data.frame(word = names(v),freq=v)
set.seed(1234)
wordcloud(words = top_words$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# Tags per experiment condition
tags_city <- ggplot(tags_experiment, aes(group))
tags_city + geom_bar(stat = 'count') + geom_label(stat = 'count', aes(label=..count..), vjust=-.1)



