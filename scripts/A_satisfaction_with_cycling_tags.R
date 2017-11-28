# Description: This script generates the graphs for anlyzing 
# participants answers about Satisfaction with cycling.
#
# Day of campaign is the difference in days between the first day of the 
# campaign at each city and the day of the record
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup 
library(ggplot2)

table_participants <- read.csv('data/Cyclist_Experiment.csv')
table_tags <- read.csv('data/Cyclist_Tag.csv', sep = '\t')
table_tags_polarity <- read.csv('data/Tags_polarity.csv')
table_tags_sense_joined <- merge(table_tags, table_tags_polarity, all = TRUE)
tags_joined <- merge(table_tags_sense_joined,table_participants)
tags_joined$campaign_day <- as.Date(tags_joined$X_created_at) - as.Date(tags_joined$questionnaire1) +1
trips_joined$day_of_week_trip <- weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)

x_label <- "Day of the campaign"
y_label <- "Number of tags per day"


# Basic plot line with the number of tags recorded per day of the campaign
ggplot(data=tags_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label)

svg(filename="graphs/B_Satisfaction_with_cycling_tags_graph1.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=tags_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label)
dev.off()


# Histogram with tags per day and the sense of its text
legend_title <- "Sentiment Polarity"
ggplot(data=tags_joined, aes(campaign_day, fill = sentiment_polarity)) +
  geom_area(stat = "bin") +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = 'bottom') + 
  labs(fill = legend_title)

svg(filename="graphs/B_Satisfaction_with_cycling_tags_graph2.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=tags_joined, aes(campaign_day, fill = sentiment_polarity)) +
  geom_area(stat = "bin") +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = 'bottom') + 
  labs(fill = legend_title)
dev.off()


# Histogram for the number of trips per day

ggplot(data=tags_joined, aes(campaign_day, colour = city)) +
  geom_freqpoly( bins = 10 ) +
  xlim(1, 10) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = "none")

svg(filename="graphs/B_Satisfaction_with_cycling_tags_cities_graph1.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=trips_joined, aes(campaign_day, colour = city)) +
  geom_freqpoly( bins = 30 ) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = "bottom")
dev.off()
