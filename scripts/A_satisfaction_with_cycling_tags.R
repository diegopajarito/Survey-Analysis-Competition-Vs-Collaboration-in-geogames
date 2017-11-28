# Description: This script generates the graphs for anlyzing 
# participants answers about Satisfaction with cycling.
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup 
library(ggplot2)

table_participants = read.csv('data/Cyclist_Experiment.csv')
table_tags = read.csv('data/Cyclist_Tag.csv', sep = '\t')
tags_joined = merge(table_participants,table_tags)
tags_joined$campaign_day = as.Date(tags_joined$X_created_at) - as.Date(tags_joined$questionnaire1) +1
trips_joined$day_of_week_trip = weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)

plot(tags_joined$campaign_day, tags_joined$tag_count)


# Basic plot line
ggplot(data=tags_joined, aes(x=campaign_day, y=tag_count, group=device)) +
  geom_line()


# Histogram with trips per day
ggplot(data=tags_joined, aes(campaign_day)) +
  geom_histogram() +
  xlim(0, 20)


# Histogram for the number of trips per day
x_label <- "Day of the campaign"
y_label <- "Number of tags"
ggplot(data=tags_joined, aes(campaign_day, colour = city)) +
  geom_freqpoly( bins = 10 ) +
  xlim(1, 10) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = "none")
