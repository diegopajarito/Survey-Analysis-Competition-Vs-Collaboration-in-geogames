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
table_answers <- read.csv('data/Questionnaire_Answers.csv')
table_tags <- read.csv('data/Cyclist_Tag.csv', sep = '\t')
table_tags_polarity <- read.csv('data/Tags_polarity.csv')
participants_competition <- data.frame(table_answers[which(table_answers$competition_1 >= -3),]$Participant)
participants_competition$group <- "Competition"
names(participants_competition) <- c("participant", "group")
participants_collaboration <- data.frame(table_answers[which(table_answers$collaboration_1 >= -3),]$Participant)
participants_collaboration$group <- "Collaboration"
names(participants_collaboration) <- c("participant", "group")
participants_group <- rbind(participants_collaboration, participants_competition)
table_tags_sense_joined <- merge(table_tags, table_tags_polarity, all = TRUE)
tags_joined <- merge(table_tags_sense_joined,table_participants)
tags_joined$campaign_day <- as.Date(tags_joined$X_created_at) - as.Date(tags_joined$questionnaire1) +1
trips_joined$day_of_week_trip <- weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)
tags_group <- merge(tags_joined, participants_group)

x_label <- "Day of the campaign"
y_label <- "Number of tags per day"


# Basic plot line with the number of tags recorded per day of the campaign
ggplot(data=tags_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) + 
  theme_bw()

svg(filename="graphs/B_Satisfaction_with_cycling_tags_graph1.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=tags_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) + 
  theme_bw()
dev.off()


# Histogram with tags per day and the sense of its text
legend_title <- "Sentiment Polarity"
ggplot(data=tags_joined, aes(campaign_day, fill = sentiment_polarity)) +
  geom_area(stat = "bin") +
  theme_bw() +
  scale_fill_manual(values = c("Positive" = "#5ab4ac",
                               "Negative" = "#d8b365",
                               "Neutral " = "#c7eae5"),
                    na.value = "#bdbdbd") +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = 'bottom') + 
  labs(fill = legend_title)
  

svg(filename="graphs/B_Satisfaction_with_cycling_tags_graph2.svg", 
    width=9, height=4, pointsize=10)09< 
ggplot(data=tags_joined, aes(campaign_day, fill = sentiment_polarity)) +
  geom_area(stat = "bin") +
  theme_bw() +
  scale_fill_manual(values = c("Positive" = "#5ab4ac",
                               "Negative" = "#d8b365",
                               "Neutral " = "#c7eae5"),
                    na.value = "#bdbdbd") +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = 'bottom') + 
  labs(fill = legend_title) 
dev.off()

# New Tags or tags without any associated polarity
tags_joined[is.na(tags_joined$sentiment_polarity),2]

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





# Tags Group
tags_group[is.na(tags_group$sentiment_polarity),]$sentiment_polarity <- "Neutral "
aggregate(tags_group$tag_count, by = list(df$Gene), max)

p <-ggplot(data=tags_group, aes(x=campaign_day, fill=sentiment_polarity)) 
p + geom_bar(stat="count") + facet_grid(sentiment_polarity ~ .)

p <-ggplot(data=tags_group, aes(x=campaign_day) ) 
p + geom_bar(stat="count", width = 0.4, aes(fill = group), position = "dodge") + xlim(0,20) + facet_grid(sentiment_polarity ~ .) + theme_bw() + theme(legend.position = "bottom")

  
  
p_tags_line <- ggplot(data=tags_group[tags_group$participant == "15",], aes(x=campaign_day, y=tag_count, group=participant))
p_tags_line + geom_line() 


# plot everything
ggplot(data.m, aes(Names, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")

# Horizontal bar plot
p + coord_flip()









# aggregate
aggregate(df$Value, by = list(df$Gene), max)
aggregate(Value ~ Gene, data = df, max)

# tapply
tapply(df$Value, df$Gene, max)

# split + lapply
lapply(split(df, df$Gene), function(y) max(y$Value))

# plyr
require(plyr)
ddply(df, .(Gene), summarise, Value = max(Value))
