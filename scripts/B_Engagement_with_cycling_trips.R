# Description: This script generates the graphs for anlyzing 
# participants answers about Engagement with cycling.
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 

# Setup
library(ggplot2)

table_participants = read.csv('data/Cyclist_Experiment.csv')
table_trips = read.csv('data/Cyclist_Trip.csv', sep = '\t')
trips_joined = merge(table_participants,table_trips)
trips_joined$campaign_day = as.Date(trips_joined$X_created_at) - as.Date(trips_joined$questionnaire1) +1
trips_joined$day_of_week_trip = weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)
x_label <- "Day of the campaign"
y_label <- "Number of trips"


# Basic line plot with points
ggplot(data=trips_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label)

svg(filename="graphs/B_Engagement_with_cycling_trips_graph1.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=trips_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label)
dev.off()

# Histogram with trips per day
ggplot(data=trips_joined, aes(campaign_day)) +
  geom_histogram() +
  xlim(0, 20)
  

# Histogram for the number of trips per day


ggplot(data=trips_joined, aes(campaign_day, colour = city)) +
  geom_freqpoly( bins = 30 ) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = "bottom")

svg(filename="graphs/B_Engagement_with_cycling_trips_cities_graph.svg", 
    width=6.5, height=3.5, pointsize=10)
ggplot(data=trips_joined, aes(campaign_day, colour = city)) +
  geom_freqpoly( bins = 30 ) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme(legend.position = "bottom")
dev.off()


# Münster, started on Tuesday
# Castelló, started on Tuesday
# Malta, started on Thursday

# Bar plot showing the amount of trips per day of the week
ggplot(data = trips_joined, aes(day_of_week_trip, fill = city)) +
  geom_bar(stat = "count", )

