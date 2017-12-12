# Description: This script generates the graphs for anlyzing 
# participants answers about Engagement with cycling.
#
# Day of campaign is the difference in days between the first day of the 
# campaign at each city and the day of the record
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
ggplot(data=trips_joined[trips_joined$city == "Malta",], aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme_bw()

svg(filename="graphs/B_Engagement_with_cycling_trips_graph1.svg", 
    width=9, height=4, pointsize=10)
ggplot(data=trips_joined, aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme_bw()
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
  geom_bar(stat = "count" )



# Graph showing the length of the trip 
trips_joined$trip_stop
strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS")

as.POSIXct((as.character(trips_joined$trip_stop)),
           format = "%Y-%m-%dT%H:%M:%OS",
           tz = "Z")

format(as.POSIXlt(trips_joined$trip_stop),format = '%T')
time_txt <- sub('.*T(.*)Z', '\\1', trips_joined$trip_stop)
strptime(time_txt, format = "%H:%M:%OS")

trip_lengt <- unclass(as.POSIXlt(trips_joined$trip_stop)) - as.POSIXct(trips_joined$trip_start)

ggplot( trip_lengt, aes(nrow(trip_lengt) )) +
  geom_bar()

df <- data.frame(x1 = 2.62, x2 = 3.57, y1 = 21.0, y2 = 15.0)
ggplot() +
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)

