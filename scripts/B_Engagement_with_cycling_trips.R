# Description: This script generates the graphs for anlyzing 
# participants answers about Engagement with cycling.
#
# Day of campaign is the difference in days between the first day of the 
# campaign at each city and the day of the record
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 

source("scripts/setup.R")

library(ggplot2)
require(lubridate)
library(dplyr)

# Defining the variables that define the duration of the trip, hour of the day when the trip starts
trips_joined$day_time <-  format ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trips_joined$day_time <- strptime(trips_joined$day_time, format = "%H:%M:%OS")

trips_joined$hour_day <- hour(trips_joined$day_time) + minute(trips_joined$day_time) / demo
trips_joined$trip_length <- difftime( strptime(trips_joined$trip_stop, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), units='mins') 
trips_joined$trip_start_experiment <- difftime ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$questionnaire1, format= "%Y-%m-%dT%H:%M:%OS"), units='days') 
trips_joined$trip_stop_experiment <- difftime( strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") , units='days')

# Trip length in minutes 
ggplot(trips_joined, aes(x=trip_length, color=group)) + 
  geom_density() +
  geom_rug(aes(x = trip_length, y = 0), position = position_jitter(height = 0)) +
  theme_bw() +
  theme(legend.position = "bottom")
  

trips_valid <- trips_joined[trips_joined$trip_length < 60 & trips_joined$trip_length >.5 & trips_joined$group != as.factor("none"),]
ggplot(trips_valid, aes(x=trip_length, color=group)) + 
  geom_histogram(aes(x = trip_length, y = ..density..),
                 binwidth = 1, fill = "white", color = "black") +
  geom_density(n=1028, bw=4.4) +
  theme_bw() +
  theme(legend.position = "bottom")

d_competition <- density(as.numeric(trips_valid[trips_valid$group == "Competition",]$trip_length), from = 0.5, to = 60, n=1028, bw=4.4)
plot (d_competition)
d_collaboration <- density(as.numeric(trips_valid[trips_valid$group == "Collaboration",]$trip_length), from = 0.5, to = 60, n=1028, bw=4.4)
plot (d_collaboration)

plot(d_competition$y/d_collaboration$y, type="l")
plot(d_competition$x/d_collaboration$x, type="l")
plot(d_competition$x/d_collaboration$x)
delta_x = d_competition$x - d_collaboration$x
range (trips_valid$trip_length)
range (trips_valid[trips_valid$group == "Competition",]$trip_length)
range (trips_valid[trips_valid$group == "Collaboration",]$trip_length)


# grouping
trips_valid$day_time <- as.numeric(trips_valid$day_time)
groups_ <- group_by(trips_valid, group, participant)
summarize(groups_, mean = mean(trip_length), n = n() )


trip_start = data.frame(trips_joined$city, trips_joined$device, trips_joined$group, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_start_experiment, trips_joined$trip_length)
names(trip_start) <- c("city", "device", "group", "trip_count", "day_time", "time", "trip_length")
trip_start$time_type <- "start"
trip_stop = data.frame(trips_joined$city, trips_joined$device, trips_joined$group, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_stop_experiment, trips_joined$trip_length)
names(trip_stop) <- c("city", "device", "group", "trip_count", "day_time", "time", "trip_length")
trip_stop$time_type <- "stop"
trip_times <- rbind(trip_start, trip_stop)

trips_joined$day_of_week_trip = weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)
x_label <- "Day of the campaign"
y_label <- "Number of trips"

# statistical test
shapiro.test(trips_joined$hour_day)
ks.test(trips_joined$hour_day)
qqnorm(trips_joined$hour_day)

wilcox.test(as.numeric(trips_joined[trips_joined$group == "Competition",]$trip_length),
            as.numeric(trips_joined[trips_joined$group == "Collaboration",]$trip_length))
mean(as.numeric(trips_joined[trips_joined$group == "Competition",]$trip_length))
mean(as.numeric(trips_joined[trips_joined$group == "Collaboration",]$trip_length))
mean(as.numeric(trips_joined$trip_length))



# Basic line plot with points
ggplot(data=trips_joined, aes(trip_length)) +
  geom_freqpoly() +
  xlim(1, 120) + xlab(x_label) + ylab(y_label) +
  theme_bw() + facet_grid(group ~ .)

ggplot(data=trips_joined, aes(hour_day) ) +
  geom_freqpoly(bins = 24) +
  xlim(0, 23) + 
  theme_bw() + facet_grid(city ~ .)


trip_times$trip_lenght

p <- ggplot(trip_times, aes( time, hour(trip_times$day_time), color=group ))
p + geom_line()  + theme(legend.position = "false")
p + geom_point() + theme(legend.position = "bottom") + facet_grid(city ~ .)

p <- ggplot(trip_times[], aes( time, trip_length, color=group ))
p + geom_point() 

hist(as.numeric(trip_times$trip_length),breaks = 300)

myfdf <- data.frame(trips_joined$trip_length, trips_joined$trip_start, trips_joined$trip_stop)
names(myfdf) <- c
myfdf <- myfdf[]



head(trip_times$day_time)
hour(trip_times$day_time)
