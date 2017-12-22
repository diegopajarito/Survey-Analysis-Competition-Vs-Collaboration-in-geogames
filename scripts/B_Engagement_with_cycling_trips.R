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

trips_joined$day_time <-  format ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trips_joined$day_time <- strptime(trips_joined$day_time, format = "%H:%M:%OS")
trips_joined$hour_day <- hour(trips_joined$day_time) + minute(trips_joined$day_time) / 60.0
trips_joined <- trips_joined[trips_joined$trip_length < 180 & trips_joined$trip_length >.5,]
trips_joined$trip_length <- difftime( strptime(trips_joined$trip_stop, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), units='mins') 
trips_joined$trip_start_experiment <- difftime ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$questionnaire1, format= "%Y-%m-%dT%H:%M:%OS"), units='days') 
trips_joined$trip_stop_experiment <- difftime( strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") , units='days')

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
