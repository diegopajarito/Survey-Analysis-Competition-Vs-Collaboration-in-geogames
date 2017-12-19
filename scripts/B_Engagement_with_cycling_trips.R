# Description: This script generates the graphs for anlyzing 
# participants answers about Engagement with cycling.
#
# Day of campaign is the difference in days between the first day of the 
# campaign at each city and the day of the record
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 

source("scripts/setup.R")

trips_joined$day_time <-  format ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trips_joined$day_time <- strptime(trips_joined$day_time, format = "%H:%M:%OS")
trips_joined$trip_lenght <- difftime( strptime(trips_joined$trip_stop, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), units='mins') 
trips_joined$trip_start_experiment <- difftime ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$questionnaire1, format= "%Y-%m-%dT%H:%M:%OS"), units='days') 
trips_joined$trip_stop_experiment <- difftime( strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") , units='days')

trip_start = data.frame(trips_joined$city, trips_joined$device, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_start_experiment, trips_joined$trip_lenght)
names(trip_start) <- c("city", "device", "trip_count", "day_time", "time", "trip_lenght")
trip_start$time_type <- "start"
trip_stop = data.frame(trips_joined$city, trips_joined$device, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_stop_experiment, trips_joined$trip_lenght)
names(trip_stop) <- c("city", "device", "trip_count", "day_time", "time", "trip_lenght")
trip_stop$time_type <- "stop"
trip_times <- rbind(trip_start, trip_stop)

trips_joined$campaign_day = as.Date(trips_joined$X_created_at) - as.Date(trips_joined$questionnaire1) +1
trips_joined$campaign_time = strptime(trips_joined$X_created_at, format= "%Y-%m-%dT%H:%M:%OS") - strptime(trips_joined$questionnaire1, format= "%Y-%m-%dT%H:%M:%OS")  
trips_joined$day_of_week_trip = weekdays(as.Date(trips_joined$trip_start), abbreviate = TRUE)
x_label <- "Day of the campaign"
y_label <- "Number of trips"


# Basic line plot with points
ggplot(data=trips_joined[trips_joined$city == "Malta",], aes(campaign_day)) +
  geom_freqpoly(bins = 30) +
  xlim(1, 30) + xlab(x_label) + ylab(y_label) +
  theme_bw()




p <- ggplot(trip_times, aes( time, hour(trip_times$day_time), color=trip_lenght ))
p + geom_line()  + theme(legend.position = "false")
p + geom_point() + theme(legend.position = "none") + facet_grid(city ~ .)




head(trip_times$day_time)
hour(trip_times$day_time)
