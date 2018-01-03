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
table_trips_length = read.csv('data/Cyclist_Trip_length.csv')
table_answers = read.csv('data/Questionnaire_Answers.csv')
trips_joined = merge(table_participants,table_trips)

trips_joined$day_time <-  format ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trips_joined$trip_start <- difftime( strptime(as.character(trips_joined$trip_start), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") ) 
trips_joined$trip_stop <- difftime( strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") )




trip_start = data.frame(trips_joined$city, trips_joined$device, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_start)
names(trip_start) <- c("city", "device", "trip_count", "day_time", "time")
trip_start$time_type <- "start"
trip_stop = data.frame(trips_joined$city, trips_joined$device, trips_joined$trip_count, trips_joined$day_time, trips_joined$trip_stop)
names(trip_stop) <- c("city", "device", "trip_count", "day_time", "time")
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



# Graph showing the length of the trip 


trip_lengt <- unclass(as.POSIXlt(trips_joined$trip_stop)) - as.POSIXct(trips_joined$trip_start)

format(trips_joined$trip_start,format = '%T')



p <- ggplot(trip_times, aes( time, day_time, color=city ))
p + geom_point() + geom_line() + xlim(0, 10000)
p + geom_line() + xlim(0, 2500)
p + geom_point() + theme(legend.position = 'bottom')



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


# trip length distance
hist(as.numeric(trip_times$trip_length),breaks = 300)
hist(table_trips_length$length_raw, breaks = 200)
hist(table_trips_length$dif_length, breaks = 30)
plot(table_trips_length$length_raw, table_trips_length$dif_length)

pd <- ggplot(table_trips_length, aes(length_raw))
pd + geom_histogram()



myfdf <- data.frame(trips_joined$trip_length, trips_joined$trip_start, trips_joined$trip_stop)
names(myfdf) <- c
myfdf <- myfdf[]



head(trip_times$day_time)
hour(trip_times$day_time)
