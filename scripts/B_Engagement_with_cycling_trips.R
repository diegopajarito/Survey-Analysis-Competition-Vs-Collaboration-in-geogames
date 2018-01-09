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
require(lubridate)
library(dplyr)

source("scripts/setup.R")

# Descriptive anallysis of the number of location and trip records
location_record <- data.frame(location_joined$participant, location_joined$device, location_joined$time_gps, location_joined$questionnaire1, location_joined$questionnaire_2,
                              location_joined$application_install, location_joined$longitude, location_joined$latitude, location_joined$altitude, location_joined$precision,
                              location_joined$city, location_joined$dem_gender, location_joined$dem_age, location_joined$group)
names(location_record) <- c("participant", "device", "time_gps", "questionnaire_1", "questionnaire_2", "application_install", "longitude", "latitude", "altitude", "precision",
                            "city", "gender", "age", "group")
location_count <- count(location_record, by=(device))
mean(location_count$n)


p_location <- ggplot(location_count, aes(x=reorder(by,n), y=n))
p_location + geom_bar(stat = 'identity') + geom_hline(yintercept = mean(location_count$n)) + coord_flip()




 # Estimation of times and length in time 
trips_joined$day_time <-  format ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trips_joined$day_time <- strptime(trips_joined$day_time, format = "%H:%M:%OS")
trips_joined$hour_day <- hour(trips_joined$day_time) + minute(trips_joined$day_time) / 60.0
trips_joined$trip_length <- difftime( strptime(trips_joined$trip_stop, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), units='mins') 
trips_joined$trip_start_experiment <- difftime ( strptime(trips_joined$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trips_joined$questionnaire1, format= "%Y-%m-%dT%H:%M:%OS"), units='days') 
trips_joined$trip_stop_experiment <- difftime( strptime(as.character(trips_joined$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trips_joined$questionnaire1), format= "%Y-%m-%dT%H:%M:%OS") , units='days')

# Classifying trips 
trips_joined$validation <- "No Valid"
trips_joined[which(trips_joined$trip_length >= 0.5 & trips_joined$trip_length <= 300.0),]$validation <- "Valid Time"
trips_joined[which(!is.na(trips_joined$length_raw) & trips_joined$length_raw > 30),]$validation <- "Valid Distance"
trips_joined[which(!is.na(trips_joined$length_raw) & trips_joined$length_raw > 30 & trips_joined$trip_length >= 0.5 & trips_joined$trip_length <= 300.0),]$validation <- "Valid"

trips_valid <- data.frame(trips_joined$participant, trips_joined$city, trips_joined$device, trips_joined$group, trips_joined$trip_count, trips_joined$trip_start, 
                          trips_joined$trip_stop, trips_joined$trip_length, trips_joined$length_raw, trips_joined$length_sim, trips_joined$dif_length,
                          trips_joined$day_time, trips_joined$hour_day, trips_joined$trip_start_experiment, trips_joined$trip_stop_experiment, trips_joined$validation)
names(trips_valid) <- c("participant", "city", "device", "group", "trip_count", "trip_star", "trip_stop", "trip_length", "length_raw", "length_sim", "dif_length", "day_time", "hour_day", 
                        "trip_start_experiment", "trip_stop_experiment", "validation")
trips_valid <- trips_valid[trips_valid$validation != "No Valid",]

range(trips_valid[trips_valid$validation == "Valid",]$trip_length)

# Distribution of Valid trips
p_valid <- ggplot(trips_valid, aes (x=city, fill=validation))
p_valid + geom_histogram(stat = "count") + theme(legend.position = "bottom")

# Trip length in minutes
trips_time <- trips_valid[trips_valid$validation == "Valid Time" | trips_valid$validation == "Valid" & trips_valid$group != "none",]
p_length_t <- ggplot(trips_distance, aes(x=trip_length, color=group)) 
p_length_t + geom_density() + theme(legend.position = "bottom") 

p_length_t + geom_histogram(aes(x = trip_length, y = ..density..),
                 binwidth = 4.4, fill = "white", color = "black", xlim = c(0,60)) +
  geom_density(n=1028, bw=4.4) +
  theme_bw() +
  theme(legend.position = "bottom")

# Comparison of density of trip length in minutes between groups 
d_competition <- density(as.numeric(trips_valid[trips_valid$group == "Competition",]$trip_length), from = 0.5, to = 60, n=1028, bw=4.4)
plot (d_competition)
d_collaboration <- density(as.numeric(trips_valid[trips_valid$group == "Collaboration",]$trip_length), from = 0.5, to = 60, n=1028, bw=4.4)
plot (d_collaboration)

plot(d_competition$y/d_collaboration$y, type="l")
plot(d_competition$x/d_collaboration$x, type="l")
delta_x = d_competition$x - d_collaboration$x
range (trips_valid$trip_length)
range (trips_valid[trips_valid$group == "Competition",]$trip_length)
range (trips_valid[trips_valid$group == "Collaboration",]$trip_length)




# Trip length in meters
p_length_d <- ggplot(trips_valid[trips_valid$validation != "Valid Time",], aes(length_raw, color=group)) 
p_length_d + geom_histogram(binwidth = 500) + xlim(c(0,10000))

p_length_d + geom_density() +
  geom_rug(aes(x = trip_length, y = 0), position = position_jitter(height = 0)) +
  theme_bw() +
  theme(legend.position = "bottom")

p_length_d + geom_density() + theme(legend.position = "bottom")

p_length_d + geom_histogram(aes(x = length_raw, y = ..density..),
                            binwidth = 500, fill = "white", color = "black") +
  geom_density() +
  theme_bw() +
  theme(legend.position = "bottom")

# Comparison of density of trip length in minutes between groups 
range(as.numeric(trips_valid[trips_valid$group == "Competition" & trips_valid$validation != "Valid Time",]$length_raw))
d_competition <- density(as.numeric(trips_valid[trips_valid$group == "Competition" & trips_valid$validation != "Valid Time",]$length_raw))
plot (d_competition)
d_collaboration <- density(as.numeric(trips_valid[trips_valid$group == "Collaboration" & trips_valid$validation != "Valid Time",]$length_raw))
plot (d_collaboration)

plot(d_competition$y/d_collaboration$y, type="l")
plot(d_competition$x/d_collaboration$x, type="l")
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
