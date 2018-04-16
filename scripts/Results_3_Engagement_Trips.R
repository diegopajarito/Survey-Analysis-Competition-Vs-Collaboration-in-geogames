# Description: This script generates the graphs for trips 
# recorded during the experiment
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
library(plyr)

source("scripts/setup.R")


# Trip_record, a list with the union of the trips recorded with the app and the estimations of distance and number of points coming from the location_record
trip_record <- data.frame(trips_joined$participant, trips_joined$device, trips_joined$city, trips_joined$group, trips_joined$trip_count, trips_joined$point_count,
                          trips_joined$trip_start, trips_joined$trip_stop, trips_joined$questionnaire1, trips_joined$questionnaire_2, trips_joined$length_raw, 
                          trips_joined$length_sim, trips_joined$dif_length, trips_joined$dem_gender, trips_joined$dem_age,
                          trips_joined$gaming_mobile_time, trips_joined$gaming_app_cycling, trips_joined$feedback_1, trips_joined$feedback_4)
names(trip_record) <- c("participant", "device", "city", "group", "trip_count", "point_count", "trip_start", "trip_stop", "exper_start", "exper_end", "length_raw", 
                        "length_sim", "dif_length", "gender", "age", "mobile_time", "app_cycling", "feedback_reminder", "feedback_function")


# Estimation of some characteristics of the trips, hour of the day, time length in minutes, time referred to the start of the experiment
trip_record$day_time <-  format ( strptime(trip_record$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), "%H:%M:%OS")
trip_record$day_time <- strptime(trip_record$day_time, format = "%H:%M:%OS")
trip_record$hour_day <- hour(trip_record$day_time) + minute(trip_record$day_time) / 60.0
trip_record$trip_length <- difftime( strptime(trip_record$trip_stop, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trip_record$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), units='mins') 
trip_record$trip_start_experiment <- difftime ( strptime(trip_record$trip_start, format= "%Y-%m-%dT%H:%M:%OS"), strptime(trip_record$exper_start, format= "%Y-%m-%dT%H:%M:%OS"), units='days') 
trip_record$trip_stop_experiment <- difftime( strptime(as.character(trip_record$trip_stop), format= "%Y-%m-%dT%H:%M:%OS"), strptime(as.character(trip_record$exper_start), format= "%Y-%m-%dT%H:%M:%OS") , units='days')
trip_record$trip_day_experiment <- round(trip_record$trip_start_experiment, digits = 0) + 1
trip_record$trip_week_experiment <- ceiling(trip_record$trip_day_experiment / 7)

# Classifying trips according to their length in time and distance
trip_record$validation <- "No Valid"
trip_record[which(trip_record$trip_length >= 0.5 & trip_record$trip_length <= 300.0),]$validation <- "Valid Time"
trip_record[which(!is.na(trip_record$length_raw) & trip_record$length_raw > 30),]$validation <- "Valid Distance"
trip_record[which(!is.na(trip_record$length_raw) & trip_record$length_raw > 30 & trip_record$trip_length >= 0.5 & trip_record$trip_length <= 300.0),]$validation <- "Valid"
trip_record$avg_speed = trip_record$length_raw/(as.numeric(trip_record$trip_length) /60)



# Table 1: Trips and participants after the experiment
trip_after_experiment <- trip_record[which(difftime( strptime(as.character(trip_record$exper_end), format= "%Y-%m-%dT%H:%M:%OS"), 
                                                     trip_record$trip_start) < 0),
                                     c('participant', 'device', 'city', 'group', 'hour_day', 'trip_day_experiment', 'trip_week_experiment')]
trips_after <- trip_after_experiment %>% 
  group_by(city, participant) %>% 
  summarise(trips = n())

# Table 2: Participants after three weeks
trip_after_threew_w <- trip_record[which(trip_record$trip_week_experiment > 2),
                                  c('participant', 'device', 'city', 'group', 'hour_day', 'trip_day_experiment', 'trip_week_experiment')]
trips_after_three <- trip_after_threew_w %>% 
  group_by(city, participant) %>% 
  summarise(trips = n())

# Participants 
l_title <- 'Condition'
x_title <- 'Week'
y_title <- 'Number of Trips'
p_trips <- ggplot(trip_record[which(trip_record$group != 'none'),], aes(trip_week_experiment, fill = group))
p_trips + geom_bar(stat = 'count') + scale_fill_manual(l_title, values = c("#303030", "#A9A9A9", "#D3D3D3")) +
  ylab(y_title) + xlab(x_title) + facet_grid(city ~ .) + scale_x_reverse() + coord_flip() + theme_bw() + theme(legend.position = 'bottom')

# Table with trips by condition
trips_first_two_weeks <- trip_record[which(trip_record$group != 'none' & trip_record$trip_week_experiment <= 2),
                                     c('group', 'city', 'participant')]

trips_after <- trips_first_two_weeks %>% 
  group_by(group) %>% 
  summarise(trips = n())

# Table with trips by condition
trips_length <- trip_record[trip_record$validation == 'Valid' | trip_record$validation == 'Valid Distance',
                                     c('group', 'city', 'participant', 'length_sim', 'trip_length')]
mean_trips <- trips_length %>% 
  group_by(city) %>% 
  summarise(mean_length = mean(length_sim), mean_duration = mean(trip_length))

trips_duration <- trip_record[trip_record$validation == 'Valid' | trip_record$validation == 'Valid Time',
                             c('group', 'city', 'participant', 'length_sim', 'trip_length')]
mean_trips <- trips_duration %>% 
  group_by(city) %>% 
  summarise(mean_length = mean(length_sim), mean_duration = mean(trip_length))
