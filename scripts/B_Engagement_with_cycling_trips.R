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

# Location_record, a list of the gps points recorded by the application 
location_record <- data.frame(location_joined$participant, location_joined$device, location_joined$time_gps, location_joined$questionnaire1, location_joined$questionnaire_2,
                              location_joined$application_install, location_joined$longitude, location_joined$latitude, location_joined$altitude, location_joined$precision,
                              location_joined$city, location_joined$dem_gender, location_joined$dem_age, location_joined$group)
names(location_record) <- c("participant", "device", "time_gps", "questionnaire_1", "questionnaire_2", "application_install", "longitude", "latitude", "altitude", "precision",
                            "city", "gender", "age", "group")
location_count <- data.frame(table(location_record$participant, location_record$device, location_record$city, location_record$group, location_record$gender, location_record$age))
names(location_count) <- c("participant", "device", "city", "group", "gender", "age", "count")
location_count <- location_count[which(location_count$count>0),]
mean(location_count$count)

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

# Estimating the proportion of of valid trips per participant
trip_validation_values <- data.frame(trip_record$participant, trip_record$city, trip_record$group, as.integer(trip_record$trip_week_experiment), trip_record$validation, trip_record$feedback_function, trip_record$feedback_reminder)
names(trip_validation_values) <- c("participant", "city", "group", "week", "validation", "feedback_function", "feedback_reminder")
trip_validation_count <- trip_validation_values %>%
  group_by(participant, city, group) %>%
  summarise(total_trips = n(),
            trips_valid = sum(ifelse(validation == "Valid", 1, 0)),
            trips_valid_time = sum(ifelse(validation == "Valid Time", 1, 0)),
            trips_valid_distance = sum(ifelse(validation == "Valid Distance", 1, 0)),
            trips_no_valid = sum(ifelse(validation == "No Valid", 1, 0)))
sum(trip_validation_count$total_trips)
mean(trip_validation_count$total_trips)
trip_validation_count$proportion_valid = trip_validation_count$trips_no_valid / trip_validation_count$total_trips

p_trips_valid <- ggplot(trip_validation_count, aes(x=reorder(participant, total_trips), y=total_trips, fill=city)) 
p_trips_valid + geom_bar(stat = 'identity') + geom_point(aes(y=trips_no_valid)) +
  geom_hline(yintercept = mean(trip_validation_count$total_trips), color="blue") +
  theme(legend.position = "bottom", axis.text.x=element_blank())

p_box_valid <- ggplot(trip_validation_count, aes(x=city, y=trips_valid_any))
p_box_valid + geom_boxplot()



trip_validation_first_week <- trip_validation_values[which(trip_validation_values$week < 3),]
nrow(trip_validation_first_week[trip_validation_first_week$validation != "No Valid",])
trip_validation_count_first_week <- trip_validation_first_week %>%
  group_by(participant, city, group) %>%
  summarise(total_trips = n(),
            trips_valid = sum(ifelse(validation == "Valid", 1, 0)),
            trips_valid_time = sum(ifelse(validation == "Valid Time", 1, 0)),
            trips_valid_distance = sum(ifelse(validation == "Valid Distance", 1, 0)),
            trips_no_valid = sum(ifelse(validation == "No Valid", 1, 0)))
sum(trip_validation_count_first_week$total_trips)
mean(trip_validation_count_first_week$total_trips)
nrow(trip_validation_values[trip_validation_values$validation != "No Valid" & trip_validation_values$week < 3, ])
mean(trip_validation_count_first_week$total_trips - trip_validation_count_first_week$trips_no_valid)
trip_validation_count_first_week$proportion_valid = trip_validation_count_first_week$trips_no_valid / trip_validation_count_first_week$total_trips


p_trips_valid_first_week <- ggplot(trip_validation_count_first_week, aes(x=reorder(participant, total_trips), y=total_trips, fill=city)) 
p_trips_valid_first_week + geom_bar(stat = 'identity') + geom_point(aes(y=trips_no_valid)) +
  geom_hline(yintercept = mean(trip_validation_count_first_week$total_trips), color="blue") +
  theme(legend.position = "bottom", axis.text.x=element_blank())

p_box_valid_first_week <- ggplot(trip_validation_count_first_week, aes(x=city, y=trips_valid))
p_box_valid_first_week + geom_boxplot() 


# 1 Why not valid? all participans had failures / during all the period of the experiment but more during first day
# Mostly in MS and more problemátic, but distribuited during the campaign
p_trips_no_valid <- ggplot(trip_record[trip_record$validation == "No Valid",], aes(x=participant, fill=validation))
p_trips_no_valid + geom_bar(stat = "count") + theme(legend.position = "bottom")
# Percentage of validation

round(trip_record$trip_start_experiment, digits = 0) + 1
# 2 What happen with distance?
p_trips_valid <- ggplot(trip_record[trip_record$validation == "Valid" | trip_record$validation == "Valid Distance" ,], aes(x=, y=length_raw, fill=participant))
p_trips_valid + geom_point() + theme(legend.position = "bottom") + geom_smooth()

# 3 What happen with time?
p_trips <- ggplot(trip_record, aes(x=validation, fill=city))
p_trips + stat_count(stat = 'identity')
p_trips + geom_point() + theme(legend.position = "bottom") + geom_smooth()

# 4 all
p_hist_all_trips <- ggplot(trip_record, aes(x=city))
p_hist_all_trips + stat_count(width = 0.8) + geom_label(stat="count", aes(label=..count..),vjust=2) + 
  theme(legend.position = "bottom") + theme_bw() + facet_grid(. ~ feedback_function)
   

ceiling(trip_record$trip_day_experiment / 10)

ggplot(trip_record, aes(x=cite())) + geom_bar(stat = "mean") +  geom_text(stat='count',aes(label=..count..))
mean(trip_record[!is.na(trip_record$length_raw),]$length_raw) + facet_grid(trip_week_experiment ~ .)
mean(trip_record[trip_record$validation != "No Valid" & trip_record$validation != "Valid Distance",]$trip_length)

210/793
380/793
240/293

29/(9+44+91)
36/(13+42+59)

343 + 335 + 115
343/793
335/793
115/793

173 + 150 + 78
173 / (173 + 150 + 78)
150 / (173 + 150 + 78)
78 / (173 + 150 + 78)

111 + 71 + 33
111 / (111 + 71 + 33)
71 / (111 + 71 + 33)
31 / (111 + 71 + 33)

32 + 29 + 4

(111 + 71 + 33 + 173 + 150 + 78 + 65) / 793

204/793
34/793
347/793
208/793



values <- data.frame(value = c("a", "a", "a", "a", "a", 
                               "b", "b", "b", 
                               "c", "c", "c", "c"))
nr.of.appearances <- aggregate(x = values, 
                               by = list(unique.values = values$value), 
                               FUN = length)

ag_participants <- aggregate(x = trip_record, by = list(trip_record$participant), FUN = length)


tab %>%
  group_by(month, variable) %>%
  summarise(a_sum=sum(amount),
            a_mean=(mean(amount)))







p_location <- ggplot(location_count, aes(x=reorder(participant,count), y=count, fill=city))
p_location + geom_bar(stat = 'identity') + geom_hline(yintercept = mean(location_count$count)) + coord_flip()


trips_count <- data.frame(table(trip_record$participant, trip_record$device, trip_record$city, trip_record$group))
names(trips_count) <- c("participant", "device", "city", "group", "count")
trips_count <- trips_count[which(trips_count$count>0),]


p_trips <- ggplot(trip_record, aes(x=length_raw, y=avg_speed, color=city))
p_trips + geom_point() + theme(legend.position = "bottom") + geom_smooth()


p_trips <- ggplot(trip_record, aes(x=reorder(trip_c,count), y=count, fill=city))
p_trips + geom_bar(stat = 'identity') + geom_hline(yintercept = mean(trips_count$count)) + coord_flip()

p_location <- ggplot(location_count, aes(x=reorder(device,count), y=count, fill=city))
p_location + geom_bar(stat = 'identity') + geom_hline(yintercept = mean(location_count$count)) + coord_flip()


trip_count_joined <- merge(trips_count, location_count, by =c("participant", "device", "city", "group")) 
names(trip_count_joined) <- c("participant", "device", "city", "group", "trips", "gender", "age", "points")
trip_count_joined$points_per_trip <- trip_count_joined$points / trip_count_joined$trips

p_trip_ratio <- ggplot(trip_count_joined, aes(x=reorder(participant,points_per_trip), y=points_per_trip, fill=city))
p_trip_ratio + geom_bar(stat = 'identity') + coord_flip()

sum(location_count$count)
sum(trips_count$count)



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
