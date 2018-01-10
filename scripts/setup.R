# Description: This script loads the datasets needed for the analysis 
# it sets variables based on data coming from the questionnaire and
# the mobile application
# It also creates the join those tables based on the participant id 
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 

# Setup

table_participants = read.csv('data/Cyclist_Experiment.csv')
table_trips = read.csv('data/Cyclist_Trip.csv', sep = '\t')
# Participant 57 - 77d5ef0502ed6625_1 after 17/11/2017
# Participant 56 - f10bdbbea0e46dd0_1 after 17/11/2017
table_location = read.csv('data/Cyclist_Location.csv', sep = '\t')
table_location$time_gps <-  strptime(table_location$time_gps, format= "%Y-%m-%dT%H:%M:%OS")
#table_location[table_location$device == "f10bdbbea0e46dd0" & difftime(table_location$time_gps, strptime("2017-11-17T00:00:00.000Z", format = "%Y-%m-%dT%H:%M:%OS")) > 0,]$device <- "f10bdbbea0e46dd0_1"
#table_location[table_location$device == "77d5ef0502ed6625" & difftime(table_location$time_gps, strptime("2017-11-17T00:00:00.000Z", format = "%Y-%m-%dT%H:%M:%OS")) > 0,]$device <- "77d5ef0502ed6625"

table_trips_length = read.csv('data/Cyclist_Trip_length.csv')
table_answers = read.csv('data/Questionnaire_Answers.csv')
table_answers$group <- "none"
table_answers[which(table_answers$competition_1 >= -3),]$group <- "Competition"
table_answers[which(table_answers$collaboration_1 >= -3),]$group <- "Collaboration"
trips_joined <- merge(table_trips, table_participants)
trips_joined <- merge(trips_joined, table_trips_length, all.x = TRUE)
trips_joined <- merge(trips_joined, table_answers)
location_joined <- merge(table_location, table_participants, all.y = TRUE)
location_joined <- merge (location_joined, table_answers, all.x = TRUE)

