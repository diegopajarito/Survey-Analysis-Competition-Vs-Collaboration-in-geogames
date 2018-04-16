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

table_tags <- read.csv('data/Cyclist_Tag.csv', sep = '\t')
table_tags_polarity <- read.csv('data/Tags_polarity.csv')
tags_polarity <- merge(table_tags, table_tags_polarity, by = "text", all.x = TRUE)
tags_polarity <- unique(tags_polarity)  # Remove duplicated rows due to tags equally spelled in different languages

table_location = read.csv('data/Cyclist_Location.csv', sep = '\t')
table_location$time_gps <-  strptime(table_location$time_gps, format= "%Y-%m-%dT%H:%M:%OS")
table_trips_length = read.csv('data/Cyclist_Trip_length.csv')

table_answers = read.csv('data/Questionnaire_Answers.csv')
table_answers$group <- "none"
table_answers[which(table_answers$competition_1 >= -3),]$group <- "Competition"
table_answers[which(table_answers$collaboration_1 >= -3),]$group <- "Collaboration"

trips_joined <- merge(table_trips, table_participants)
trips_joined <- merge(trips_joined, table_trips_length, all.x = TRUE)
trips_joined <- merge(trips_joined, table_answers)

tags_joined <- merge(tags_polarity,table_participants, by = "device", all.x = TRUE)
tags_joined <- tags_joined[which(!duplicated(tags_joined$X_created_at)),] # Remove duplicated rows due to the same device
tags_joined <- merge(tags_joined, table_answers[,c("participant", "group")], all.x = TRUE)
tags_joined$campaign_day = as.Date(tags_joined$X_created_at) - as.Date(tags_joined$questionnaire1) + 1

location_joined <- merge(table_location, table_participants, all.y = TRUE)
location_joined <- merge (location_joined, table_answers, all.x = TRUE)