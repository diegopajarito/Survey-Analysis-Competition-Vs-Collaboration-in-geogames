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
table_trips_length = read.csv('data/Cyclist_Trip_length.csv')
table_answers = read.csv('data/Questionnaire_Answers.csv')
table_answers$group <- "none"
table_answers[which(table_answers$competition_1 >= -3),]$group <- "Competition"
table_answers[which(table_answers$collaboration_1 >= -3),]$group <- "Collaboration"
trips_joined <- merge(table_trips, table_participants)
trips_joined <- merge(trips_joined, table_trips_length, all.x = TRUE)
trips_joined <- merge(trips_joined, table_answers)
  