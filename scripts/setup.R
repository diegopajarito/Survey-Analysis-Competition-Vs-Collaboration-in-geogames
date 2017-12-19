# Setup
library(ggplot2)
require(lubridate)

table_participants = read.csv('data/Cyclist_Experiment.csv')
table_trips = read.csv('data/Cyclist_Trip.csv', sep = '\t')
table_answers = read.csv('data/Questionnaire_Answers.csv')
trips_joined = merge(table_participants,table_trips)
