
table_participants = read.csv('data/Cyclist_Experiment.csv')
table_trips = read.csv('data/Cyclist_Trip.csv', sep = '\t')

trips_joined = merge(table_participants,table_trips)

trips_device = subset(trips_joined, device == "14571aeb92ef00fd")
trips_device = trips_device[order(as.numeric(trips_device$trip_count)),]
trips_device_1 = subset(trips_joined, device == "14a96735022e788e")
trips_device_1 = trips_device_1[order(as.numeric(trips_device_1$trip_count)),]

trips_joined$campaign_day = as.Date(trips_joined$X_created_at) - as.Date(trips_joined$questionnaire1)

plot (trips_joined$campaign_day, trips_joined$trip_count)
plot (trips_device$campaign_day, trips_device$trip_count)
plot (trips_device$campaign_day, trips_device$trip_count, type = 'n')
lines(trips_device$campaign_day, trips_device$trip_count)
lines(trips_device_1$campaign_day, trips_device_1$trip_count)
