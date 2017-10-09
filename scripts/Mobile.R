# Description: Graphs for the use of mobile phone section
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

table_answers = read.csv('data/Questionnaire_Answers.csv')

# use of mobile phone
time_mobile_values <- c(1,2,3,4,5)
tmobile_labels <- c("Less than 30 min","Between 30 min and 1 hour","Between 1 and 2 hours","Between 2 and 5 hours","More than 5 hours ")
tmobile_factors <- factor(time_mobile_values)
answers_tmobile = factor(table_answers$gaming_mobile_time, tmobile_factors, labels = tmobile_labels)
barplot(table(answers_tmobile))

pdf("graphs/time_mobile.pdf")
barplot(table(answers_tmobile))
dev.off()


# money on mobile applications
apps_money_values <- c(1,2,3,4)
apps_money_labels <- c("I don't spend money on that","Less than 10 EUR","Between 10 and 25 EUR","More than 25 EUR")
apps_money_factors <- factor(apps_money_values)
answers_apps_money = factor(table_answers$gaming_money_apps, apps_money_factors, labels = apps_money_labels)
barplot(table(answers_apps_money))

pdf("graphs/money_apps.pdf")
barplot(table(answers_apps_money))
dev.off()


# mobile app categories
apps_labels <- c("Basic Utility","Messaging and Social Networks","Lifestyle and Fitness","Games and Entertainment","Productivity","News and Information")
answers_apps <- table_answers[,29:34]
apps_total <- data.frame( cat=apps_labels, count=colSums(answers_apps, na.rm=TRUE) )
ggplot(apps_total, aes(cat, count)) + geom_col()

pdf("graphs/apps_cathegory.pdf")
ggplot(apps_total, aes(cat, count)) + geom_col()
dev.off()

# games on mobile phone
time_games_values <- c(1,2,3,4,5)
tgames_labels <- c("Less than 30 min","Between 30 min and 1 hour","Between 1 and 2 hours","Between 2 and 5 hours","More than 5 hours ")
tgames_factors <- factor(time_games_values)
answers_tgames = factor(table_answers$gaming_play_time, tgames_factors, labels = tgames_labels)
barplot(table(answers_tgames))

pdf("graphs/time_games.pdf")
barplot(table(answers_tgames))
dev.off()


# money on mobile games
games_money_values <- c(1,2,3,4)
games_money_labels <- c("I don't spend money on that","Less than 10 EUR","Between 10 and 25 EUR","More than 25 EUR")
games_money_factors <- factor(games_money_values)
answers_games_money = factor(table_answers$gaming_money_games, games_money_factors, labels = games_money_labels)
barplot(table(answers_games_money))

pdf("graphs/money_games.pdf")
barplot(table(answers_games_money))
dev.off()


# mobile games categories
games_labels <- c("Simulation","Action","Puzzle","Stealth Shooter","Combat","First Person Shooting","Sports","Role-Playing","Educational","None")
answers_games <- table_answers[,37:46]
games_total <- data.frame( cat=games_labels, count=colSums(answers_games, na.rm=TRUE) )
ggplot(games_total, aes(cat, count)) + geom_col()

pdf("graphs/games_cathegory.pdf")
ggplot(games_total, aes(cat, count)) + geom_col()
dev.off()


# mobile games categories
app_cycling_labels <- c("Google Fit","Endomondo","Bike Citizens","Fitbit","Strava","Runtastic","Human","Wikiloc","None")
answers_app_cycling <- table_answers[,49:57]
app_cycling_total <- data.frame( cat=app_cycling_labels, count=colSums(answers_app_cycling, na.rm=TRUE) )
ggplot(app_cycling_total, aes(cat, count)) + geom_col()

pdf("graphs/app_cycling.pdf")
ggplot(app_cycling_total, aes(cat, count)) + geom_col()
dev.off()



# social app categories
app_social_labels <- c("Foursquare","Trip Advisor","Find my friends","Tinder","Swarm","Snapchat map","Waze","None")
answers_app_social <- table_answers[,59:66]
app_social_total <- data.frame( cat=app_social_labels, count=colSums(answers_app_social, na.rm=TRUE) )
ggplot(app_social_total, aes(cat, count)) + geom_col()

pdf("graphs/app_social.pdf")
ggplot(app_social_total, aes(cat, count)) + geom_col()
dev.off()


# social app categories
wearable_labels <- c("Fitness band","VR headset","Smart glasses","Smart watch","Smart headphones","none")
answers_wearable <- table_answers[,68:73]
wearable_total <- data.frame( cat=wearable_labels, count=colSums(answers_wearable, na.rm=TRUE) )
ggplot(wearable_total, aes(cat, count)) + geom_col()

pdf("graphs/app_wearable.pdf")
ggplot(wearable_total, aes(cat, count)) + geom_col()
dev.off()
