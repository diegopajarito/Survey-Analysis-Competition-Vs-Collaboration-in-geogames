# Description: This script evaluates the answers for the use of mobile devices and applications
# and the difference between cities
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup 
library(ggplot2)
# Data reading and group asignation based on the answers
table_answers <- read.csv('data/Questionnaire_Answers.csv', stringsAsFactors=FALSE)

table_answers <- table_answers[table_answers$City == "Münster",]
table_answers <- table_answers[which( table_answers$competition_1 >= -3 ),]

mobile_use <- data.frame(table_answers$gaming_apps_basic, table_answers$gaming_apps_messaging, table_answers$gaming_apps_lifestyle,
                          table_answers$gaming_apps_games, table_answers$gaming_apps_productivity, table_answers$gaming_apps_news)
names(mobile_use) <- c( " basic", " messaging", " lifestyle", " games", " productivity", " news")

count_mobile_use <- sapply(mobile_use, function(y) sum(length(which(!is.na(y)))))

data <- data.frame( names(count_mobile_use), count_mobile_use )
names(data) <- c("use", "count")


# Circular bar graph
ggplot(data, aes(x = use, y = count ,fill = use)) + 
  geom_bar(width = 0.85, stat="identity") +    
  coord_polar(theta = "y") +    # To use a polar plot and not a basic barplot
  xlab("") + ylab("") +    #Remove useless labels of axis
  ylim(c(0,33)) + #Increase ylim to avoid having a complete circle
  geom_text(data = data, hjust = 1, size = 3, aes(x = use, y = 0, label = use)) +   #Add group labels close to the bars :
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())    #Remove useless legend, y axis ticks and y axis text

# Circular bar graph with facets
ggplot(data, aes(x = use, y = count ,fill = use)) + 
  geom_bar(width = 0.85, stat="identity") +    
  coord_polar(theta = "y") +    # To use a polar plot and not a basic barplot
  xlab("") + ylab("") +    #Remove useless labels of axis
  ylim(c(0,20)) + #Increase ylim to avoid having a complete circle
  geom_text(data = data, hjust = 1, size = 3, aes(x = application, y = 0, label = application)) +   #Add group labels close to the bars :
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())    #Remove useless legend, y axis ticks and y axis text

