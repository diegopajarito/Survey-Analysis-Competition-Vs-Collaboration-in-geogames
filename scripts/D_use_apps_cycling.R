# Description: This script evaluates the answers for the use of mobile applications for cycling
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

app_cycling <- data.frame(table_answers$gaming_app_cycling_googlefit, table_answers$gaming_app_cycling_bikecitizens, table_answers$gaming_app_cycling_endomondo,
                          table_answers$gaming_app_cycling_fitbit, table_answers$gaming_app_cycling_human, table_answers$gaming_app_cycling_runtastic, table_answers$gaming_app_cycling_strava,
                          table_answers$gaming_app_cycling_wikiloc, table_answers$gaming_app_cycling_other, table_answers$gaming_app_cycling)
names(app_cycling) <- c( " google_fit", " bikecitizens", " endomondo", " fitbit", " human", " runtastic", " strava", " wikiloc", "other", "using_no_app")
app_cycling[app_cycling$using_no_app != "N",]$using_no_app <- NA
app_cycling[app_cycling$other == "",]$other <- NA

count_app_cycling <- sapply(app_cycling, function(y) sum(length(which(!is.na(y)))))

data <- data.frame( names(count_app_cycling), count_app_cycling )
names(data) <- c("application", "count")


# Circular bar graph
ggplot(data, aes(x = application, y = count ,fill = application)) + 
  geom_bar(width = 0.85, stat="identity") +    
  coord_polar(theta = "y") +    # To use a polar plot and not a basic barplot
  xlab("") + ylab("") +    #Remove useless labels of axis
  ylim(c(0,33)) + #Increase ylim to avoid having a complete circle
  geom_text(data = data, hjust = 1, size = 3, aes(x = application, y = 0, label = application)) +   #Add group labels close to the bars :
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())    #Remove useless legend, y axis ticks and y axis text

# Circular bar graph with facets
ggplot(data, aes(x = application, y = count ,fill = application)) + 
  geom_bar(width = 0.85, stat="identity") +    
  coord_polar(theta = "y") +    # To use a polar plot and not a basic barplot
  xlab("") + ylab("") +    #Remove useless labels of axis
  ylim(c(0,20)) + #Increase ylim to avoid having a complete circle
  geom_text(data = data, hjust = 1, size = 3, aes(x = application, y = 0, label = application)) +   #Add group labels close to the bars :
  theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())    #Remove useless legend, y axis ticks and y axis text

