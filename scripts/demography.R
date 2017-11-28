# Description: TThis script generates the resources for anlyzing 
# participants demography.
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

# setup
library(reshape2)
library(ggplot2)


rm(list=ls())
table_answers <- read.csv('data/Questionnaire_Answers.csv')

gender_values <- c(1,2,3)
gender_labels <- c("Male","Female","Other")
gender_factors <- factor(gender_values)
gender_factor <- factor(table_answers$dem_gender, gender_factors,labels = gender_labels)

marital_values <- c(1,2,3,4,5,6)
marital_labels <- c("Single","In relationship but not living together","In relationship and living together",
                   "Married","Divorced or widowed","Other")
marital_factors <- factor(marital_values)
marital_factor <- factor(table_answers$dem_marital, marital_factors,labels = marital_labels)

city_factors <- factor(table_answers$City)
city_factors[1]

transport_values <- c(1)
transport_labels <- c("True")
transport = data.frame(table_answers$dem_transport_walk, table_answers$dem_transport_bicycle, 
                       table_answers$dem_transport_public, table_answers$dem_transport_car)
names(transport) = c("Walk", "Bicycle", "Public Transport", "Private Car")
ggplot(transport) +
  geom_bar()
  

demography_data <- data.frame(gender_factor, city_factors, marital_factor, table_answers$dem_age)


# Anova
# Values that sumarises the basic statistics 
summary(demography_data)

summary(demography_data[city_factors == "Münster",])
summary(demography_data[city_factors == "Castelló",])
summary(demography_data[city_factors == "Malta",])


tapply(demography_data$table_answers.dem_age, demography_data$city_factors, summary)




# Distribution of transport modes
x_label <- ""
y_label <- "Participants"

transport_labels <- c("private car","Public transport","Bicycle","Walking")
answers_transport <- table_answers[,8:11] 
transports_total <- data.frame( modes=transport_labels, count=colSums(answers_transport, na.rm=TRUE) )

answers_transport_muenster <- table_answers[table_answers$City == "Münster",8:11] 
transports_muenster <- data.frame( modes=transport_labels, count=colSums(answers_transport_muenster, na.rm=TRUE) )
transports_total$muenster = transports_muenster$count

answers_transport_castello<- table_answers[table_answers$City == "Castelló",8:11] 
transports_castello <- data.frame( modes=transport_labels, count=colSums(answers_transport_castello, na.rm=TRUE) )
transports_total$castello = transports_castello$count

answers_transport_malta<- table_answers[table_answers$City == "Malta",8:11] 
transports_malta <- data.frame( modes=transport_labels, count=colSums(answers_transport_malta, na.rm=TRUE) )
transports_total$malta = transports_malta$count

ggplot(transports_total, aes(modes, muenster, fill=modes)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  xlab(x_label) + ylab(y_label) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Greens", direction = -1)

freq = table(col(transports_total), as.matrix((transports_total)))

ggplot(transports_total, aes(modes )) +
  geom_bar(aes(fill=modes), position = "dodge", stat="identity")






tmp <- melt(answers_transport_city, id.vars = NULL)
names(tmp) <- c('Occupation', 'ID')

ggplot(data = tmp, aes(x = Occupation, fill= ID), stat="count") + geom_histogram()


svg(filename="graphs/Demography_graph1.svg", 
    width=6.5, height=3, pointsize=10)
ggplot(transports_total, aes(modes, count, fill=modes)) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip() + ylim(0, 60) +
  xlab(x_label) + ylab(y_label) + 
  theme(legend.position="none") + scale_fill_brewer(palette = "Greens", direction = -1)
dev.off()




# Age of participants
answers_ages = table_answers[,4]
age_histogram <- hist(answers_ages, breaks=4, main = "")
age_density <- density(table_answers[,4])
lines(lines(age_density$x,age_density$y*(1/sum(age_histogram$density))*length(answers_ages)))

pdf("graphs/age_participants.pdf")
age_histogram <- hist(answers_ages, breaks=4, main = "")
lines(lines(age_density$x,age_density$y*(1/sum(age_histogram$density))*length(answers_ages)))
dev.off()


# gender of participants
gender_values <- c(1,2,3)
gender_labels <- c("male","female","other")
gender_factors <- factor(gender_values)
answers_gender = factor(table_answers[,3], gender_factors, labels = gender_labels)
barplot(table(answers_gender))

pdf("graphs/gender_participants.pdf")
barplot(table(answers_gender))
dev.off()


# education of participants
education_values <- c(1,2,3,4)
education_labels <- c("primary","secondary","university","Master / Ph.D.")
education_factors <- c(education_values)
answers_education = factor(table_answers[,6], education_factors, labels = education_labels)
barplot(table(answers_education))


pdf("graphs/education_participants.pdf")
barplot(table(answers_education))
dev.off()



# marital status
marital_values <- c(1,2,3,4,5,6)
marital_labels <- c("Single","In relationship but not living together","In relationship and living together","Married","Divorced or widowed","Other")
answers_marital = factor(table_answers[,7], marital_values, labels = marital_labels)
barplot(table(answers_marital))

pdf("graphs/marital_participants.pdf")
barplot(table(answers_marital))
dev.off()
