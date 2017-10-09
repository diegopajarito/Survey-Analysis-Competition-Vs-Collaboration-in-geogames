# Description:
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

rm(list=ls())
table_answers = read.csv('data/Questionnaire_Answers.csv')


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


# transport mode
transport_labels <- c("I use my private car","By public transport (including Taxi)","By bicycle","By walking")
answers_transport <- table_answers[,8:11]
transports_total <- data.frame( modes=transport_labels, count=colSums(answers_transport, na.rm=TRUE) )
ggplot(transports_total, aes(modes, count)) + geom_col()

pdf("graphs/transport_participants.pdf")
ggplot(transports_total, aes(modes, count)) + geom_col()
dev.off()


