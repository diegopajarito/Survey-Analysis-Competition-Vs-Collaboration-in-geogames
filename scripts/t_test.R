# Description: This script evaluates the t-test for the answers of participants
# it compares the differences between participants asigned to competition and colaboration
# groups
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

library(sjmisc)
library(dplyr)
library(knitr)

# Setup 
table_answers <- read.csv('data/Questionnaire_Answers.csv')
table_answers$group <- ifelse( !is.na(table_answers$competition_1) , "Competition", ifelse(!is.na(table_answers$collaboration_1), "Collaboration", ""))





# Paired t test

# Satisfaction with cycling before and after - Comparison between groups collaboration / competitition
fligner.test(answers_collaboration$profile_cycling_1, answers_collaboration$satisfaction_cycling)
bartlett.test(answers_collaboration$profile_cycling_1, answers_collaboration$satisfaction_cycling)

# Summarize data by groups
md <- table_answers %>% # "Start with the data set we imported, d 
  group_by(group) %>% # Then group d by IV
  summarize(N = length(profile_cycling_1), # Then summarize each group
            Mean = mean(profile_cycling_1),
            SD = sd(profile_cycling_1),
            SE = SD/sqrt(N)) 
md
kable(md, digits = 2)

t_satisfaction_before_after_collaboration <- t.test(answers_collaboration$profile_cycling_1 , answers_collaboration$satisfaction_cycling)
with(answers_collaboration, t.test(profile_cycling_1 , satisfaction_cycling))




# Engagement with cycling before and after  - Comparison between groups collaboration / competitition


with(answers_collaboration, t.test(engagement_A1 , engagement_B1))
with(answers_competition, t.test(engagement_A1 , engagement_B1))





# Engagement with mobile applications for cycling before and after  - Comparison between groups collaboration / competitition


with(answers_collaboration, t.test(engagement_A3 , engagement_B3))
with(answers_competition, t.test(engagement_A3 , engagement_B3))






















t.test(answers_collaboration$profile_cycling_1 , answers_collaboration$satisfaction_cycling)













t.test(profile_cycling_1, profile_cycling_1)


















fligner.test(profile_cycling_1 , satisfaction_cycling, data = answers_collaboration)


t.test(table_answers$engagement_A1, table_answers$engagement_B1)

t.test(table_answers$engagement_A2, table_answers$engagement_B2)

t.test(table_answers$engagement_A3, table_answers$engagement_B3)











































a <- c( 1, 1, 1, 1, 1, 1, 0 )

b <- c( 1.1, 1.2, 1.2, 0.9, 0.9, 1.2, 1.1 )

t.test(a,b)




fishlengths <- c(2,4,9,20,50)
t.test(fishlengths,mu=20)









# Unpaired

comp vs colla