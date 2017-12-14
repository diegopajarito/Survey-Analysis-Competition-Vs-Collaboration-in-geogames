# Description: This script evaluates the t-test for the answers of participants
# it compares the differences between participants asigned to competition and colaboration
# groups
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

library(sjmisc)
library(dplyr)
library(knitr)
library(ggplot2)
library(Hmisc)

# Setup 
# Set ggplot defaults & Custom colors 
themeMod <- theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(hjust = 1))
theme_set(themeMod)
myFill <- function(...){
  scale_fill_manual(values = alpha(c("#5ab4ac","#d8b365", "#f5f5f5"),0.8),...)
}
scale_fill_discrete <- myFill
# Data reading and group asignation based on the answers
table_answers <- read.csv('data/Questionnaire_Answers.csv')
table_answers$group <- ifelse( !is.na(table_answers$competition_1) , "Competition", ifelse(!is.na(table_answers$collaboration_1), "Collaboration", NA))
table_answers <- table_answers[!is.na(table_answers$group),]





# Paired test
# Satisfaction with cycling before and after - Comparison between groups collaboration / competitition

satisfaction_before <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$profile_cycling_1)
names(satisfaction_before) <- c("participant", "city", "group", "satisfaction")
satisfaction_before$time <- "Before"

satisfaction_after <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling)
names(satisfaction_after) <- c("participant", "city", "group", "satisfaction")
satisfaction_after$time <- "After"

answers_satisfaction_paired <- rbind(satisfaction_before, satisfaction_after)
answers_satisfaction_competition <- answers_satisfaction_paired[answers_satisfaction_paired$group == "Competition",]
answers_satisfaction_competition_before <- answers_satisfaction_competition[answers_satisfaction_competition$time == "Before",]
answers_satisfaction_competition_after <- answers_satisfaction_competition[answers_satisfaction_competition$time == "After",]
answers_satisfaction_collaboration <- answers_satisfaction_paired[answers_satisfaction_paired$group == "Collaboration",]
answers_satisfaction_collaboration_before <- answers_satisfaction_collaboration[answers_satisfaction_collaboration$time == "Before",]
answers_satisfaction_collaboration_after <- answers_satisfaction_collaboration[answers_satisfaction_collaboration$time == "After",]

# Variance Test
fligner.test(satisfaction_before$satisfaction, satisfaction_after$satisfaction)

# Summarize data by groups
answers_satisfaction_paired %>%                         # "Start with the data set we imported, d 
  group_by(time) %>%                           # Then group d by IV 
  summarise(N = length(satisfaction),      # Then summarize each group
            Mean = mean(satisfaction),
            SD = sd(satisfaction),
            SE = SD/sqrt(N)) 
# T Test
t.test(answers_satisfaction_competition$satisfaction, satisfaction_after$satisfaction ) 
t.test(answers_satisfaction_competition_before$satisfaction, answers_satisfaction_competition_after$satisfaction ) 
t.test(answers_satisfaction_collaboration_before$satisfaction, answers_satisfaction_collaboration_after$satisfaction ) 

# T Test - Satisfaction


# Boxplot  
p <- ggplot(data = answers_satisfaction_paired, aes(y = satisfaction, x = time, fill = time)) 
p_competition <- ggplot(data = answers_satisfaction_competition, aes(y = satisfaction, x = time, fill = time)) 
p_collaboration <- ggplot(data = answers_satisfaction_collaboration, aes(y = satisfaction, x = time, fill = time)) 

p + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p + geom_boxplot() 
p_competition + geom_boxplot() 
p_collaboration + geom_boxplot() 





# Boxplot brings a the outliers that should not be considered in the analysis
outliers_collaboration <-  answers_satisfaction_paired[answers_satisfaction_paired$satisfaction<=0,]
answers_satisfaction_witout_outliers <- answers_satisfaction_paired[answers_satisfaction_paired$satisfaction>0,]

# T Test
t.test(answers_satisfaction_witout_outliers$satisfaction, answers_satisfaction_witout_outliers$satisfaction ) 

# Boxplot  
p_wo <- ggplot(data = answers_satisfaction_witout_outliers, aes(y = satisfaction, 
                                                    x = time, fill = time)) 
p_wo + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_wo + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_wo + geom_boxplot() 








# Paired test
# Engagement with cycling before and after - Comparison between groups collaboration / competitition

satisfaction_before <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$profile_cycling_1)
names(satisfaction_before) <- c("participant", "city", "group", "satisfaction")
satisfaction_before$time <- "Before"

satisfaction_after <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling)
names(satisfaction_after) <- c("participant", "city", "group", "satisfaction")
satisfaction_after$time <- "After"

answers_satisfaction_paired <- rbind(satisfaction_before, satisfaction_after)

# Variance Test
fligner.test(satisfaction_before$satisfaction, satisfaction_after$satisfaction)

# Summarize data by groups
answers_satisfaction_paired %>%                         # "Start with the data set we imported, d 
  group_by(time) %>%                           # Then group d by IV 
  summarise(N = length(satisfaction),      # Then summarize each group
            Mean = mean(satisfaction),
            SD = sd(satisfaction),
            SE = SD/sqrt(N)) 
# T Test
t.test(satisfaction_before$satisfaction, satisfaction_after$satisfaction ) 

# Boxplot  
p <- ggplot(data = answers_satisfaction_paired, aes(y = satisfaction, 
                                                    x = time, fill = time)) 
p + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p + geom_boxplot() 




# Boxplot brings a the outliers that should not be considered in the analysis
outliers_collaboration <-  answers_satisfaction_paired[answers_satisfaction_paired$satisfaction<=0,]
answers_satisfaction_witout_outliers <- answers_satisfaction_paired[answers_satisfaction_paired$satisfaction>0,]

# T Test
t.test(answers_satisfaction_witout_outliers$satisfaction, answers_satisfaction_witout_outliers$satisfaction ) 

# Boxplot  
p_wo <- ggplot(data = answers_satisfaction_witout_outliers, aes(y = satisfaction, 
                                                                x = time, fill = time)) 
p_wo + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_wo + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_wo + geom_boxplot() 









names(answers_satisfaction) <- c("participant", "city", "group", "satisfaction_before", "satisfaction_after")
answers_satisfaction_collaboration <- answers_satisfaction[answers_satisfaction$group == "Collaboration",]
answers_satisfaction_competition <- answers_satisfaction[answers_satisfaction$group == "Competition",]


# Paired t test, same participants before and after answers 
fligner.test(answers_satisfaction$profile_cycling_1, answers_satisfaction$satisfaction_cycling)
bartlett.test(answers_collaboration$profile_cycling_1, answers_collaboration$satisfaction_cycling)

# Summarize data by groups
md <- answers_satisfaction %>%                         # "Start with the data set we imported, d 
  group_by(group) %>%                           # Then group d by IV 
  summarise(N = length(satisfaction_before),      # Then summarize each group
            Mean = mean(satisfaction_before),
            SD = sd(satisfaction_before),
            SE = SD/sqrt(N)) 
md
kable(md, digits = 2)

with(answers_satisfaction, t.test(satisfaction_before , satisfaction_after))



# Unpaired t test, same participants before and after answers 
p <- ggplot(data = satisfaction_after, aes(y = profile_cycling_1, 
                          x = group, fill = group)) 
# Barplot of the means of the two groups
p + stat_summary(fun.y = "mean", geom = "bar")  
# Barplot of the means of the two groups and the Error bars
p + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p + geom_boxplot() 

# Boxplot brings a the outliers that should not be considered in the analysis
outliers_collaboration <- c(14, 5, 3, 6)
answers_without_outliers_coolaboration <- table_answers[!table_answers$Participant %in% outliers_collaboration,]



with(answers_without_outliers_coolaboration, t.test(profile_cycling_1 , satisfaction_cycling))

p_w <- ggplot(data = answers_without_outliers_coolaboration, aes(y = profile_cycling_1,
                                                               x = group, fill = group)) 
p_w + geom_boxplot() 


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
