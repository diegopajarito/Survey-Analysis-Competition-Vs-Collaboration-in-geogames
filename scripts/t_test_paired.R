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
answers_satisfaction_paired %>% 
  group_by(time) %>%
  summarise(N = length(satisfaction),
            Mean = mean(satisfaction),
            SD = sd(satisfaction),
            SE = SD/sqrt(N)) 
# T Test
t.test(satisfaction_before$satisfaction, satisfaction_after$satisfaction ) 
t.test(answers_satisfaction_competition_before$satisfaction, answers_satisfaction_competition_after$satisfaction ) 
t.test(answers_satisfaction_competition_before[answers_satisfaction_competition_before$city=="Castelló",]$satisfaction, 
       answers_satisfaction_competition_after[answers_satisfaction_competition_after$city=="Castelló",]$satisfaction ) 
t.test(answers_satisfaction_competition_before[answers_satisfaction_competition_before$city=="Malta",]$satisfaction, 
       answers_satisfaction_competition_after[answers_satisfaction_competition_after$city=="Malta",]$satisfaction ) 
t.test(answers_satisfaction_competition_before[answers_satisfaction_competition_before$city=="Münster",]$satisfaction, 
       answers_satisfaction_competition_after[answers_satisfaction_competition_after$city=="Münster",]$satisfaction ) 

t.test(answers_satisfaction_collaboration_before$satisfaction, answers_satisfaction_collaboration_after$satisfaction)
t.test(answers_satisfaction_collaboration_before[answers_satisfaction_collaboration_before$city=="Castelló",]$satisfaction, 
       answers_satisfaction_collaboration_after[answers_satisfaction_collaboration_after$city=="Castelló",]$satisfaction)
t.test(answers_satisfaction_collaboration_before[answers_satisfaction_collaboration_before$city=="Malta",]$satisfaction, 
       answers_satisfaction_collaboration_after[answers_satisfaction_collaboration_after$city=="Malta",]$satisfaction)
t.test(answers_satisfaction_collaboration_before[answers_satisfaction_collaboration_before$city=="Münster",]$satisfaction, 
       answers_satisfaction_collaboration_after[answers_satisfaction_collaboration_after$city=="Münster",]$satisfaction)


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







#
#
# Paired test
# Engagement with cycling before and after - Comparison between groups collaboration / competitition

engagement_before <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$engagement_A1)
names(engagement_before) <- c("participant", "city", "group", "engagement")
engagement_before$time <- "Before"

engagement_after <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$engagement_B1)
names(engagement_after) <- c("participant", "city", "group", "engagement")
engagement_after$time <- "After"

engagement_paired <- rbind(engagement_before, engagement_after)
engagement_competition <- engagement_paired[engagement_paired$group == "Competition",]
engagement_competition_before <- engagement_competition[engagement_competition$time == "Before",]
engagement_competition_after <- engagement_competition[engagement_competition$time == "After",]
engagement_collaboration <- engagement_paired[engagement_paired$group == "Collaboration",]
engagement_collaboration_before <- engagement_collaboration[engagement_collaboration$time == "Before",]
engagement_collaboration_after <- engagement_collaboration[engagement_collaboration$time == "After",]

# Variance Test
fligner.test(engagement_before$engagement, engagement_after$engagement)

# Summarize data by groups
engagement_paired %>%                         # "Start with the data set we imported, d 
  group_by(time) %>%                           # Then group d by IV 
  summarise(N = length(engagement),      # Then summarize each group
            Mean = mean(engagement),
            SD = sd(engagement),
            SE = SD/sqrt(N)) 
# T Test
t.test(engagement_before$engagement, engagement_after$engagement ) 
t.test(engagement_competition_before$engagement, engagement_competition_after$engagement ) 
t.test(engagement_competition_before[engagement_competition_before$city == "Castelló",]$engagement,
       engagement_competition_after[engagement_competition_after$city == "Castelló",]$engagement ) 
t.test(engagement_competition_before[engagement_competition_before$city == "Malta",]$engagement,
       engagement_competition_after[engagement_competition_after$city == "Malta",]$engagement ) 
t.test(engagement_competition_before[engagement_competition_before$city == "Münster",]$engagement,
       engagement_competition_after[engagement_competition_after$city == "Münster",]$engagement ) 

t.test(engagement_collaboration_before$engagement, engagement_collaboration_after$engagement)
t.test(engagement_collaboration_before[engagement_collaboration_before$city == "Castelló",]$engagement, 
       engagement_collaboration_after[engagement_collaboration_after$city == "Castelló",]$engagement)
t.test(engagement_collaboration_before[engagement_collaboration_before$city == "Malta",]$engagement, 
       engagement_collaboration_after[engagement_collaboration_after$city == "Malta",]$engagement)
t.test(engagement_collaboration_before[engagement_collaboration_before$city == "Münster",]$engagement, 
       engagement_collaboration_after[engagement_collaboration_after$city == "Münster",]$engagement)


# Boxplot  
p_engagement <- ggplot(data = engagement_paired, aes(y = engagement, x = time, fill = time)) 
p_engagement_competition <- ggplot(data = engagement_competition, aes(y = engagement, x = time, fill = time)) 
p_engagement_collaboration <- ggplot(data = engagement_collaboration, aes(y = engagement, x = time, fill = time)) 

p_engagement + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_engagement + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_engagement + geom_boxplot() 
p_engagement_competition + geom_boxplot() 
p_engagement_competition + geom_boxplot() + facet_grid(. ~ city)
p_engagement_collaboration + geom_boxplot() 
p_engagement_collaboration + geom_boxplot() + facet_grid(. ~ city)
