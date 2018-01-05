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
source("scripts/setup.R")
# Set ggplot defaults & Custom colors 
themeMod <- theme_bw() + 
  theme(legend.position = "none", axis.text.y = element_text(hjust = 1))
theme_set(themeMod)
myFill <- function(...){
  scale_fill_manual(values = alpha(c("#5ab4ac","#d8b365", "#f5f5f5"),0.8),...)
}
scale_fill_discrete <- myFill

# Just working with the answers from participants that completed the experiment
table_answers <- table_answers[table_answers$group != "none",]


# Paired test
# Satisfaction with cycling. Comparison before the experiment and during the experiment
# Q1: “I would find cycling enjoyable”
# Q2: “Indicate how satisfied / dissatisfied in general you were with: cycling during the experiment”
satisfaction <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling, table_answers$satisfaction_1)
names(satisfaction) <- c("participant", "city", "group", "s_cycling", "s_during_experiment")
satisfaction_general <- data.frame(satisfaction$participant, satisfaction$city, satisfaction$group, satisfaction$s_cycling)
names(satisfaction_general) <- c("participant", "city", "group", "satisfaction")
satisfaction_general$context <- "General"
satisfaction_experiment <- data.frame(satisfaction$participant, satisfaction$city, satisfaction$group, satisfaction$s_during_experiment)
names(satisfaction_experiment) <- c("participant", "city", "group", "satisfaction")
satisfaction_experiment$context <- "Experiment"
satisfaction_context <- rbind(satisfaction_general, satisfaction_experiment)

# Normality Test
shapiro.test(satisfaction$s_cycling)
shapiro.test(satisfaction$s_during_experiment)

mean(satisfaction$s_cycling)
mean(satisfaction$s_during_experiment)
range(satisfaction$s_cycling)
range(satisfaction$s_during_experiment)

#Variance Test
fligner.test(satisfaction$s_cycling, satisfaction$s_during_experiment)

# T Test
t.test(satisfaction$s_cycling, satisfaction$s_during_experiment) 

# Effect size - Cohen's effect size
cohen.d(satisfaction$s_cycling, satisfaction$s_during_experiment)

# Wilcox Test
wilcox.test(satisfaction$s_cycling, satisfaction$s_during_experiment)
wilcox.test(satisfaction$s_during_experiment, satisfaction$s_cycling)

# Boxplot  
p_sat_cyc <- ggplot(data = satisfaction_context, aes(x= context, y = satisfaction ))
p_sat_cyc + geom_boxplot()   
p_sat_cyc + geom_boxplot() + facet_grid(. ~ city)


# Removing Outliers
out_general <- boxplot.stats(satisfaction_general$satisfaction)$out
satisfaction_general$satisfaction <- ifelse(satisfaction_general$satisfaction %in% out_general, NA, satisfaction_general$satisfaction)
out_experiment <- boxplot.stats(satisfaction_experiment$satisfaction)$out
satisfaction_experiment$satisfaction <- ifelse(satisfaction_experiment$satisfaction %in% out_experiment, NA, satisfaction_experiment$satisfaction)
satisfaction_context <- rbind(satisfaction_general, satisfaction_experiment)

# Wilcox Test without outliers
wilcox.test(satisfaction_general$satisfaction, satisfaction_experiment$satisfaction)
wilcox.test(satisfaction_general[satisfaction_general$city == "Castelló",]$satisfaction, satisfaction_experiment[satisfaction_experiment$city == "Castelló",]$satisfaction)
wilcox.test(satisfaction_general[satisfaction_general$city == "Malta",]$satisfaction, satisfaction_experiment[satisfaction_experiment$city == "Malta",]$satisfaction)
wilcox.test(satisfaction_general[satisfaction_general$city == "Münster",]$satisfaction, satisfaction_experiment[satisfaction_experiment$city == "Münster",]$satisfaction)

# Boxplot without outliers
p_sat_cyc <- ggplot(data = satisfaction_context, aes(x= context, y = satisfaction ))
p_sat_cyc + geom_boxplot()  
p_sat_cyc + geom_boxplot() + facet_grid(. ~ city)







# Paired test
# Satisfaction with cycling before and after - Comparison between groups collaboration / competitition

satisfaction_before <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$profile_cycling_1)
names(satisfaction_before) <- c("participant", "city", "group", "satisfaction")
satisfaction_before$time <- "Before"

satisfaction_after <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling)
names(satisfaction_after) <- c("participant", "city", "group", "satisfaction")
satisfaction_after$time <- "After"

answers_satisfaction_paired <- rbind(satisfaction_before, satisfaction_after)
answers_satisfaction_competition <- answers_satisfaction_paired[answers_satisfaction_paired$group == "Competition",]
answers_satisfaction_competition_before <- answers_satisfaction_competition[answers_satisfaction_competition$time == "Before",]
answers_satisfaction_competition_after <- answers_satisfaction_competition[answers_satisfaction_competition$time == "After",]
answers_satisfaction_collaboration <- answers_satisfaction_paired[answers_satisfaction_paired$group == "Collaboration",]
answers_satisfaction_collaboration_before <- answers_satisfaction_collaboration[answers_satisfaction_collaboration$time == "Before",]
answers_satisfaction_collaboration_after <- answers_satisfaction_collaboration[answers_satisfaction_collaboration$time == "After",]

# Normality Test Shapiro-Wilk
shapiro.test(answers_satisfaction_paired$satisfaction)
shapiro.test(answers_satisfaction_collaboration_before$satisfaction)
shapiro.test(answers_satisfaction_collaboration_after$satisfaction)

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


# Wilcox Test
wilcox.test(satisfaction_before$satisfaction, satisfaction_after$satisfaction)


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

engagement_before <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$engagement_A1)
names(engagement_before) <- c("participant", "city", "group", "engagement")
engagement_before$time <- "Before"

engagement_after <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$engagement_B1)
names(engagement_after) <- c("participant", "city", "group", "engagement")
engagement_after$time <- "After"

engagement_paired <- rbind(engagement_before, engagement_after)
engagement_competition <- engagement_paired[engagement_paired$group == "Competition",]
engagement_competition_before <- engagement_competition[engagement_competition$time == "Before",]
engagement_competition_after <- engagement_competition[engagement_competition$time == "After",]
engagement_collaboration <- engagement_paired[engagement_paired$group == "Collaboration",]
engagement_collaboration_before <- engagement_collaboration[engagement_collaboration$time == "Before",]
engagement_collaboration_after <- engagement_collaboration[engagement_collaboration$time == "After",]

# Normality Test Shapiro-Wilk
shapiro.test(engagement_paired$engagement)
shapiro.test(engagement_competition_before$engagement)
shapiro.test(engagement_collaboration_after$engagement)

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
