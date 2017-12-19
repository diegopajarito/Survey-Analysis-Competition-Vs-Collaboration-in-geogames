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
library(effsize)

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


# Un-paired test
# Satisfaction with cycling after the experiment - Comparison between groups collaboration / competitition
shapiro.test(table_answers$satisfaction_cycling)
satisfaction <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling, table_answers$satisfaction_1)
names(satisfaction) <- c("participant", "city", "group", "s_cycling", "s_during_experiment")

satisfaction_competition <- satisfaction[satisfaction$group == "Competition",]
satisfaction_collaboration <- satisfaction[satisfaction$group == "Collaboration",]

#Variance Test
fligner.test(satisfaction_competition$s_cycling, satisfaction_collaboration$s_cycling)

# Summarize data by groups
satisfaction %>%                         # "Start with the data set we imported, d 
  group_by(group) %>%                           # Then group d by IV 
  summarise(N = length(s_during_experiment),      # Then summarize each group
            Mean = mean(s_during_experiment),
            SD = sd(s_during_experiment),
            SE = SD/sqrt(N)) 
# T Test
t.test(satisfaction_competition$s_cycling, satisfaction_collaboration$s_cycling ) 
t.test(satisfaction_competition$s_during_experiment, satisfaction_collaboration$s_during_experiment ) 

# Effect size - Cohen's effect size
cohen.d(satisfaction_competition$s_cycling, satisfaction_collaboration$s_cycling)
cohen.d(satisfaction_competition$s_during_experiment, satisfaction_collaboration$s_during_experiment)


# Boxplot  
p_cycling <- ggplot(data = satisfaction, aes(y = s_cycling, x = group, fill = group)) 
p_during_experiment <- ggplot(data = satisfaction, aes(y = s_during_experiment, x = group, fill = group)) 

p_cycling + stat_summary(fun.y = "mean", geom = "bar")  
p_during_experiment + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_cycling + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
p_during_experiment + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 

# Boxplot comparizon of the two groups
p_cycling + geom_boxplot() 
p_during_experiment + geom_boxplot() 

p_cycling + geom_boxplot() + facet_grid(. ~ city)
p_during_experiment + geom_boxplot() + facet_grid(. ~ city)




# Un-paired test
# Engagement with cycling after the experiment - Comparison between groups collaboration / competitition

engagement <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$engagement_B1, table_answers$engagement_cycling_2w, table_answers$engagement_cycling_future)
names(engagement) <- c("participant", "city", "group", "engagement", "engagement_2w", "engagement_future")

engagement_competition <- engagement[engagement$group == "Competition",]
engagement_collaboration <- engagement[engagement$group == "Collaboration",]

#Variance Test
fligner.test(engagement_competition$engagement, engagement_collaboration$engagement)

# Summarize data by groups
engagement %>%                         # "Start with the data set we imported, d 
  group_by(group) %>%                           # Then group d by IV 
  summarise(N = length(engagement),      # Then summarize each group
            Mean = mean(engagement),
            SD = sd(engagement),
            SE = SD/sqrt(N)) 
# T Test
t.test(engagement_competition$engagement, engagement_collaboration$engagement ) 
t.test(engagement_competition$engagement_2w, engagement_collaboration$engagement_2w ) 

t.test(engagement_competition$engagement_future, engagement_collaboration$engagement_future ) 

# Effect size - Cohen's effect size
cohen.d(engagement_competition$engagement, engagement_collaboration$engagement)
cohen.d(engagement_competition$engagement_2w, engagement_collaboration$engagement_2w)
cohen.d(engagement_competition$engagement_future, engagement_collaboration$engagement_future ) 



# Boxplot  
p_engagement <- ggplot(data = engagement, aes(y = engagement, x = group, fill = group)) 
p_engagement_2w <- ggplot(data = engagement, aes(y = engagement_2w, x = group, fill = group))
p_engagement_future <- ggplot(data = engagement, aes(y = engagement_future, x = group, fill = group))

p + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_engagement + geom_boxplot() 
p_engagement_2w + geom_boxplot()
p_engagement_future + geom_boxplot()

p_engagement + geom_boxplot() + facet_grid(. ~ city)













# Un-paired test
# Satisfaction with cycling / Satisfaction during the experiment - Comparison between groups

satisfaction_comparison <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling, table_answers$satisfaction_app_1)
names(satisfaction_comparison) <- c("participant", "city", "group", "satisfaction_cycling", "satisfaction_exp")
satisfaction_general <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_cycling)
names(satisfaction_general) <- c("participant", "city", "group", "satisfaction")
satisfaction_general$context <- "general"
satisfaction_experiment <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$satisfaction_app_1)
names(satisfaction_experiment) <- c("participant", "city", "group", "satisfaction")
satisfaction_experiment$context <- "experiment"

satisfaction_context_cc <- rbind(satisfaction_general, satisfaction_experiment)

satisfaction_comparison_competition <- satisfaction_comparison[satisfaction_comparison$group == "Competition",]
satisfaction_comparison_collaboration <- satisfaction_comparison[satisfaction_comparison$group == "Collaboration",]


satisfaction_comparison_cc <- rbind(satisfaction_competition, satisfaction_competition)

#Variance Test
fligner.test(satisfaction_comparison$enjoyment_1, enjoyment_collaboration$enjoyment_1)



# T Test
t.test(satisfaction_comparison$satisfaction_cycling, satisfaction_comparison$satisfaction_exp)
t.test(enjoyment_competition[enjoyment_competition$city == "Castelló",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Castelló",]$enjoyment_1)
t.test(enjoyment_competition[enjoyment_competition$city == "Malta",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Malta",]$enjoyment_1)
t.test(enjoyment_competition[enjoyment_competition$city == "Münster",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Münster",]$enjoyment_1)
t.test(satisfaction_comparison_competition$satisfaction_cycling, satisfaction_comparison_competition$satisfaction_exp)
t.test(satisfaction_comparison_collaboration$satisfaction_cycling, satisfaction_comparison_collaboration$satisfaction_exp)

# Effect size - Cohen's effect size
cohen.d(satisfaction_comparison$satisfaction_cycling, satisfaction_comparison$satisfaction_exp)

cohen.d(satisfaction_comparison_competition$satisfaction_cycling, satisfaction_comparison_competition$satisfaction_exp)
cohen.d(satisfaction_comparison_collaboration$satisfaction_cycling, satisfaction_comparison_collaboration$satisfaction_exp) 

# Boxplot  
p_satisfaction_ex_1 <- ggplot(data = satisfaction_context_cc, aes(y = satisfaction, x = context, fill = context)) 

p_satisfaction_ex_1 + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_satisfaction_ex_1 + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_satisfaction_ex_1 + geom_boxplot() 


p_satisfaction_ex_1 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_2 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_3 + geom_boxplot() + facet_grid(. ~ city)








# Un-paired test
# Enjoyment with Competition / Satisfaction after the experiment - Comparison between groups 

enjoyment_competition <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$competition_1, table_answers$competition_2, table_answers$competition_3)
names(enjoyment_competition) <- c("participant", "city", "group", "enjoyment_1", "enjoyment_2", "enjoyment_3")
enjoyment_competition <- enjoyment_competition[enjoyment_competition$group == "Competition",]

enjoyment_collaboration <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$collaboration_1, table_answers$collaboration_2, table_answers$collaboration_3)
names(enjoyment_collaboration) <- c("participant", "city", "group", "enjoyment_1", "enjoyment_2", "enjoyment_3")
enjoyment_collaboration <- enjoyment_collaboration[enjoyment_collaboration$group == "Collaboration",]

enjoyment_cc <- rbind(enjoyment_competition, enjoyment_collaboration)

#Variance Test
fligner.test(enjoyment_competition$enjoyment_1, enjoyment_collaboration$enjoyment_1)
fligner.test(enjoyment_competition$enjoyment_2, enjoyment_collaboration$enjoyment_2)
fligner.test(enjoyment_competition$enjoyment_3, enjoyment_collaboration$enjoyment_3)

# Summarize data by groups
enjoyment_cc %>%                         # "Start with the data set we imported, d 
  group_by(group) %>%                           # Then group d by IV 
  summarise(N = length(enjoyment_1),      # Then summarize each group
            Mean = mean(enjoyment_1),
            SD = sd(enjoyment_1),
            SE = SD/sqrt(N)) 

# T Test
t.test(enjoyment_competition$enjoyment_1, enjoyment_collaboration$enjoyment_1)
t.test(enjoyment_competition[enjoyment_competition$city == "Castelló",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Castelló",]$enjoyment_1)
t.test(enjoyment_competition[enjoyment_competition$city == "Malta",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Malta",]$enjoyment_1)
t.test(enjoyment_competition[enjoyment_competition$city == "Münster",]$enjoyment_1, 
       enjoyment_collaboration[enjoyment_collaboration$city == "Münster",]$enjoyment_1)
t.test(enjoyment_competition$enjoyment_2, enjoyment_collaboration$enjoyment_2)
t.test(enjoyment_competition$enjoyment_3, enjoyment_collaboration$enjoyment_3)

# Effect size - Cohen's effect size
cohen.d(enjoyment_competition$enjoyment_1, enjoyment_collaboration$enjoyment_1)
cohen.d(enjoyment_competition[enjoyment_competition$city == "Castelló",]$enjoyment_1, 
        enjoyment_collaboration[enjoyment_collaboration$city == "Castelló",]$enjoyment_1)
cohen.d(enjoyment_competition[enjoyment_competition$city == "Malta",]$enjoyment_1, 
        enjoyment_collaboration[enjoyment_collaboration$city == "Malta",]$enjoyment_1)
cohen.d(enjoyment_competition[enjoyment_competition$city == "Münster",]$enjoyment_1, 
        enjoyment_collaboration[enjoyment_collaboration$city == "Münster",]$enjoyment_1)
cohen.d(enjoyment_competition$enjoyment_2, enjoyment_collaboration$enjoyment_2)
cohen.d(enjoyment_competition$enjoyment_3, enjoyment_collaboration$enjoyment_3,  na.rm=TRUE) 

# Boxplot  
p_enjoyment_1 <- ggplot(data = enjoyment_cc, aes(y = enjoyment_1, x = group, fill = group)) 
p_enjoyment_2 <- ggplot(data = enjoyment_cc, aes(y = enjoyment_2, x = group, fill = group))
p_enjoyment_3 <- ggplot(data = enjoyment_cc, aes(y = enjoyment_3, x = group, fill = group))

p_enjoyment_1 + stat_summary(fun.y = "mean", geom = "bar")  

# Barplot of the means of the two groups and the Error bars
p_enjoyment_1 + stat_summary(fun.y = "mean", geom = "bar") +
  stat_summary(fun.data = "mean_cl_normal", 
               geom = "errorbar", 
               width = 0.1) 
# Boxplot comparizon of the two groups
p_enjoyment_1 + geom_boxplot() 
p_enjoyment_2 + geom_boxplot()
p_enjoyment_3 + geom_boxplot()

p_enjoyment_1 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_2 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_3 + geom_boxplot() + facet_grid(. ~ city)











# Un-paired test
# Satisfaction with Cycling in general / During the experiment - Comparison between participants using or not mobile applications 

participants_mobileapp <- data.frame(table_answers$Participant, table_answers$City, table_answers$group, table_answers$gaming_app_cycling, table_answers$gaming_app_cycling_strava, 
                                     table_answers$profile_cycling_1, table_answers$satisfaction_cycling, table_answers$satisfaction_1,
                                     table_answers$engagement_A1, table_answers$engagement_B1)
names(participants_mobileapp) <- c("participant", "city", "group", "app_cycling", "app_strava", "satisfaction_before", "satisfaction_after", "satisfaction_experiment", "engagement_before", "engagement_after")

participants_using_app <- participants_mobileapp[participants_mobileapp$app_cycling == "Y",]
participants_not_using_app <- participants_mobileapp[participants_mobileapp$app_cycling == "N",]
participants_using_app_competition <- participants_using_app[participants_using_app$group == "Competition",]
participants_using_app_collaboration <- participants_using_app[participants_using_app$group == "Collaboration",]
participants_not_using_app_competitiion <- participants_not_using_app[participants_not_using_app$group == "Competition",]
participants_not_using_app_collaboration <- participants_not_using_app[participants_not_using_app$group == "Collaboration",]
participants_mobileapp[participants_mobileapp$app_cycling == "N",]$app_strava <- "None"
participants_mobileapp[is.na(participants_mobileapp$app_strava),]$app_strava <- "Other App"
participants_mobileapp[which(participants_mobileapp$app_strava == 1),]$app_strava <- "Strava"
participants_using_strava <- participants_mobileapp[which(participants_mobileapp$app_strava == "Strava"),]
participants_notusing_strava <- participants_mobileapp[which(participants_mobileapp$app_strava != "Strava"),]
participants_using_app_nostrava <- participants_mobileapp[which(participants_mobileapp$app_strava == "Other App"),]
participants_using_strava_competition <- participants_using_strava[participants_using_strava$group == "Competition",]
participants_using_strava_collaboration <- participants_using_strava[participants_using_strava$group == "Collaboration",]




# T Test   users and no users of applications for cycling
t.test(participants_using_app$satisfaction_before, participants_not_using_app$satisfaction_before)
t.test(participants_using_app$satisfaction_after, participants_not_using_app$satisfaction_after)
t.test(participants_using_app$satisfaction_before, participants_using_app$satisfaction_after)
t.test(participants_not_using_app$satisfaction_before, participants_not_using_app$satisfaction_after)
t.test(participants_using_app$engagement_before, participants_using_app$engagement_after)
t.test(participants_not_using_app$engagement_before, participants_not_using_app$engagement_after)
t.test(participants_not_using_app$satisfaction_after, participants_not_using_app$satisfaction_experiment) #
t.test(participants_using_app$satisfaction_after, participants_using_app$satisfaction_experiment)  #
t.test(participants_using_app$satisfaction_experiment, participants_not_using_app$satisfaction_experiment)
# T Test    users of applications competition and collaboration groups
t.test(participants_using_app_competition$satisfaction_before, participants_using_app_competition$satisfaction_after)
t.test(participants_using_app_collaboration$satisfaction_before, participants_using_app_collaboration$satisfaction_after)
t.test(participants_using_app_competition$engagement_before, participants_using_app_competition$engagement_after)
t.test(participants_using_app_collaboration$engagement_before, participants_using_app_collaboration$engagement_after)
t.test(participants_using_app_competition$satisfaction_after, participants_using_app_competition$satisfaction_experiment) #
t.test(participants_using_app_collaboration$satisfaction_after, participants_using_app_collaboration$satisfaction_experiment)
t.test(participants_using_app_competition$satisfaction_before, participants_using_app_collaboration$satisfaction_before)
t.test(participants_using_app_competition$satisfaction_after, participants_using_app_collaboration$satisfaction_after)
t.test(participants_using_app_competition$satisfaction_experiment, participants_using_app_collaboration$satisfaction_experiment)
t.test(participants_using_app_competition$engagement_after, participants_using_app_collaboration$engagement_after)
t.test(participants_using_app_competition$engagement_before, participants_using_app_collaboration$engagement_before)
# T Test    not users of applications competition and collaboration groups
t.test(participants_not_using_app_competitiion$satisfaction_before, participants_not_using_app_competitiion$satisfaction_after)
t.test(participants_not_using_app_collaboration$satisfaction_before, participants_not_using_app_collaboration$satisfaction_after)
t.test(participants_not_using_app_competitiion$engagement_before, participants_not_using_app_competitiion$engagement_after)
t.test(participants_not_using_app_collaboration$engagement_before, participants_not_using_app_collaboration$engagement_after)
t.test(participants_not_using_app_competitiion$satisfaction_after, participants_not_using_app_competitiion$satisfaction_experiment)
t.test(participants_not_using_app_collaboration$satisfaction_after, participants_not_using_app_collaboration$satisfaction_experiment) #
t.test(participants_not_using_app_competitiion$satisfaction_before, participants_not_using_app_collaboration$satisfaction_before)
t.test(participants_not_using_app_competitiion$satisfaction_after, participants_not_using_app_collaboration$satisfaction_after)
t.test(participants_not_using_app_competitiion$satisfaction_experiment, participants_not_using_app_collaboration$satisfaction_experiment)
t.test(participants_not_using_app_competitiion$engagement_before, participants_not_using_app_collaboration$engagement_before)
t.test(participants_not_using_app_competitiion$engagement_after, participants_not_using_app_collaboration$engagement_after)
# T Test    users of strava competition and collaboration groups
t.test(participants_using_strava$satisfaction_before, participants_using_strava$satisfaction_after)
t.test(participants_using_strava$engagement_before, participants_using_strava$engagement_after)
t.test(participants_using_strava$satisfaction_after, participants_using_strava$satisfaction_experiment)#
t.test(participants_using_app_nostrava$satisfaction_before, participants_using_app_nostrava$satisfaction_after)
t.test(participants_using_app_nostrava$engagement_before, participants_using_app_nostrava$engagement_after)
t.test(participants_using_app_nostrava$satisfaction_after, participants_using_app_nostrava$satisfaction_experiment)
t.test(participants_using_strava$satisfaction_experiment, participants_using_app_nostrava$satisfaction_experiment)#
t.test(participants_using_strava$satisfaction_after, participants_using_app_nostrava$satisfaction_after)
t.test(participants_notusing_strava$satisfaction_after, participants_using_strava$satisfaction_after) #
t.test(participants_notusing_strava$satisfaction_before, participants_using_strava$satisfaction_before) #
t.test(participants_notusing_strava$engagement_before, participants_using_strava$engagement_before)#
t.test(participants_notusing_strava$engagement_after, participants_using_strava$engagement_after)#
t.test(participants_notusing_strava$satisfaction_experiment, participants_using_strava$satisfaction_experiment)#
t.test(participants_using_strava_competition$satisfaction_before, participants_using_strava_competition$satisfaction_after)
t.test(participants_using_strava_competition$satisfaction_after, participants_using_strava_competition$satisfaction_experiment) #
t.test(participants_using_strava_collaboration$satisfaction_before, participants_using_strava_collaboration$satisfaction_after)
t.test(participants_using_strava_collaboration$satisfaction_after, participants_using_strava_collaboration$satisfaction_experiment)



# Effect size - Cohen's effect size
cohen.d(participants_using_app$satisfaction_before, participants_not_using_app$satisfaction_before)
cohen.d(participants_using_app$satisfaction_after, participants_not_using_app$satisfaction_after)
cohen.d(participants_using_app$satisfaction_before, participants_using_app$satisfaction_after)
cohen.d(participants_not_using_app$satisfaction_before, participants_not_using_app$satisfaction_after)
cohen.d(participants_using_app$satisfaction_after, participants_using_app$satisfaction_experiment)
cohen.d(participants_using_app$satisfaction_experiment, participants_not_using_app$satisfaction_experiment)
cohen.d(participants_using_strava$satisfaction_experiment, participants_using_app_nostrava$satisfaction_experiment) #
cohen.d(participants_notusing_strava$satisfaction_after, participants_using_strava$satisfaction_after) #
cohen.d(participants_notusing_strava$satisfaction_before, participants_using_strava$satisfaction_before) #
cohen.d(participants_notusing_strava$engagement_before, participants_using_strava$engagement_before) #
cohen.d(participants_notusing_strava$engagement_after, participants_using_strava$engagement_after) #
cohen.d()) #



# Boxplot  
p_part_app <- ggplot(data = participants_mobileapp, aes(y = satisfaction_experiment, x = app_cycling, fill = app_cycling)) 
p_part_strava <- ggplot(data = participants_mobileapp, aes(y = engagement_before, x = app_strava, fill = app_strava)) 


# Boxplot comparizon of the two groups
p_part_app + geom_boxplot() 
p_part_strava + geom_boxplot()
p_sat_strava + geom_boxplot()
p_sat_strava_group + geom_boxplot()
p_sat_nostrava_group + geom_boxplot()


p_enjoyment_1 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_2 + geom_boxplot() + facet_grid(. ~ city)
p_enjoyment_3 + geom_boxplot() + facet_grid(. ~ city)




