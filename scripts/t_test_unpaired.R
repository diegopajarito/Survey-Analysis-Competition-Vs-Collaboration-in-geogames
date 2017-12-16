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
