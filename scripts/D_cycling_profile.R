# Description: This script creates graphs of the cycling profile of participants
# it groups the 
# groups
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)
library(plyr)
library(ddply)

# Setup 
source("scripts/setup.R")

cycling_profile <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$profile_cycling_1, table_answers$profile_cycling_2, table_answers$profile_cycling_3,
                              table_answers$profile_cycling_4, table_answers$profile_cycling_5, table_answers$profile_cycling_6, table_answers$profile_cycling_7, table_answers$profile_cycling_8,
                              table_answers$profile_cycling_9, table_answers$profile_cycling_10, table_answers$profile_cycling_11, table_answers$profile_cycling_12, table_answers$profile_cycling_13,
                              table_answers$profile_cycling_14, table_answers$profile_cycling_15, table_answers$satisfaction_1)
names(cycling_profile) <- c("participant", "city", "group", "profile_cycling_1", "profile_cycling_2", "profile_cycling_3", "profile_cycling_4", "profile_cycling_5", "profile_cycling_6",
                            "profile_cycling_7", "profile_cycling_8", "profile_cycling_9", "profile_cycling_10", "profile_cycling_11", "profile_cycling_12", "profile_cycling_13", 
                            "profile_cycling_14", "profile_cycling_15", "satisfaction_experiment")
likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree","Disagree","(-)","Neutral","(+)","Agree","Strongly agree")
likert_factors <- factor(likert_values)

# Setting up satisfaction with cycling 
cycling_profile$satisfaction <- cycling_profile$profile_cycling_1
cycling_profile[cycling_profile$profile_cycling_1 > 1 & !is.na(cycling_profile$profile_cycling_1),]$satisfaction <- 2
cycling_profile[cycling_profile$profile_cycling_1 < 2 & !is.na(cycling_profile$profile_cycling_1),]$satisfaction <- 0
cycling_profile[cycling_profile$satisfaction_experiment > 1 & !is.na(cycling_profile$satisfaction_experiment),]$satisfaction_experiment <- 2
cycling_profile[cycling_profile$satisfaction_experiment < 2 & !is.na(cycling_profile$satisfaction_experiment),]$satisfaction_experiment <- 0

# Setting up answers as factors
profile_1_factor <- factor(cycling_profile$profile_cycling_1, likert_factors,labels = likert_labels)
profile_5_factor <- factor(cycling_profile$profile_cycling_5, likert_factors,labels = likert_labels)
profile_8_factor <- factor(cycling_profile$profile_cycling_8, likert_factors,labels = likert_labels)
profile_10_factor <- factor(cycling_profile$profile_cycling_10, likert_factors,labels = likert_labels)
profile_13_factor <- factor(cycling_profile$profile_cycling_13, likert_factors,labels = likert_labels)
profile_14_factor <- factor(cycling_profile$profile_cycling_14, likert_factors,labels = likert_labels)
profile_15_factor <- factor(cycling_profile$profile_cycling_15, likert_factors,labels = likert_labels)
satisfaction_factor <- factor(cycling_profile$satisfaction, likert_factors,labels = likert_labels)
satisfaction_experiment_factor <- factor(cycling_profile$satisfaction_experiment, likert_factors, labels = likert_labels)


profile_infratructure_factors <- data.frame(satisfaction_factor, satisfaction_experiment_factor, profile_5_factor, profile_8_factor, profile_10_factor, profile_13_factor, profile_14_factor, profile_15_factor)
names(profile_infratructure_factors) <- c("satisfaction", "satisfaction_experiment", "More cycle lanes would make me feel safer", "It would be a bad experience using the existing roads", "It would be too much physical effort", "It would take me too long", "It would put my bike at risk of being stolen while is parked", "It would mean I have to negotiate difficult road junctions")
profile_infratructure_factors <-profile_infratructure_factors[complete.cases(profile_infratructure_factors),]
title <- "Perception of infrastructrue "
profile_infrastructure_likert <- likert(profile_infratructure_factors[,c(3:8)], grouping = profile_infratructure_factors$satisfaction)
plot(profile_infrastructure_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1)) 
plot(profile_infrastructure_likert, type = "density")
plot(profile_infrastructure_likert)

profile_infrastructure_likert <- likert(profile_infratructure_factors[,c(3:8)])
plot(profile_infrastructure_likert) +
  guides(fill=guide_legend(title=NULL, nrow = 1)) 

profile_infrastructure_experiment_likert <- likert(profile_infratructure_factors[,c(3:8)], grouping = profile_infratructure_factors$satisfaction_experiment)
plot(profile_infrastructure_experiment_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1)) 
plot(profile_infrastructure_likert, type = "density")
plot(profile_infrastructure_likert)




# Cycling profile questions related to cycling environment
profile_2_factor <- factor(cycling_profile$profile_cycling_2, likert_factors,labels = likert_labels)
profile_3_factor <- factor(cycling_profile$profile_cycling_3, likert_factors,labels = likert_labels)
profile_4_factor <- factor(cycling_profile$profile_cycling_4, likert_factors,labels = likert_labels)
profile_6_factor <- factor(cycling_profile$profile_cycling_6, likert_factors,labels = likert_labels)
profile_9_factor <- factor(cycling_profile$profile_cycling_9, likert_factors,labels = likert_labels)
profile_11_factor <- factor(cycling_profile$profile_cycling_11, likert_factors,labels = likert_labels)
profile_12_factor <- factor(cycling_profile$profile_cycling_12, likert_factors,labels = likert_labels)
profile_environment_factors <- data.frame(satisfaction_factor, satisfaction_experiment_factor, profile_2_factor, profile_3_factor, profile_4_factor, profile_6_factor, profile_9_factor, profile_11_factor, profile_12_factor)
names(profile_environment_factors) <- c("satisfaction", "satisfaction_experiment", "I would get a sense of freedom", "I would feel part of my community", "I would find it relaxing", "It would benefit my health", "It would mean 'I contribute less to climate change'", "It would more than likely expose me to wet or windy weather", "It would mean 'I contribute less to local air pollution'")
profile_environment_factors <- profile_environment_factors[complete.cases(profile_environment_factors),]
title <- "Perception of environment"
profile_environment_likert <- likert(profile_environment_factors[,c(3:9)], grouping = profile_environment_factors$satisfaction)
plot(profile_environment_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))
plot(profile_environment_likert, type = "density")
plot(profile_environment_likert)

profile_environment_likert <- likert(profile_environment_factors[,c(3:9)], grouping = profile_environment_factors$satisfaction_experiment)
plot(profile_environment_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))
plot(profile_environment_likert, type = "density")
plot(profile_environment_likert)





# Cyclist profile per city based on questionnaire

profile_questions <- c("Question",
  "I would find cycling enjoyable",
  "I would get a sense of freedom",
  "I would feel part of my community",
  "I would find it relaxing",
  "More cycle lanes would make me feel safer",
  "It would benefit my health",
  "I would save me money",
  "It would be a bad experience using the existing roads",
  "It would mean 'I contribute less to climate change'",
  "It would be too much physical effort",
  "It would more than likely expose me to wet or windy weather",
  "It would mean 'I contribute less to local air pollution'",
  "It would take me too long",
  "It would put my bike at risk of being stolen whilst parked",
  "It would mean I have to negotiate difficult road junctions")

profile_city <- ddply(cycling_profile, ~ city, summarize, 
      q1 = round(mean(profile_cycling_1, na.rm = TRUE), digits = 1), 
      q2 = round(mean(profile_cycling_2, na.rm = TRUE), digits = 1),
      q3 = round(mean(profile_cycling_3, na.rm = TRUE), digits = 1),
      q4 = round(mean(profile_cycling_4, na.rm = TRUE), digits = 1),
      q5 = round(mean(profile_cycling_5, na.rm = TRUE), digits = 1),
      q6 = round(mean(profile_cycling_6, na.rm = TRUE), digits = 1),
      q7 = round(mean(profile_cycling_7, na.rm = TRUE), digits = 1),
      q8 = round(mean(profile_cycling_8, na.rm = TRUE), digits = 1),
      q9 = round(mean(profile_cycling_9, na.rm = TRUE), digits = 1),
      q10 = round(mean(profile_cycling_10, na.rm = TRUE), digits = 1),
      q11 = round(mean(profile_cycling_11, na.rm = TRUE), digits = 1),
      q12 = round(mean(profile_cycling_12, na.rm = TRUE), digits = 1),
      q13 = round(mean(profile_cycling_13, na.rm = TRUE), digits = 1),
      q14 = round(mean(profile_cycling_14, na.rm = TRUE), digits = 1),
      q15 = round(mean(profile_cycling_15, na.rm = TRUE), digits = 1)
      )

profile_city <- data.frame(profile_questions, t(profile_city))
