# Description: This script creates graphs of the cycling profile of participants
# it groups the 
# groups
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)


# Setup 
source("scripts/setup.R")



cycling_profile <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$profile_cycling_1, table_answers$profile_cycling_2, table_answers$profile_cycling_3,
                              table_answers$profile_cycling_4, table_answers$profile_cycling_5, table_answers$profile_cycling_6, table_answers$profile_cycling_7, table_answers$profile_cycling_8,
                              table_answers$profile_cycling_9, table_answers$profile_cycling_10, table_answers$profile_cycling_11, table_answers$profile_cycling_12, table_answers$profile_cycling_13,
                              table_answers$profile_cycling_14, table_answers$profile_cycling_15)
names(cycling_profile) <- c("participant", "city", "group", "profile_cycling_1", "profile_cycling_2", "profile_cycling_3", "profile_cycling_4", "profile_cycling_5", "profile_cycling_6",
                            "profile_cycling_7", "profile_cycling_8", "profile_cycling_9", "profile_cycling_10", "profile_cycling_11", "profile_cycling_12", "profile_cycling_13", 
                            "profile_cycling_14", "profile_cycling_15")
likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
likert_factors <- factor(likert_values)


# Cycling profile questions related to cycling infrastructure
profile_5_factor <- factor(cycling_profile$profile_cycling_5, likert_factors,labels = likert_labels)
profile_8_factor <- factor(cycling_profile$profile_cycling_8, likert_factors,labels = likert_labels)
profile_10_factor <- factor(cycling_profile$profile_cycling_10, likert_factors,labels = likert_labels)
profile_13_factor <- factor(cycling_profile$profile_cycling_13, likert_factors,labels = likert_labels)
profile_14_factor <- factor(cycling_profile$profile_cycling_14, likert_factors,labels = likert_labels)
profile_15_factor <- factor(cycling_profile$profile_cycling_15, likert_factors,labels = likert_labels)
profile_infratructure_factors <- data.frame(profile_5_factor, profile_8_factor, profile_10_factor, profile_13_factor, profile_14_factor, profile_15_factor)
names(profile_infratructure_factors) <- c("More cycle lanes would make me feel safer", "It would be a bad experience using the existing roads", "It would be too much physical effort", "It would take me too long", "It would put my bike at risk of being stolen while is parked", "It would mean I have to negotiate difficult road junctions")
title <- "Perception of infrastructrue "
profile_infrastructure_likert <- likert(profile_infratructure_factors )
plot(profile_infrastructure_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))


# Cycling profile questions related to cycling environment
profile_3_factor <- factor(cycling_profile$profile_cycling_3, likert_factors,labels = likert_labels)
profile_9_factor <- factor(cycling_profile$profile_cycling_9, likert_factors,labels = likert_labels)
profile_11_factor <- factor(cycling_profile$profile_cycling_11, likert_factors,labels = likert_labels)
profile_12_factor <- factor(cycling_profile$profile_cycling_12, likert_factors,labels = likert_labels)
profile_environment_factors <- data.frame(profile_3_factor, profile_9_factor, profile_11_factor, profile_12_factor)
names(profile_environment_factors) <- c("I would feel part of my community", "It would mean 'I contribute less to climate change'", "It would more than likely expose me to wet or windy weather", "It would mean 'I contribute less to local air pollution'")
title <- "Perception of environment"
profile_environment_likert <- likert(profile_environment_factors )
plot(profile_environment_likert, centered = TRUE) + ggtitle("") + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

