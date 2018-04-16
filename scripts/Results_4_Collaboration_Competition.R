# Description: This script generates the graphs and tests to compare 
# participants answers about their perception of competition 
# and collaboration when using the bicycle.
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup
require(likert)

table_answers = read.csv('data/Questionnaire_Answers.csv')
likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
likert_factors <- factor(likert_values)


# Getting participants group
participants_group <- ifelse(table_answers$competition_1 >= -3, "Competition")
participants_group <- ifelse(!is.na(participants_group) & table_answers$collaboration_1 >= -3, "Collaboration",participants_group)

compet1_factor <- factor(table_answers$competition_1, likert_factors, labels = likert_labels)
collab1_factor <- factor(table_answers$collaboration_1, likert_factors, labels = likert_labels)
compet2_factor <- factor(table_answers$competition_2, likert_factors, labels = likert_labels)
collab2_factor <- factor(table_answers$collaboration_2, likert_factors, labels = likert_labels)
compet3_factor <- factor(table_answers$competition_3, likert_factors, labels = likert_labels)
collab3_factor <- factor(table_answers$collaboration_3, likert_factors, labels = likert_labels)


# Graph number 1
# Comparison 1: Competition Vs. Collaboration
comparison1_comp_coll <- data.frame(compet1_factor, collab1_factor)
title <- "'I found ... other cyclists enjoyable'"
names(comparison1_comp_coll) <- c("Competing against", "Collaborating with")
comparison1_likert <- likert(comparison1_comp_coll)
plot(comparison1_likert, type = 'density')
plot(comparison1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))