# Description: This script generates the graphs for anlyzing 
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

svg(filename="graphs/C_Comparison_graph1.svg", 
    width=9, height=2.7, pointsize=10)
plot(comparison1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1)) +
  theme(legend.position = "none")
dev.off()


# Graph number 2
# Comparison 2: Checking contribution Vs. Progress
comparison2_comp_coll <- data.frame(compet2_factor, collab2_factor)
title <- "'I found checking ... enjoyable'"
names(comparison2_comp_coll) <- c("my own progress", "my contribution")
comparison2_likert <- likert(comparison2_comp_coll)
plot(comparison2_likert, type = 'density')
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/C_Comparison_graph2.svg", 
    width=6.5, height=2.5, pointsize=10)
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()
