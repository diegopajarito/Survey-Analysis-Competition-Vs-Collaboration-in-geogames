# Description: This script generates the graphs for anlyzing 
# participants answers about Satisfaction with cycling.
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup 
require(likert)
table_answers = read.csv('data/Questionnaire_Answers.csv')
likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
likert_factors <- factor(likert_values)


# Graph number 1
# Satisfaction with cycling, comparison before and After the experiment
satis_before_factor <- factor(table_answers$profile_cycling_1, likert_factors, labels = likert_labels )
satis_after_factor <- factor(table_answers$satisfaction_cycling, likert_factors, labels = likert_labels )

satis_factors <- data.frame( satis_after_factor, satis_before_factor)

names(satis_factors) <- c("After", "Before")
title <- "'I would find cycling enjoyable'"
satis_likert <- likert(satis_factors)
plot(satis_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/A_sastisfaction_graph1.svg", 
    width=6.5, height=3, pointsize=10)
plot(satis_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()



# Graph number 2
# Satisfaction with cycling / Mobile App during the experiment 
likert_labels_satisfaction <- c("Very dissatisfied (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Very satisfied (3)")
satis_cycling_during_ex_factor <- factor(table_answers$satisfaction_app_1, likert_factors, labels = likert_labels_satisfaction)
satis_app_during_ex_factor <- factor(table_answers$satisfaction_app_2, likert_factors, labels = likert_labels_satisfaction)
satis_cycling_during_ex <- data.frame(satis_app_during_ex_factor, satis_cycling_during_ex_factor)
names
title <- "During the experiment, how satisfied or dissatisfied were with:"
names(satis_cycling_during_ex) <- c("Cycling?", "The Mobile App?")
satis_cycling_during_ex_likert <- likert(satis_cycling_during_ex)
plot(satis_cycling_during_ex_likert) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/A_sastisfaction_graph2.svg", 
    width=6.5, height=3, pointsize=10)
plot(satis_cycling_during_ex_likert) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()

