# Description: Graphs for the use of mobile phone section
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)

table_answers = read.csv('data/Questionnaire_Answers.csv')

likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
likert_factors <- factor(likert_values)


# Satisfaction before and After
satis_before_factor <- factor(table_answers$profile_cycling_1, likert_factors, labels = likert_labels )
satis_after_factor <- factor(table_answers$satisfaction_cycling, likert_factors, labels = likert_labels )

satis_factors <- data.frame( satis_after_factor, satis_before_factor)

names(satis_factors) <- c("After", "Before")
title <- "I would find cycling enjoyable'"
satis_likert <- likert(satis_factors)
plot(satis_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))


pdf("graphs/sastisfaction.pdf")
plot(satis_likert)
dev.off()


# Satisfaction with cycling / Mobile App during the experiment 
satis_cycling_during_ex_factor <- factor(table_answers$satisfaction_app_1, likert_factors, labels = likert_labels)
satis_app_during_ex_factor <- factor(table_answers$satisfaction_app_2, likert_factors, labels = likert_labels)
satis_cycling_during_ex <- data.frame(satis_app_during_ex_factor, satis_cycling_during_ex_factor)
names
title <- "How satisfied / dissatisfied during the experimetn you were with:"
names(satis_cycling_during_ex) <- c("Cycling", "The Mobile App")
satis_cycling_during_ex_likert <- likert(satis_cycling_during_ex)
plot(satis_cycling_during_ex_likert, centered = FALSE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))



# Satisfaction with the App during the experiment




