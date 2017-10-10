# Description: Graphs for the use of mobile phone section
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)

table_answers = read.csv('data/Questionnaire_Answers.csv')

likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree"," ","  ","Neutral","   ","    ","Strongly agree")
likert_factors <- factor(likert_values)


satis_before_factor <- factor(table_answers$profile_cycling_1, likert_factors, labels = likert_labels)
satis_after_factor <- factor(table_answers$satisfaction_cycling, likert_factors, labels = likert_labels)

satis_factors <- data.frame(satis_before_factor, satis_after_factor)

names(satis_factors) <- c("With Cycling - Before", "With Cycling - After")
satis_likert <- likert(satis_factors)
plot(satis_likert)

pdf("graphs/sastisfaction.pdf")
plot(satis_likert)
dev.off()




