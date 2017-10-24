# Description: Graphs for the use of mobile phone section
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)

table_answers = read.csv('data/Questionnaire_Answers.csv')

likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Very weak (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Very strong (3)")
likert_factors <- factor(likert_values)


compet1_factor <- factor(table_answers$competition_1, likert_factors, labels = likert_labels)
collab1_factor <- factor(table_answers$collaboration_1, likert_factors, labels = likert_labels)
compet2_factor <- factor(table_answers$competition_2, likert_factors, labels = likert_labels)
collab2_factor <- factor(table_answers$collaboration_2, likert_factors, labels = likert_labels)
compet3_factor <- factor(table_answers$competition_3, likert_factors, labels = likert_labels)
collab3_factor <- factor(table_answers$collaboration_3, likert_factors, labels = likert_labels)

comparison1_factors <- data.frame(compet1_factor, collab1_factor)

names(comparison1_factors) <- c("I found competing against other cyclists enjoyable", "I found collaborating with other cyclists enjoyable")
comparison1_likert <- likert(comparison1_factors)
plot(comparison1_likert)

pdf("graphs/comparison.pdf")
plot(comparison1_likert, centered = FALSE) + ggtitle("title")
dev.off()


comparison_factors <- data.frame(compet1_factor,collab1_factor,compet2_factor,collab2_factor,compet3_factor,collab3_factor)
names(comparison_factors) <- c("I found competing against other cyclists enjoyable", "I found collaborating with other cyclists enjoyable","I enjoyed checking my own progress with the app","I enjoyed checking my contribution with the app","I enjoyed checking my position on the leaderboard","I wanted to check my position on a leaderboard")
comparison_likert <- likert(comparison_factors)
plot(comparison_likert)


pdf("graphs/comparison_full.pdf")
plot(comparison_likert)
dev.off()


