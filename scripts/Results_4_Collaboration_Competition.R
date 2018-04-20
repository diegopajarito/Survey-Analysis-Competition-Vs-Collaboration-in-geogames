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


# Getting participants city
competition_all_factor <- factor(table_answers$competition_1, likert_factors, labels = likert_labels)
collaboration_all_factor <- factor(table_answers$collaboration_1, likert_factors, labels = likert_labels)
competition_ms_factor <- factor(table_answers[table_answers$City == 'Münster',]$competition_1, likert_factors, labels = likert_labels)
collaboration_ms_factor <- factor(table_answers[table_answers$City == 'Münster',]$collaboration_1, likert_factors, labels = likert_labels)
competition_cs_factor <- factor(table_answers[table_answers$City == 'Castelló',]$competition_1, likert_factors, labels = likert_labels)
collaboration_cs_factor <- factor(table_answers[table_answers$City == 'Castelló',]$collaboration_1, likert_factors, labels = likert_labels)
competition_mt_factor <- factor(table_answers[table_answers$City == 'Malta',]$competition_1, likert_factors, labels = likert_labels)
collaboration_mt_factor <- factor(table_answers[table_answers$City == 'Malta',]$collaboration_1, likert_factors, labels = likert_labels)


# Graph number 1
# Comparison 1: Competition Vs. Collaboration
title_all <- "'I found ... other cyclists enjoyable'"
title_ms <- "Münster"
title_cs <- "Castelló"
title_mt <- "Valletta"
comparison_all <- data.frame(competition_all_factor, collaboration_all_factor)
comparison_ms <- data.frame(competition_ms_factor, collaboration_ms_factor)
comparison_cs <- data.frame(competition_cs_factor, collaboration_cs_factor)
comparison_mt <- data.frame(competition_mt_factor, collaboration_mt_factor)
names(comparison_all) <- c("Competing against", "Collaborating with")
names(comparison_ms) <- c("Competing against", "Collaborating with")
names(comparison_cs) <- c("Competing against", "Collaborating with")
names(comparison_mt) <- c("Competing against", "Collaborating with")
comparison_all_likert <- likert(comparison_all)
comparison_ms_likert <- likert(comparison_ms)
comparison_cs_likert <- likert(comparison_cs)
comparison_mt_likert <- likert(comparison_mt)

# The four graphs must be intergated manually from the Vector Files
svg(filename="graphs/Results_4_all.svg", width=7.50, height=2.50)
plot(comparison_all_likert, centered = TRUE) + ggtitle(title_all) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()
svg(filename="graphs/Results_4_ms.svg", width=7.50, height=2.50)
plot(comparison_ms_likert, centered = TRUE) + ggtitle(title_ms) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()
svg(filename="graphs/Results_4_cs.svg", width=7.50, height=2.50)
plot(comparison_cs_likert, centered = TRUE) + ggtitle(title_cs) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()
svg(filename="graphs/Results_4_mt.svg", width=7.50, height=2.50)
plot(comparison_mt_likert, centered = TRUE) + ggtitle(title_mt) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()
