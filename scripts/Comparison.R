# Description: Graphs for the use of mobile phone section
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

require(likert)

table_answers = read.csv('data/Questionnaire_Answers_temp.csv')

likert_values <- c(-3,-2,-1,0,1,2,3)
<<<<<<< HEAD
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","(0)","(1)","(2)","Strongly agree (3)")
=======
likert_labels <- c("Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
>>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
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



# Comparison 1: Competition Vs. Collaboration
comparison1_comp_coll <- data.frame(compet1_factor, collab1_factor)
<<<<<<< HEAD
title <- "To which extent do you consider these activities enjoyable"
=======
title <- "To which extent do you aggree or dissagree that these activities are enjoyable"
>>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
names(comparison1_comp_coll) <- c("Competing", "Collaborating")
comparison1_likert <- likert(comparison1_comp_coll)
plot(comparison1_likert, type = 'density')
plot(comparison1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
<<<<<<< HEAD

pdf("graphs/comparison.pdf")
plot(comparison1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()


# Comparison 2: Checking contribution Vs. Progress
comparison2_comp_coll <- data.frame(compet2_factor, collab2_factor)
title <- "To which extent do you aggree or dissagree with these activities are enjoyable"
names(comparison2_comp_coll) <- c("Checking my own progress", "Checking my contribution")
comparison2_likert <- likert(comparison2_comp_coll)
plot(comparison2_likert, type = 'density')
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

pdf("graphs/comparison.pdf")
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
=======

pdf("graphs/comparison.pdf")
plot(comparison1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
>>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()


<<<<<<< HEAD


=======
# Comparison 2: Checking contribution Vs. Progress
comparison2_comp_coll <- data.frame(compet2_factor, collab2_factor)
title <- "To which extent do you aggree or dissagree with these activities are enjoyable"
names(comparison2_comp_coll) <- c("Checking my own progress", "Checking my contribution")
comparison2_likert <- likert(comparison2_comp_coll)
plot(comparison2_likert, type = 'density')
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

pdf("graphs/comparison.pdf")
plot(comparison2_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()




>>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
# Comparison 3: Checking leader board
comparison3_comp_coll <- data.frame(compet3_factor, collab3_factor)
title <- "To which extent do you aggree or dissagree with these activities are enjoyable"
names(comparison3_comp_coll) <- c("Enjoyed", "Wanted")
comparison3_likert <- likert(comparison3_comp_coll)
plot(comparison3_likert, type = 'density')
plot(comparison3_likert, centered = TRUE) + ggtitle(title) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

pdf("graphs/comparison.pdf")
plot(comparison3_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()









<<<<<<< HEAD
# Satisfaction per group 

# Getting participants group
likert_values <- c("None", "Collaboration","Competition",-3,-2,-1,0,1,2,3)
likert_labels <- c("None", "Collaboration","Competition","Strongly disagree (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Strongly agree (3)")
likert_factors <- factor(likert_values)

participants_group <- ifelse(table_answers$competition_1 >= -3, "Competition")
participants_group <- ifelse(is.na(participants_group) & table_answers$collaboration_1 >= -3, "Collaboration",participants_group)
participants_group <- ifelse(is.na(participants_group), "None",participants_group)
participants_group_factor <- factor(participants_group, likert_factors, labels = likert_labels)



# Satisfaction before 
satisfaction_cycling_before <- factor(table_answers$profile_cycling_1, likert_factors, labels = likert_labels)

satisfaction_competition_collaboration <- data.frame (satisfaction_cycling_before, participants_group_factor)
satisfaction_competition_collaboration_likert <- likert(satisfaction_competition_collaboration, grouping = participants_group_factor)
plot(satisfaction_competition_collaboration_likert)


# Satisfaction after
satisfaction_cycling_after <- factor(table_answers$satisfaction_app_1, likert_factors, labels = likert_labels)

satisfaction_competition_collaboration_after <- data.frame(satisfaction_cycling_after, participants_group_factor)
satisfaction_competition_collaboration_after_likert <- likert(satisfaction_competition_collaboration_after, grouping = participants_group_factor)
plot(satisfaction_competition_collaboration_after_likert)




# Chages in Satisfaction with cycling
likert_values_change <- c(-2,-1,0,1,2)
likert_labels_change <- c('Less (-2)', 'Less (-1)', 'Equal (0)', 'More (+1)', 'More (+2)')
change_satisfaction <- table_answers$satisfaction_cycling - table_answers$profile_cycling_1
change_satisfaction_factor <-  data.frame( factor(change_satisfaction,likert_values_change, labels = likert_labels_change) )
names(change_satisfaction_factor) <- c("Change in Satisfaction")
change_satisfaction_likert <- likert(change_satisfaction_factor)
plot(change_satisfaction_likert)




change_engagement_cycling <- table_answers$eng

engagemet cycling - engagement during the experiment
engagement after - engagement during the experiment






# 
=======
# 

>>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
