# Description: This script generates the graphs for anlyzing 
# participants answers in the different cities that the 
# experimet was deployed
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup

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












# Satisfaction grouped by city
satis_per_city <- data.frame(satis_after_factor)
title <- "Satisfaction with cycling"
satis_per_city_likert <- likert(satis_per_city, grouping = table_answers$City)
plot(satis_per_city_likert) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))






# 
=======
  # 
  
  >>>>>>> c8e271916325f3c4f735fa50bb7566d689b275e2
