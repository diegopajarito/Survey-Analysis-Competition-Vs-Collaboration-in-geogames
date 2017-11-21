# Description: This script generates the graphs for anlyzing 
# participants answers in the different cities that the 
# experimet was deployed
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
participants_group <- ifelse(is.na(participants_group) & table_answers$collaboration_1 >= -3, "Collaboration",participants_group)
participants_group <- ifelse(is.na(participants_group), "None",participants_group)


satisfaction_factor <- factor(table_answers$profile_cycling_1, likert_factors, labels = likert_labels)
engagement_factor <- factor(table_answers$engagement_A1, likert_factors,labels = likert_labels)
competition_collaboratio <- ifelse$competition_1 + table_answers$collaboration_1
compet1_factor <- factor(table_answers$competition_1, likert_factors, labels = likert_labels)
collab1_factor <- factor(table_answers$collaboration_1, likert_factors, labels = likert_labels)
compet2_factor <- factor(table_answers$competition_2, likert_factors, labels = likert_labels)
collab2_factor <- factor(table_answers$collaboration_2, likert_factors, labels = likert_labels)
compet3_factor <- factor(table_answers$competition_3, likert_factors, labels = likert_labels)
collab3_factor <- factor(table_answers$collaboration_3, likert_factors, labels = likert_labels)



# Graph number 1
# Comparison 1: Satisfaction with cycling at each city
satifaction_city <- data.frame(satisfaction_factor)
title <- "'I would find cycling enjoyable'"
satisfaction_city_likert <- likert(satifaction_city, grouping = table_answers$City)
plot(satisfaction_city_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/D_Comparison_graph1.svg", 
    width=6.5, height=3, pointsize=10)
plot(satisfaction_city_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()





# Graph number 2
# Comparison 2: Engagement with cycling at each city
engagement_city <- data.frame(engagement_factor)
title <- "'My intention to use a bike is'"
engagement_city_likert <- likert(engagement_city, grouping = table_answers$City)
plot(engagement_city_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/D_Comparison_graph2.svg", 
    width=6.5, height=3, pointsize=10)
plot(engagement_city_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()





# Graphy number 3
# Comparison 3: Enjoyment with competition at each city 
comparison1_comp_coll <- data.frame(compet1_factor, collab1_factor)
names(comparison1_comp_coll) <- c("'I found competing against other cyclists enjoyable'", 
                                  "'I found collaborating with other cyclists enjoyable'")
comparison1_likert <- likert(comparison1_comp_coll, grouping = table_answers$City )
plot(comparison1_likert)
plot(comparison1_likert, centered = TRUE) +
  guides(fill=guide_legend(title=NULL, nrow = 1))

svg(filename="graphs/C_Comparison_graph1.svg", 
    width=6.5, height=6, pointsize=10)
plot(comparison1_likert, centered = TRUE) +
  guides(fill=guide_legend(title=NULL, nrow = 1))
dev.off()







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









