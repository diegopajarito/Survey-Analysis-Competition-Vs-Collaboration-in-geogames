# Description: This script generates the graphs for anlyzing 
# participants answers about Engagement with cycling.
#
# Comments: set your working directory to the project folder
# Author: Diego Pajarito 

# Setup
require(likert)
rs = read.csv('data/Questionnaire_Answers.csv')
answers_engagement <- table_answers[,75:86]
likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Very weak (-3)","(-2)","(-1)","Neutral (0)","(1)","(2)","Very strong (3)")
likert_factors <- factor(likert_values)


# Graph number 1
# Engagement with cycling
eng1_before_factor <- factor(answers_engagement$engagement_A1, likert_factors,labels = likert_labels)
eng1_after_factor <- factor(answers_engagement$engagement_B1, likert_factors,labels = likert_labels)
eng1_factors <- data.frame(eng1_before_factor, eng1_after_factor)

names(eng1_factors) <- c("Before", "After")
title <- "'My intention to use a bike is'"
eng1_likert <- likert(eng1_factors)
plot(eng1_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))




# Comparison between cities
engagement_per_city = data.frame(eng1_before_factor)
title <- "Indicate how weak or strong is your intention to use a bicycle"
engagement_per_city_likert <- likert(engagement_per_city, grouping = table_answers$City)
plot(engagement_per_city_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))

# Engagement with games for mobile phones
eng2_before_factor <- factor(answers_engagement$engagement_A2, likert_factors,labels = likert_labels)
eng2_after_factor <- factor(answers_engagement$engagement_B2, likert_factors,labels = likert_labels)
eng2_factors <- data.frame(eng2_before_factor, eng2_after_factor)

title <- "Indicate how weak or strong is your intention to play some game on your phone"
names(eng2_factors) <- c("Before", "After")
eng2_likert <- likert(eng2_factors, sort(names(eng2_factors)))
plot(eng2_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))


# Engagement with app for cycling
eng3_before_factor <- factor(answers_engagement$engagement_A3, likert_factors,labels = likert_labels)
eng3_after_factor <- factor(answers_engagement$engagement_B3, likert_factors,labels = likert_labels)
eng3_factors <- data.frame(eng3_before_factor, eng3_after_factor)

title <- "How is your intention to use an app while cycling"
names(eng3_factors) <- c("Before", "After")
eng3_likert <- likert(eng3_factors)
plot(eng3_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill=guide_legend(title=NULL, nrow = 1))





# Engagement 
eng_factors <- data.frame(eng1_before_factor,eng1_after_factor, eng2_before_factor, eng2_after_factor, eng3_before_factor, eng3_after_factor)
names(eng_factors) <- c("With Cycling - Before", "With Cycling - After", "With Mobile Games - Before", "With Mobile Games - After", "With Aps for Cycling - Before", "With Aps for Cycling - After")

eng_likert <- likert(eng_factors)
plot(eng_likert)

pdf("graphs/engagement.pdf")
plot(eng_likert)
dev.off()






# Engagement with future cycling
eng_future_cycling_factor <- factor(answers_engagement$engagement_cycling_2w, likert_factors,labels = likert_labels)
eng_future_cycling2_factor <- factor(answers_engagement$engagement_cycling_future, likert_factors,labels = likert_labels)

eng_future_cycling_ex <- data.frame(eng_future_cycling_factor,eng_future_cycling2_factor)
names(eng_future_cycling_ex) <- c("in the next 2 weeks", "In the future")
title <- "I intend to bicycle"
eng_future_cycling_ex_likert <- likert(eng_future_cycling_ex)
plot (eng_future_cycling_ex_likert)
plot(eng_future_cycling_ex_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))







# Engagement with the experiment app
eng_future_app_factor <- factor(answers_engagement$engagement_app_2w, likert_factors,labels = likert_labels)
eng_future_app2_factor <- factor(answers_engagement$engagement_app_future, likert_factors,labels = likert_labels)

eng_future_app_ex <- data.frame(eng_future_app_factor, eng_future_app2_factor)
names(eng_future_app_ex) <- c("in the next 2 weeks", "In the future")
title <- "I intend to use the app of the experiment"
eng_future_app_likert <- likert(eng_future_app_ex)

plot(eng_future_app_likert)
plot(eng_future_app_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))



# Engagement with cycling apps
eng_future_any_app_factor <- factor(answers_engagement$engagement_any_app_2w, likert_factors,labels = likert_labels)
eng_future_any_app2_factor <- factor(answers_engagement$engagement_any_app_future, likert_factors,labels = likert_labels)

eng_future_any_app <- data.frame(eng_future_any_app_factor, eng_future_any_app2_factor)
names(eng_future_any_app) <- c("in the next 2 weeks", "In the future")
title <- "I intend to use the an app while cycling"
eng_future_any_app_likert <- likert(eng_future_any_app)
plot(eng_future_any_app_likert)
plot(eng_future_any_app_likert, centered = TRUE) + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=NULL, nrow = 1))





eng_future_cycling <- data.frame(eng_future_cycling_factor, eng_future_cycling2_factor, eng_future_app_factor, eng_future_app2_factor, eng_future_any_app_factor, eng_future_any_app2_factor)

names(eng_future_cycling) <- c("I intend to bicycle in the next 2 weeks","I intend to use a bicycle in the future", "I intend to use the given app in the next 2 weeks","I intend to use the given app in the future","I intend to use an app while cycling in the next 2 weeks","I intend to use an app while cycling in the future")
eng_future_cycling_likert <- likert(eng_future_cycling)
plot(eng_future_cycling_likert)

pdf("graphs/engagement_future.pdf")
plot(eng_future_cycling_likert)
dev.off()


#Engagement with the experiment
experim_experiment_factor <- factor(table_answers$engagement_1, likert_factors, labels = likert_labels)
experim_experiment_app_factor <- factor(table_answers$engagement_2, likert_factors, labels = likert_labels)

eng_experim <- data.frame(experim_experiment_factor, experim_experiment_app_factor)
names(eng_experim) <- c("cycling during the experiment","using the given app")
eng_experim_likert <- likert(eng_experim)
plot(eng_experim_likert)

pdf("graphs/engagement_experiment.pdf")
plot(eng_experim_likert)
dev.off()

