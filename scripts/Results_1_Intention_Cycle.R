# Description: This script evaluates the t-test for the answers of participants
# it compares the intentions to cycle between participants asigned to competition and colaboration
# groups
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

library(sjmisc)
library(dplyr)
library(knitr)
library(ggplot2)
library(Hmisc)
library(effsize)

# Setup 
source("scripts/setup.R")




# Un-paired test
# Satisfaction with cycling during the experiment. Comparison between groups using the collaboration-based / competitition-based version of the app
# Q1: “I ejoyed collaborating with / competing against other cyclists”

intention <- data.frame(table_answers$participant, table_answers$City, table_answers$group, table_answers$engagement_A1, table_answers$engagement_B1)
names(intention) <- c("participant", "city", "group", "intention_before", "intention_after")
intention_collaboration <- intention[intention$group == "Collaboration",]
intention_competition <- intention[intention$group == "Competition",]
intention_group <- rbind(intention_collaboration, intention_competition)


# Normality Test
shapiro.test(intention_collaboration$intention_before)
shapiro.test(intention_competition$intention_after)

#Variance Test
fligner.test(intention_collaboration$intention_before, intention_competition$intention_after)

#t-test
t.test(intention$intention_before, intention$intention_after)
# Wilcox Test
wilcox.test(intention$intention_before, intention$intention_after)
wilcox.test(intention_collaboration$intention_before, intention_competition$intention_before)
wilcox.test(intention_collaboration$intention_after, intention_competition$intention_after)
wilcox.test(intention_collaboration$intention_before, intention_collaboration$intention_after)
wilcox.test(intention_competition$intention_before, intention_competition$intention_after)

# Effect size - Cohen's effect size
cohen.d(intention$intention_before, intention$intention_after, na.rm = TRUE)


# Boxplot  
p_experiment <- ggplot(data = enjoyment, aes(x = group, y = enj_functionality)) 
p_experiment + geom_boxplot()
p_experiment + geom_boxplot() + facet_grid(. ~ city)

# Removing Outliers
out_collaboration <- boxplot.stats(enjoyment_collaboration$enj_functionality)$out
enjoyment_collaboration$enj_functionality <- ifelse(enjoyment_collaboration$enj_functionality %in% out_collaboration, NA, enjoyment_collaboration$enj_functionality)
out_competition <- boxplot.stats(enjoyment_competition$enj_functionality)$out
enjoyment_competition$enj_functionality <- ifelse(enjoyment_competition$enj_functionality %in% out_competition, NA, enjoyment_competition$enj_functionality)
enjoyment_group <- rbind(enjoyment_collaboration, enjoyment_competition)

# Wilcox Test without outliers
wilcox.test(enjoyment_collaboration$enj_functionality, enjoyment_competition$enj_functionality)
wilcox.test(enjoyment_collaboration[enjoyment_collaboration$city == "Castelló",]$enj_functionality, enjoyment_competition[enjoyment_competition$city == "Castelló",]$enj_functionality)
wilcox.test(enjoyment_collaboration[enjoyment_collaboration$city == "Malta",]$enj_functionality, enjoyment_competition[enjoyment_competition$city == "Malta",]$enj_functionality)
wilcox.test(enjoyment_collaboration[enjoyment_collaboration$city == "Münster",]$enj_functionality, enjoyment_competition[enjoyment_competition$city == "Münster",]$enj_functionality)

# Boxplot without outliers
p_experiment <- ggplot(data = enjoyment_group, aes(x= group, y = enj_functionality ))
p_experiment + geom_boxplot()  
p_experiment + geom_boxplot() + facet_grid(. ~ city)



