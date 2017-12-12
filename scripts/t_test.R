# Description: This script generates the graphs for anlyzing 
# reported mode of transportantion based on and the absence / presence matrix
#
# Comments: set your working directory to 
# Author: Diego Pajarito 


# Setup 
table_answers <- read.csv('data/Questionnaire_Answers.csv')

# Paired t.test


t.test(table_answers$profile_cycling_1, table_answers$satisfaction_cycling)



t.test(table_answers$engagement_A1, table_answers$engagement_B1)

t.test(table_answers$engagement_A2, table_answers$engagement_B2)

t.test(table_answers$engagement_A3, table_answers$engagement_B3)





# Unpaired

comp vs colla