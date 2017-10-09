# Description:
#
# Comments: set your working directory to 
# Author: Diego Pajarito 

table_answers = read.csv('data/Questionnaire_Answers.csv')
questions_cyclist <- read.csv('data/Questions_Cyclist_Profile.csv')

answers_cyclist <- table_answers[,12:26]

likert_values <- c(-3,-2,-1,0,1,2,3)
likert_labels <- c("Strongly disagree"," ","  ","Neutral","   ","    ","Strongly agree")
likert_factors <- factor(likert_values)

q1_factor <- factor(answers_cyclist$profile_cycling_1, likert_factors,labels = likert_labels)
q2_factor <- factor(answers_cyclist$profile_cycling_2, likert_factors,labels = likert_labels)
q3_factor <- factor(answers_cyclist$profile_cycling_3, likert_factors,labels = likert_labels)
q4_factor <- factor(answers_cyclist$profile_cycling_4, likert_factors,labels = likert_labels)
q5_factor <- factor(answers_cyclist$profile_cycling_5, likert_factors,labels = likert_labels)
q6_factor <- factor(answers_cyclist$profile_cycling_6, likert_factors,labels = likert_labels)
q7_factor <- factor(answers_cyclist$profile_cycling_7, likert_factors,labels = likert_labels)
q8_factor <- factor(answers_cyclist$profile_cycling_8, likert_factors,labels = likert_labels)
q9_factor <- factor(answers_cyclist$profile_cycling_9, likert_factors,labels = likert_labels)
q10_factor <- factor(answers_cyclist$profile_cycling_10, likert_factors,labels = likert_labels)
q11_factor <- factor(answers_cyclist$profile_cycling_11, likert_factors,labels = likert_labels)
q12_factor <- factor(answers_cyclist$profile_cycling_12, likert_factors,labels = likert_labels)
q13_factor <- factor(answers_cyclist$profile_cycling_13, likert_factors,labels = likert_labels)
q14_factor <- factor(answers_cyclist$profile_cycling_14, likert_factors,labels = likert_labels)
q15_factor <- factor(answers_cyclist$profile_cycling_15, likert_factors,labels = likert_labels)


answers_factors <- data.frame(q1_factor,q2_factor,q3_factor,q4_factor,q5_factor,q6_factor,q7_factor,q8_factor,q9_factor,q10_factor,q11_factor,q12_factor,q13_factor,q14_factor,q15_factor)

names(answers_factors) <- t(questions_cyclist)
answers_likert <- likert(answers_factors)
plot(answers_likert)

plot(answers_likert, centered = FALSE, wrap = 30)

plot(answers_likert, type = "density")
plot(answers_likert, type = "heat")

pdf("graphs/cyclist_profile.pdf")
plot(answers_likert)
dev.off()


# Creating likert grouped by gender 
answers_gender <- table_answers[,3]
answers_gender[answers_gender==1] <- "male"
answers_gender[answers_gender==2] <- "female"
answers_gender[answers_gender==3] <- "other"

likert_values <- c("male", "female","other", -3,-2,-1,0,1,2,3)
likert_labels <- c("male", "female","other","Strongly disagree"," ","  ","Neutral","   ","    ","Strongly agree")
likert_factors <- factor(likert_values)
gender_factors <- factor(answers_gender, likert_labels)

q1_factor <- factor(answers_cyclist$profile_cycling_1, likert_factors,labels = likert_labels)
q2_factor <- factor(answers_cyclist$profile_cycling_2, likert_factors,labels = likert_labels)
q3_factor <- factor(answers_cyclist$profile_cycling_3, likert_factors,labels = likert_labels)
q4_factor <- factor(answers_cyclist$profile_cycling_4, likert_factors,labels = likert_labels)
q5_factor <- factor(answers_cyclist$profile_cycling_5, likert_factors,labels = likert_labels)
q6_factor <- factor(answers_cyclist$profile_cycling_6, likert_factors,labels = likert_labels)
q7_factor <- factor(answers_cyclist$profile_cycling_7, likert_factors,labels = likert_labels)
q8_factor <- factor(answers_cyclist$profile_cycling_8, likert_factors,labels = likert_labels)
q9_factor <- factor(answers_cyclist$profile_cycling_9, likert_factors,labels = likert_labels)
q10_factor <- factor(answers_cyclist$profile_cycling_10, likert_factors,labels = likert_labels)
q11_factor <- factor(answers_cyclist$profile_cycling_11, likert_factors,labels = likert_labels)
q12_factor <- factor(answers_cyclist$profile_cycling_12, likert_factors,labels = likert_labels)
q13_factor <- factor(answers_cyclist$profile_cycling_13, likert_factors,labels = likert_labels)
q14_factor <- factor(answers_cyclist$profile_cycling_14, likert_factors,labels = likert_labels)
q15_factor <- factor(answers_cyclist$profile_cycling_15, likert_factors,labels = likert_labels)

answers_demograpy_factors_a <- data.frame(gender_factors,q1_factor,q2_factor,q3_factor,q4_factor)
names(answers_demograpy_factors_a) <- c("Gender", questions_cyclist.df.nf[1], questions_cyclist.df.nf[2],questions_cyclist.df.nf[3],questions_cyclist.df.nf[4]) 
answers_demography_likert_a <- likert(answers_demograpy_factors_a[2:5], grouping = answers_demograpy_factors_a$Gender )
pdf("graphs/cyclist_profile_gender_a.pdf")
plot(answers_demography_likert_a)
dev.off()

answers_demograpy_factors_b <- data.frame(gender_factors,q5_factor,q6_factor,q7_factor,q8_factor)
names(answers_demograpy_factors_b) <- c("Gender", questions_cyclist.df.nf[5], questions_cyclist.df.nf[6],questions_cyclist.df.nf[7],questions_cyclist.df.nf[8]) 
answers_demography_likert_b <- likert(answers_demograpy_factors_b[2:5], grouping = answers_demograpy_factors_b$Gender )
pdf("graphs/cyclist_profile_gender_b.pdf")
plot(answers_demography_likert_b)
dev.off()

answers_demograpy_factors_c <- data.frame(gender_factors,q9_factor,q10_factor,q11_factor,q12_factor)
names(answers_demograpy_factors_c) <- c("Gender", questions_cyclist.df.nf[9], questions_cyclist.df.nf[10],questions_cyclist.df.nf[11],questions_cyclist.df.nf[12]) 
answers_demography_likert_c <- likert(answers_demograpy_factors_c[2:5], grouping = answers_demograpy_factors_c$Gender )
pdf("graphs/cyclist_profile_gender_c.pdf")
plot(answers_demography_likert_c)
dev.off()

answers_demograpy_factors_d <- data.frame(gender_factors,q13_factor,q14_factor,q15_factor)
names(answers_demograpy_factors_d) <- c("Gender", questions_cyclist.df.nf[13], questions_cyclist.df.nf[14],questions_cyclist.df.nf[15]) 
answers_demography_likert_d <- likert(answers_demograpy_factors_d[2:4], grouping = answers_demograpy_factors_d$Gender )
pdf("graphs/cyclist_profile_gender_d.pdf")
plot(answers_demography_likert_d)
dev.off()


answers_demograpy_factors_a <- data.frame(gender_factors,q1_factor,q2_factor,q3_factor,q4_factor,q5_factor,q6_factor,q7_factor,q8_factor,q9_factor,q10_factor,q11_factor,q12_factor,q13_factor,q14_factor,q15_factor)

plot(answers_demograpy$dem_age, type = "h")
plot(answers_demograpy$dem_marital, type = "b")
plot(answers_demograpy$dem_transport_car)
plot(answers_demograpy$dem_transport_public)
plot(answers_demograpy$dem_transport_bicycle)
plot(answers_demograpy$dem_transport_walk)

answers_transport <- table_answers[2:21,8:11]
plot(t(answers_transport))

