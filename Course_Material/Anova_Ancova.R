#******************************************************************************************************
# Applied generalized linear model - FS
# Viviana Amati
# Social Network Labs
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 10 March 2020

# This script provides the code to make inference using ANOVA and ANCOVA.
# The commented output is in the lecture notes.
#******************************************************************************************************


#-----------------------------------------------------------------------------------------------------
# Setting directory and loading packages
#-----------------------------------------------------------------------------------------------------
setwd("~/Data/github/AGLM/Course Material")
library(ggplot2)
library(tidyr)


#-----------------------------------------------------------------------------------------------------
# One-way ANOVA
#-----------------------------------------------------------------------------------------------------
# A clinical trial is run to evaluate the effect of several weight loss programs.
# 20 participants are randomly assigned to one of the programs and are counseled on the details
# of the assigned program. Participants follow the assigned program for 8 weeks.
# The outcome of interest is weight loss, defined as the difference in weight measured at the start of
# the study (baseline) and weight measured at the end of the study (8 weeks), measured in Kilograms.
# Thus, a positive value indicate a weight loss, while a negative value a weight gain.

# Three weight loss programs and one control group are considered :
# - low calorie diet
# - low fat diet
# - low carbohydrate diet
# - control group: participants are told that they are participating in a study of healthy behaviors
#   with weight loss only one component of interest. The control group is included to assess the placebo effect
#   (i.e., weight loss due to simply participating in the study).

# Is there any difference in the effects of the weight loss programs?

#----------------------------------------------
# Reading data and descriptives
#----------------------------------------------
weightOrig <- read.csv("weightloss.csv")
weightOrig

# Arrange the data set so that each observation is a row
weight <- weightOrig %>% gather(subject, wloss, subj1:subj5)
head(weight)

# Box-plot, mean and variance by program
ggplot(aes(x=program, y=wloss, fill=program), data=weight) +
  geom_boxplot() + theme_bw()
tapply(weight$wloss, weight$program, mean)
tapply(weight$wloss, weight$program, var)

# ANOVA
mod1 <- aov(wloss ~ program, data=weight, contrasts = list(program = "contr.sum"))
# The argument contrasts = list(program = "contr.sum") tells R to use the deviation regressors
# otherwise R by default use the parametrization using the dummy model
# Try
# mod2 <- aov(wloss ~ program, data=weight)
# model.matrix(mod2)
model.matrix(mod1) # by default, the last level is taken as reference
head(weight)

# We would like to set the control variable as our reference
weight$program <- factor(weight$program,levels(weight$program)[c(2:4,1)]) # reordering levels so that control is the reference
mod1 <- aov(wloss ~ program, data=weight, contrasts = list(program = "contr.sum"))
model.matrix(mod1)
summary(mod1) # ANOVA table
summary.lm(mod1) # ANOVA model

# Diagnostics
par(mfrow=c(2,2))
plot(mod1)
bartlett.test(wloss ~ program, data=weight) # Bartlett's test for equality of variance

# Interpretation
summary(mod1)
summary.lm(mod1)
intercept <- mean(weight$wloss)
intercept
alphas <- tapply(weight$wloss, weight$program, mean) - mean(weight$wloss)
alphas
# What is the average for the control group?

# Compare all the pairs of programs
TukeyHSD(mod1)

# Relation between ANOVAand LRM model with dummy variables
weight$program <- relevel(weight$program, ref="control")
mod2 <- lm(wloss ~ program, data=weight)
summary(mod2)
# What is the average for the control group?
# What about the F-test?


#-----------------------------------------------------------------------------------------------------
# ANCOVA
#-----------------------------------------------------------------------------------------------------
# For a sample of adults American aged over 25 the following information has been collected:
# - inc: annual income (thousands of dollars)
# - race: racial-ethnic group (b = black, h=hispanic, w=white)
# - educ: number of years of education (12 = high school graduate, 16 = college graduate)
# We considere the information collected on N=80 subjects from a larger sample the U.S. Bureau of census.

# Is there a relationship between race and income while controlling for education?
#----------------------------------------------
# Reading data and descriptives
#----------------------------------------------
income <- read.table("incomeRaceEduc.dat", header=TRUE)
head(income)
summary(income)

# Box-plot, mean and variance by race
ggplot(aes(x=race, y=inc, fill=race), data=income) +
  geom_boxplot() + theme_bw()
tapply(income$inc, income$race, mean)
tapply(income$inc, income$race, var)
ggplot(aes(x=educ, y=inc), data=income) +
  geom_point() + theme_bw()

# ANCOVA: deviation regressors and centred quantitative explanatory varibles
income$educCentred <- income$educ - mean(income$educ) # centering
mod3 <- aov(inc ~ race + educCentred, data=income, contrasts = list(race = "contr.sum"))
model.matrix(mod3)

# Diagnostics
par(mfrow=c(2,2))
plot(mod3)

# Interpretation
summary(mod3) # ANCOVA table
summary.lm(mod3) # ANCOVA model
# What is the expected income for a subject with mean education and black racial-ethnic group?

# Interaction: does education has a different effect on income for different levels of race?
ggplot(income, aes(x=educ, y=inc, color=race)) +
  geom_point() +
  geom_smooth(method=lm,se=FALSE) +  # Add linear regression lines
  theme_bw()
mod4 <- aov(inc ~ race + educCentred + race:educCentred, data=income, contrasts = list(race = "contr.sum"))
summary(mod4) # ANCOVA table
summary.lm(mod4) # ANCOVA model
par(mfrow=c(2,2))
plot(mod4)

# Comparison with LRM with dummy variables and non-centred variables
income$race <- relevel(income$race, ref="w")
mod5 <- lm(inc ~ race + educ , data=income)
summary(mod5)

# Same test F and variance decomposition
anova(mod5)
summary(mod3)
