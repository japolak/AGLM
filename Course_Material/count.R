#******************************************************************************************************
# Applied generalized linear model - FS 
# Viviana Amati 
# Social Network Labs
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 12 May 2020

# This script provides the code to make inference using the Poisson and the Negative Binomial regression models. 
# The commented output is in the lecture notes.
#******************************************************************************************************


# The data set absences.csv contains information on the number of absences of high school students in a school. 
# The variables in the data set are:
# gender: gender ("male,"female")
# math: the score on a math test (range: 0 - 100)
# nabs: number absences in last school year (in days)
# prog: the type of instructional program in which the student is enrolled ("General", "Academic", "Vocational")

# The school director would like to understand the attendance behaviour of the students and 
# relate it to the characteristics of the students and the study program they chose described 
# by the variables above  


#-----------------------------------------------------------------------------------------------------
# Directory and libraries
#-----------------------------------------------------------------------------------------------------
setwd("...")
library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)
library(car)
install.packages("sandwich")
library(sandwich)


#-----------------------------------------------------------------------------------------------------
# Import data and descriptive statistics
#-----------------------------------------------------------------------------------------------------
absence <- read.csv("absences.csv")
head(absence)
summary(absence)


# Histograms showing the distribution of the variables
ggplot(gather(absence[,2:3]), aes(value)) + 
  geom_histogram(bins=8,color="black", fill="grey70") + 
  facet_wrap(~key, scales = 'free') +
  theme_bw()

ggplot(gather(absence[,c(1,4)]), aes(value)) + 
  geom_histogram(color="black", fill="grey70", stat="count") + 
  facet_wrap(~key, scales = 'free') +
  theme_bw()


# Dependence of Y on the explanatory variables
# Math score
ggplot(absence, aes(x=math,y=nabs))+ geom_point()+
  theme_bw()

# Program
ggplot(absence, aes(nabs, fill = prog)) + geom_histogram(aes(y=..density..),color="black",binwidth = 1) + 
  facet_grid(cols = vars(prog), scales = "fixed")+ theme(legend.position="bottom")

# Gender
ggplot(absence, aes(nabs, fill=gender)) + geom_histogram(aes(y=..density..),color="black",binwidth = 1) + 
  facet_grid(cols = vars(gender), scales = "fixed")+ theme(legend.position="bottom")

# Given the descriptives, what would you expect the sign of the coefficients of the estimated PRM to be?
# Take three minutes to think about this and record your expectation below

# gender ref. female: 
# program ref. academic: 
# math:

# We will check at the end


#-----------------------------------------------------------------------------------------------------
# Model estimation
#-----------------------------------------------------------------------------------------------------
# The code below estimates a PRM using the glm function. 
# The function requires the specification of the dependent and explanatory variables 
# using the usual formula:
#          dep ~ explanatory variables separated by +

modPoi1 <- glm(nabs ~ math + prog + gender, family="poisson", data=absence)
summary(modPoi1)


#-----------------------------------------------------------------------------------------------------
# Model diagnostics
#-----------------------------------------------------------------------------------------------------
# A better way to visualize the diagnostics
# Linearity
residualPlots(modPoi1, type = "deviance", pch=20, smooth=list(col="red"))

# Outliers, leverage, Cook's distance
influenceIndexPlot(modPoi1,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)))

# I will send you in the breaking rooms. Analyse the model diagnostic and argue whether the linearity 
# assumption is met by the data, there are outliers, high leverage or influential points. 
# Summarize your conclusions (answers + motivation).
# Choose one reference person in the group that will report your findings.
# You have 5 minutes and then I will call you back


#-----------------------------------------------------------------------------------------------------
# Model fit
#-----------------------------------------------------------------------------------------------------
# Test the goodness of fit of the model
modPoi2 <-  glm(nabs ~ 1, family="poisson", data=absence)
anova(modPoi2, modPoi1, test="Chisq")


#-----------------------------------------------------------------------------------------------------
# Parameter interpretation
#-----------------------------------------------------------------------------------------------------
# Factor and percentage change
resPoi <- round(data.frame(summary(modPoi1)$coefficients, exp.Est=exp(modPoi1$coefficients), 
                     perc.Est=(exp(modPoi1$coefficients)-1)*100),digits=3)
resPoi


#-----------------------------------------------------------------------------------------------------
# Have we forgotten anything? 
#-----------------------------------------------------------------------------------------------------
# Overdispersion/Underdispersion!
with(absence, tapply(nabs, prog, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
with(absence, tapply(nabs, gender, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))


# Robust standard error
cov.modPoi1 <- vcovHC(modPoi1, type="HC0")
std.err <- sqrt(diag(cov.modPoi1))
resPoiSan <- round(data.frame(Estimate= coef(modPoi1), Robust.se = std.err,
                   p.value = 2 * pnorm(abs(coef(modPoi1)/std.err), lower.tail=FALSE), 
                   exp.Est=exp(modPoi1$coefficients)),digits=3)
resPoiSan


#-----------------------------------------------------------------------------------------------------
# Negative binomial model
#-----------------------------------------------------------------------------------------------------
modNegBin <- glm.nb(nabs ~ math + prog + gender, data = absence)
summary(modNegBin)


#-----------------------------------------------------------------------------------------------------
# Model diagnostics
#-----------------------------------------------------------------------------------------------------
# A better way to visualize the diagnostics
# Linearity
residualPlots(modNegBin, type = "deviance", pch=20, smooth=list(col="red"))

# Outliers, leverage, Cook's distance
influenceIndexPlot(modNegBin,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)))
outlierTest(modNegBin) # Testing outliers


#-----------------------------------------------------------------------------------------------------
# Parameter interpretation
#-----------------------------------------------------------------------------------------------------
# Factor and percentage change
resmodNegBin <- round(data.frame(summary(modNegBin)$coefficients, exp.Est=exp(modNegBin$coefficients), 
                           perc.Est=(exp(modNegBin$coefficients)-1)*100),digits=3)
alpha <- 1/modNegBin$theta
alpha

# Formal test for overdispersion: Assignment 3, Task 4

# Predicted values
# Order by gender, program and math
absence$predict <- predict(modNegBin,type="response")
absence2 <- absence[with(absence, order(gender,prog, math)), ]

#Plot predict values
ggplot(absence2, aes(x = math, y = predict, colour = prog)) +
  geom_point(aes(y = nabs), alpha=.5, position=position_jitter(h=.2)) +
  geom_line(size = 1) +
  facet_grid(cols = vars(gender)) +
  labs(x = "Math Score", y = "Expected number of absences")




