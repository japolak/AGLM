Ass3-Code-Jakub
#library(ggplot2)
library(MASS)
#library(reshape2)
#library(tidyverse)
#library(car)
#library(sandwich)
library(plyr)

##### Import data #####
inputdata <- read.csv("medpar.csv",header = TRUE, sep = ",")

##### 3.1 Descriptive los vs type #####
# Data modification into factor
df1 <- data.frame(los=inputdata$los, type=inputdata$type)
df1$type <- factor(los_type$type)
# Pairs plot
pairs(df1)
# Box plot
plot(y=df1$los,x=df1$type)
# Summary stats for factor
df1_summary<-ddply(df1, .(type), summarize, mean=mean(los), sd=sd(los))

## From the above pairs plot one sees that there seems to be different
## variability across the los for the three groups of “type”.
## This is supported when looking at the average “los” and the
## corresponding standard deviation per level of “type”.
## There is a difference between the means and also the sd increases
## with the level of “type”. This is also visible in the following box plots.
## Concluding we will expect an impact of “type” on los, as the expected (mean)
## “los” values seem to differ depending on “type”.

##### 3.2 Poisson los vs type #####
# Fit of model with only type
model1 <- glm(los ~ type, family="poisson", data=df1)
summary(model1)

# Check residuals
plot(model1)
residualPlots(model1, type = "deviance", pch=20, smooth=list(col="red"))
influenceIndexPlot(model1,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)), cex=" ")

# Fit of empty model
model0 <- glm(los ~ 1, family="poisson", data=los_type)
summary(model0)

# Comparison of models
anova(model0,model1, test="Chisq")
# From the anova()-output we can conlude that we can reject the H0
# that “type” has no influence at a significance level of 5%, as the
# corresponding p-value lies below this threshold.

##### 3.3 Poisson los vs everything #####
# Preprocess data
df2 <- inputdata
df2$hmo <- factor(df2$hmo)
df2$white <- factor(df2$white)
df2$age80 <- factor(df2$age80)
df2$type <- factor(df2$type)
# Fit model with everything
model2 <- glm(los ~ type+hmo+white+age80, family="poisson", data=df2)
summary(model2)

# Check residuals
plot(model2)
residualPlots(model2, type = "deviance", pch=20, smooth=list(col="red"))
influenceIndexPlot(model2,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)))

# Compare models
anova(model1,model2, test="Chisq")

##### 3.4 Interpretation of age80
# As "age80" is significantly different from 0 at the 5% level:
# The expected value for “los” differs by a factor 0.9467596 when
# changing “age80” from 0 to 1, controlling for all other variables.
# This seems to make sense, as it suggests that patients older
# than 80 years tend to stay in hospital longer.

##### 3.5 Test for equidispersion #####
# Fit Negative binomial model
model3 <-glm.nb(los ~ type+hmo+white+age80, data=df2)
summary(model3)

# Check residuals
plot(model3)
residualPlots(model3, type = "deviance", pch=20, smooth=list(col="red"))
influenceIndexPlot(model3 ,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)))
outlierTest(model3)

# Equidispersion Test
library(AER)
dispersiontest(model2,alternative="greater",trafo=2)
