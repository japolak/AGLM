#******************************************************************************************************
# Applied generalized linear model - FS 
# Viviana Amati 
# Social Network Labs
# Department of Humanities, Social and Political Sciences
# ETH Zurich
# 24 March 2020

# This script provides the code for applying binary logistic regression models
# The commented output is in the lecture notes.
#******************************************************************************************************


#-----------------------------------------------------------------------------------------------------
# Setting directory and loading packages
#-----------------------------------------------------------------------------------------------------
setwd("~/Data/github/AGLM/Course_Material")
library(ggplot2)
library(tidyr)
library(car)

# Importing the data and check
admission <- read.csv("admission.csv",header=TRUE)
head(admission)
summary(admission)


# Recoding the variable rank as a factor
admission$rank <- factor(admission$rank,levels=1:4,labels=1:4)
summary(admission)


#-----------------------------------------------------------------------------------------------------
# Some descriptive statistics
#-----------------------------------------------------------------------------------------------------
# Histograms showing the distribution of the variables
histData <- gather(admission, key=key, value=value)
histData$value <- as.integer(histData$value)
plot1= ggplot(histData, aes(value)) +
  geom_histogram(bins = 10, color= "black", fill="grey70") +
  facet_wrap(~key, scales = "free_x", nrow = 2, ncol = 2) +
  theme_bw()
plot1
# Scatter matrix
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}
pairs(admission, lower.panel = panel.cor, pch = 18)

# Proportions of admitted by gpa, gre and rank
gpaCat <- cut(admission$gpa,c(seq(4,6,0.2)), labels=FALSE)
prop.admit.gpa <- tapply(admission$admit,gpaCat,mean)

greCat <- cut(admission$gre,c(seq(260,960,50)), labels=FALSE)
prop.admit.gre <- tapply(admission$admit,greCat,mean) 

prop.admit.rank <-tapply(admission$admit,admission$rank,mean)
plot(prop.admit.rank,pch=19,xlab="rank")

par(mfrow=c(2,2))
plot(seq(4.1,6,0.2),prop.admit.gpa,pch=19,xlab="gpa")
plot(seq(275,935,50), prop.admit.gre,pch=19,xlab="gre")
plot(prop.admit.rank,pch=19,xlab="rank")


#-----------------------------------------------------------------------------------------------------
# Model estimation
#-----------------------------------------------------------------------------------------------------
# The code below estimates a logistic regression model using the 
# glm (generalized linear model) function. 
# This function is used to fit generalized linear models and requires the specification 
# of the
# - dependent and explanatory variables using the usual formula
#   dependent variable ~ explanatory variables separated by +
# - description of the error distribution using the "family" argument
#   For the logistic model family = binomial(link = "logit")
mod1 <- glm(admit~gre+gpa+rank, family=binomial(link = "logit"), data=admission[,1:4])

# When a model includes all the other variables in the data frame
# we can avoid to list all the variables by using
mod1 <- glm(admit~.,family="binomial",data=admission)
# The default link function for the binomial family is the logit. Therefore, we can omit
# (link = "logit") from the formula above and use the upper commas.


#-----------------------------------------------------------------------------------------------------
# Model diagnostics
#-----------------------------------------------------------------------------------------------------
# Standard way not too helpful
par(mfrow=c(2,2))
plot(mod1, pch=19,cex=0.1)

# A better way to visualize the diagnostics
# Linearity
residualPlots(mod1, type = "deviance", pch=20, smooth=list(col="red"))

# Outliers, leverage, Cook's distance
influenceIndexPlot(mod1,vars=c("Studentized", "hat", "Cook"), id=list(n=c(4)))
outlierTest(mod1) # Testing outliers
CookThreshold <- 5/400*qchisq(0.05,1,lower.tail=FALSE) # Cook?s distance threshold for GLM
CookThreshold

# Are 198 and 156 really influential?
mod2 <-update(mod1,subset=-c(198))
compareCoefs(mod1,mod2)

mod3 <-update(mod1,subset=-c(156))
compareCoefs(mod1,mod3)

#-----------------------------------------------------------------------------------------------------
# Parameter interpretation
#-----------------------------------------------------------------------------------------------------
# The commented code shows how the p-values of the Wald test are computed 
# Wald test for testing the association between admit and each explanatory variable:
# H_0: beta_j=0 vs. H_1: beta_j != 0
# names(summary(mod1))
# summary(mod1)$coefficients
# beta.est <- summary(mod1)$coefficients[,1]
# se.est <- summary(mod1)$coefficients[,2]
# z.values <- beta.est/se.est
# p.values <- 2*pnorm(abs(z.values),lower.tail=FALSE)
# data.frame(beta.est,se.est,z.values,p.values)

summary(mod1)

# Odds ratios and Wald CIs
results <- cbind(coefs=mod1$coefficients, OR = exp(coef(mod1)), exp(confint.default(mod1)))
exp(summary(mod1)$coefficients[,1]-qnorm(0.975)*summary(mod1)$coefficients[,2])
exp(summary(mod1)$coefficients[,1]+qnorm(0.975)*summary(mod1)$coefficients[,2])

# Odds ratios and profile-likelihood CIs
results <- cbind(coefs=mod1$coefficients, OR = exp(coef(mod1)), exp(confint(mod1)))
results

# Percentage change
100*(exp(coef(mod1))-1)

# Predicted probabilities
source("multiplot.R")

# Predicted probabilities for the variable gpa
data.gpa <- with(admission, 
                 data.frame(gre = mean(gre), gpa = rep(seq(from = 4, to = 6, length.out=200),4),
                            rank = factor(rep(1:4, each = 200))))
predict.gpa <- cbind(data.gpa, predict(mod1, newdata=data.gpa, type = "response", se = TRUE))
predict.gpa <- within(predict.gpa, PredictedProb <- plogis(fit))
head(predict.gpa)
p.gpa <- ggplot(predict.gpa, aes(x = gpa, y = PredictedProb)) + 
  geom_line(aes(colour = rank), size = 1)+theme_bw()

# Predicted probabilities for the variable gre
data.gre <- with(admission, 
                 data.frame(gpa = mean(gpa), gre = rep(seq(from = 260, to = 960, length.out=700),4),
                            rank = factor(rep(1:4, each = 700))))
predict.gre <- cbind(data.gre, predict(mod1, newdata=data.gre, type = "response", se = TRUE))
predict.gre <- within(predict.gre, PredictedProb <- plogis(fit))
head(predict.gre)
p.gre <- ggplot(predict.gre, aes(x = gre, y = PredictedProb)) + 
  geom_line(aes(colour = rank), size = 1)+theme_bw()


# Predicted probabilities for the variable rank
data.rank <- with(admission, 
                  data.frame(gpa=mean(gpa), gre = mean(gre), rank = factor(1:4)))
predict.rank <- cbind(data.rank, predict(mod1, newdata = data.rank, type = "response"))
colnames(predict.rank)[4] <- "PredictedProb"
p.rank <- ggplot(predict.rank, aes(x = rank, y = PredictedProb)) +  
          geom_point(aes(colour = rank))+theme_bw()

multiplot(p.gpa, p.gre,p.rank, cols = 1)


#-----------------------------------------------------------------------------------------------------
# Hypothesis testing
#-----------------------------------------------------------------------------------------------------
# More than one parameter:
# Model fit (overall test): H_0: beta_1=...=beta_p=0
mod.empty <- glm(admit~1,family="binomial",data=admission)
anova(mod.empty,mod1,test="Chisq")

# Computing the test by hand
# G.value <- with(mod1, null.deviance - deviance)
# G.value 
# df.G <- with(mod1, df.null - df.residual)
# df.G
# pvalue.G <- pchisq(G.value,df.G,lower.tail=FALSE)
# pvalue.G
# quantile.G <- qchisq(0.05,df.G) 
# quantile.G


# Subset of parameters
# E.g. H_0 = beta_{r2}=beta_{r3}=beta_{r4}
mod.red <- glm(admit~gre+gpa, family="binomial", data=admission)
anova(mod.red,mod1,test="Chisq")

# By hand
# G.value <- mod.red$deviance - mod1$deviance
# G.value 
# df.G <- with(mod1, df.null - df.residual)-with(mod.red, df.null - df.residual)
# df.G
# pvalue.G <- pchisq(G.value,df.G,lower.tail=FALSE)
# pvalue.G
# quantile.G <- qchisq(0.05,df.G) 
# quantile.G


#-----------------------------------------------------------------------------------------------------
# Model selection
#-----------------------------------------------------------------------------------------------------
# Forward selection: start from the model with only the intercept:
mod.fin <- step(mod.empty, direction="forward",
                scope=formula(mod1))
mod.fin 
summary(mod.fin)

#-----------------------------------------------------------------------------------------------------
# Probit model
#-----------------------------------------------------------------------------------------------------
mod2 <- glm(admit~gre+gpa+rank,family=binomial(link = "probit"),data=admission[,1:4])
summary(mod2)

#-----------------------------------------------------------------------------------------------------
# Grouped data
#-----------------------------------------------------------------------------------------------------
titanic <- read.csv("titanic.csv",header=TRUE)
modTitanic <- glm(cbind(Survived,Died)~.,data=titanic,family="binomial")
summary(modTitanic) 
w
