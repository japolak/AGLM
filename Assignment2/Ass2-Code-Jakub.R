## Set working directory or open Open-To-Compile.Rproj
setwd("~/Data/github/AGLM/Assignment2")

#####  Task 1  #####

X1_est = -0.868
X1_z = -2.365
X1_se = X1_est / X1_z
X1_or = exp(X1_est)
X1_l = exp(X1_est - qnorm(0.975)*X1_se)
X1_u = exp(X1_est + qnorm(0.975)*X1_se)

X2_est = 2.404
X2_se = 0.601
X2_z = 4.000
X2_or = exp(X2_est)
X2_l = exp(X2_est - qnorm(0.975)*X2_se)
X2_u = exp(X2_est + qnorm(0.975)*X2_se)

X3_l = 0.01
X3_u = 0.074
X3_est = (log(X3_l) + log(X3_u))/2
X3_se = (log(X3_u) - log(X3_l))/(-qnorm(0.975)*2)
X3_z = X3_est / X3_se
X3_or = exp(X3_est)

##### Task 2 #####

# H0: β_jm = 0                  vs. H1: β_jm !=0

# H0: β_j1 = ... = β_j(M-1) = 0 vs. H1: at least one β_jm !=0  for all m

# H0: β_j1 = ... = β_j(M-1) = 0 vs. H1: at least one β_jm !=0  for all j,m

# H0: β_jm = β_j for all m      vs. H1: β_jm != β_j for at least one m

# The individual parameter characterization of every logit indicates that the relation between Y and X might vary over categories of Y.
# Thus, our proposed hypothesis would serve the purpose of testing whether these relation (for some j of interest) actually varies over the categories m or not (test if parameter βj is independent of level m of Y or not).

# The hypothesis could be reformulated as H0 : βjm − βj = 0 vs. H1 : βjm − βj != 0 for at least one m

##### Task 3 #####

magazine <- read.csv("magazine.csv",header=TRUE)

## 1
model_0 <- glm(Buy~Is.Female+Unemployed+Income+Own,family="binomial", data = magazine)
summary(model_0)

drop1(model_0,test="Chisq")

#Q: Is there any explanatory variable you would remove from the model?
#Short Answer: Unemployed

#Q: On the basis of which test?
#Short Answer: Wald test -Normal dist or Likelihood ratio test -Chisq dist

# Long Answer:
# From the above summary of glm model, it seems that the variable “Unemployed” is not significant at level 5% (p-value = 0.8>>0.05). The summary function performs a Wald test. This is the result of a two sided test testing of hypothesis H0 : β_Unemployed = 0 vs H1: β_Unemployed != 0. The test statistic is computed as W = Estimate/Std.Error and compares it to standard normal distribution.
# The same result would be found by using drop1 function and performing Likelihood ratio test. LRT compares whether the difference in log likelihoods of a full model and a nested smaller model is significant. The model that fits better has a higher log-likelihood so the test statistic is computed as D = 2(LL_0 - LL_drop1) follows a Chisq distribution with the degrees of freedom being the difference in number of variables.
# It is indicative of an input variable that is in some way important to include in a good model. We see that all smaller models are significantly different from the full model, besides the model without Unemployment. Hence, the model without Unemployment is not significantly different from the model with Unemployment, which means the smaller model without it is just as good.
# The drop1 function also shows AIC criterion for the models obtained by dropping one term. Leaving all the variables in the model gives an AIC of 234.53. The AIC can be improved (=decreased) by dropping “Unemployed”, resulting in a new AIC of 232.59.

model_1 <- glm(Buy~Is.Female+Income+Own,family="binomial",data = magazine)
summary(model_1)
# Now all the parameters are significantly different from 0 (at α = 0.05) in the summary.

## 2
model_2 <- update(model_1,.~.+Prev.Art.Mag+Prev.Cinema.Mag)
summary(model_2)

anova(model_1,model_2,test="Chisq")
# We compare the two models with the anova function and a chi-squared-test. The statistics G takes value 6.7045, has χ2 distribution and p-value 0.035. Thus at significance level α = 0.05 we reject H0 : β_PrevArt = β_PrevCin = 0 and conclude that Model 2 including PrevArt and PrevCin has a better fit than Model 1.
#Remark: from the summary output of Model 2 one sees that PrevCin is probably not significantly different from 0. The following checks the Model 2a which adds only “PrevArt” to Model 1.

model_2a <- update(model_1,.~.+Prev.Art.Mag)
summary(model_2a)
anova(model_1,model_2a,test="Chisq")
anova(model_2a,model_2,test="Chisq")

#One sees that the step from Model 1 to Model 2a (adding PrevArt) yields significant (simultaneous) statistics at α = 0.05, whereas adding PrevCin (Model 2a -> Model 2) does not test as significant at α = 0.05: we cannot reject H0 : βPrevCin = 0. Thus probably only adding PrevArt would be a better choice than adding PrevArt and PrevCin simultaneously. It is essential to notice, that this is not a contradiction to the above test results of H0 : βPrevArt = βPrevCin = 0, as the alternative hypothesis is “at least one β is not equal to 0”. Thus, βPrevCin could still be equal 0 given this α and data.

## 3
fullmodel<- glm(Buy ~.,family='binomial', data=magazine)
mod.fin <- step(fullmodel, direction = 'backward')
summary(mod.fin)

# Interpretation
odds_inc <- exp(1000*mod.fin$coefficients["Income"])
odds_fem <- exp(mod.fin$coefficients["Is.Female"])
odds_ret <- exp(mod.fin$coefficients["Is.Retired"])
odds_res <- exp(mod.fin$coefficients["Residence.Length"])
odds_dual <- exp(mod.fin$coefficients["Dual.Income"])
odds_min <- exp(mod.fin$coefficients["Minors"])
odds_own <- exp(mod.fin$coefficients["Own"])
odds_house <- exp(mod.fin$coefficients["House"])
odds_whi <- exp(mod.fin$coefficients["White"])
odds_eng <- exp(mod.fin$coefficients["English"])
odds_prart <- exp(mod.fin$coefficients["Prev.Art.Mag"])



##### Task 4 #####

allig <- read.csv("alligator.csv")
allig$food <- relevel(allig$food, ref = "fish") #set fish as reference level allig.data$lake <- relevel(allig.data$lake, ref = "George")
allig$size <- relevel(allig$size, ref = "small")
allig$sex <- relevel(allig$sex, ref = "male")
library(nnet)
model0 <- multinom(food ~ lake+size+sex, weights = count, data = allig,trace=0)
summary(model0)

## 2 Interpretation

coefR <- summary(model0)$coefficients["reptile",]
odds_lakeHancock <- round(exp(coefR["lakeHancock"]),digits=3)
odds_lakeOklawaha <- round(exp(coefR["lakeOklawaha"]),digits=3)
odds_lakeTrafford <- round(exp(coefR["lakeTrafford"]),digits=3)
odds_sizelarge <- round(exp(coefR["sizelarge"]),digits=3)
odds_sexfemale <- round(exp(coefR["sexfemale"]),digits=3)
# In general for a +1 unit change in x_j (or a category switch for dummy variables) the factor by which the odds of m relative to M are expected to change by a factor of eβjm controlling for all other variables.
reptile_z <- summary(model0)$coefficients["reptile",]/summary(model0)$standard.errors["reptile",]
reptile_p.value <- round((1 - pnorm(abs(reptile_z), 0, 1)) * 2, digits=3)
reptile_z
reptile_p.value
# When accounting for significance at 5% level, one sees, that lakeHancock, sizelarge and sexfemale do not have an impact on the expected odds when changing the dummy (individually) from 0 to 1, controlling for all other variables.

## 3 Odds ratio

# “bird vs. other” for “small relative to large”
(OR = exp(-0.2906)/exp(0.7302))
# For the switch of a small to a large Alligator, the change in odds for the preference of “other” over “fish” is 0.3603 times larger than the change in odds for the preference of “bird” over “fish” for the same change in size; always controlling for all other variables. Furthermore we do not take significance into account. If either βsizelarge,m for m in (“other”,“bird”) is not significant (and thus assumed to not have any impact on the corresponding odds), the interpretation would not be feasible.

## 4 Global significance of lake

# This corresponds to the following null hypothesis: H0 : β_lakeHancock,m = β_lakeOklawaha,m = β_lakeTrafford,m = 0 for m=1,. . . ,M-1. This null hypothesis correpsonds to a reduced model “mod1” without the lake variable. The test statistic G is defined as the difference of the deviances and follows a chi-squared-distribution with the difference of the degrees of freedom of the models. We use the anova()-function of R. We choose α = 0.05.
model1 <- multinom(food ~ size+sex, weights = count, data = allig,trace=0)
summary(model1)
anova(model1,model0)
# as the p-value p.1.4 is 1.228388e-06 which is smaller than 0.05, we reject the hypothesis that the “lake” variable is 0 for all “food”.

# 5 Fit of the model

# Here the null hypothesis is H0 : βj,m = 0 for all j and for m=1,. . . ,M-1. Thus we fit a reduced model “model2” without any explanatory variables, leaving only intercept terms for the different levels of Y. The test statistic G is defined as the difference of the deviances and follows a chi-squared-distribution with degrees of freedom (df) being the difference of the degrees of freedom of the two models. G = D(reduced) − D(f ull) ∼ χ ^2_df,1-α .
# H0 is rejected for G > χ ^2_df,1−α. We choose α = 0.05 and use the anova()-function in R.
model2 <- multinom(food ~ 1, weights = count, data = allig,trace=0)
summary(model2)
anova(model2,model0)
# as the p-value p.1.5 is 6.723387e-07 which is smaller than 0.05, , we reject the hypothesis that all βj, m for all j and m=1,. . . ,M-1 are simultaneously equal to zero at 5% significance level.
