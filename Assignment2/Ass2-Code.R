setwd("~/...")

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

##### Task 3 #####

magazine <- read.csv("magazine.csv",header=TRUE)

## 1
model_0 <- glm(Buy~Is.Female+Unemployed+Income+Own,family="binomial", data = magazine)
summary(model_0)

drop1(model_0,test="Chisq")

model_1 <- glm(Buy~Is.Female+Income+Own,family="binomial",data = magazine)
summary(model_1)


## 2
model_2 <- update(model_1,.~.+Prev.Art.Mag+Prev.Cinema.Mag)
summary(model_2)

anova(model_1,model_2,test="Chisq")

model_2a <- update(model_1,.~.+Prev.Art.Mag)
summary(model_2a)
anova(model_1,model_2a,test="Chisq")
anova(model_2a,model_2,test="Chisq")


## 3
fullmodel<- glm(Buy ~.,family='binomial', data=magazine)
mod.fin <- step(fullmodel, direction = 'backward')
summary(mod.fin)

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

library(nnet)
data <- read.csv("alligator.csv")
data$food <- relevel(data$food, "fish")

## 2 Interpretation
fit <- multinom(food ~ lake + size + sex, data, weights = count)
summary(fit)

output <- summary(fit)
z <- output$coefficients/output$standard.errors
p <- (1 - pnorm(abs(z), 0, 1))*2
resbird <-cbind(output$coefficients[1,],output$standard.errors[1,],z[1,],p[1,],exp(output$coefficients[1,]))
resinvert <- cbind(output$coefficients[2,],output$standard.errors[2,],z[2,],p[2,],exp(output$coefficients[2,]))
resother <- cbind(output$coefficients[3,],output$standard.errors[3,],z[3,],p[3,],exp(output$coefficients[3,]))
resreptile <- cbind(output$coefficients[4,],output$standard.errors[4,],z[4,],p[4,],exp(output$coefficients[4,]))

res <- rbind(resbird, resinvert, resother, resreptile)
res <- round(res,digits=3)
colnames(res) <- c("Est.","Std. Errors","z stat","p value","RRR")

res <- data.frame(logit=c(rep("Bird vs. Fish",6),rep("Invertebrate vs. Fish",6),rep("Other vs. Fish",6),rep("Reptile vs. Fish",6)),
                  param=rep(rownames(resbird),4) , res)
rownames(res) <- NULL
res


## 3 Odds ratio
OR = exp(-0.2906)/exp(0.7302)


## 4 Global significance of lake
fit2 <- update(fit, ~ . - lake)
anova(fit, fit2, test="Chisq")


## 5 Fit of the null model
fit.null <- multinom(food ~ 1, data, weights = count)
anova(fit.null, fit, test="Chisq")
