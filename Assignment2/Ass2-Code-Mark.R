library(nnet)

data <- read.csv("alligator.csv")
data$food <- relevel(data$food, "fish")


# 1
#

# 2
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


# 4
fit2 <- update(fit, ~ . - lake)
anova(fit, fit2, test="Chisq")


# 5

fit.null <- multinom(food ~ 1, data, weights = count)
anova(fit.null, fit, test="Chisq")



