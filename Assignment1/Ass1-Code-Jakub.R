## Set working directory or open Open-To-Compile.Rproj
setwd("~/Data/github/AGLM/Assignment1")

#####  Task 1  #####

a <- -498.683 / 140.988
b <- 4.885 * 6.677
c <- 9.112 / 6.900
d <- 1841257.15 / (1 - 0.5708)
e <- d - 1841257.15
f = (e / 2) / (181257.15 / (51-(2+1)) )
p = pf(f,2,48,lower.tail = FALSE)

##### Task 2 #####

# Nothing yet

##### Task 3 #####

# TBC

##### Task 4 #####

munich <- read.csv("munich.csv", header=TRUE, fill=FALSE, sep = ",")
munich$location <- factor(munich$location)
str(munich)
summary(munich)

fit1 <- lm(rent ~ ., data=munich)
summary(fit2)

fit2 <- lm(rent ~ . + location:area, data=munich)
summary(fit2)

par(mfrow=c(2,2))
plot(fit2)
par(mfrow=c(1,1))

fit3 <- lm(rent ~ area, data=munich)
summary(fit3)

plot(rent ~ area, data=munich, pch=20)
abline(fit3,col=2)
