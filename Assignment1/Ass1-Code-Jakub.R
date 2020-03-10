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

1-pt(3,1000)

##### Task 2 #####

library(dplyr)
library(ggplot2)

X <- data.frame("grade" = c(4,6,8,1,5,9,10,5), "group" = c("A","A","A","B","B","C","C","C"))
X2 <- data.frame("grade" = c(4,6,8,9,5,9,10,5), "group" = c("A","A","A","B","B","C","C","C"))
X3 <- data.frame("grade" = c(5,6,7,1.73,4.27,9.6,8,6.4), "group" = c("A","A","A","B","B","C","C","C"))
set.seed(352144)
X4 <- data.frame("grade" = c(rnorm(30,6,2),rnorm(20,3,2.83),rnorm(30,8,2.65)),"group" = c(rep(c("A","B","C"),c(30,20,30)))  )
str(X)
out = group_by(X, group) %>% summarise( count = n(), mean = mean(grade), var = var(grade), sd = sd(grade))
print(data.frame(out))

boxplot(grade ~ group, data=X, col=5:7, frame = FALSE, xlab = "Groups", ylab="Grade")
points(grade ~ group, data=X,pch=20)

Xplot <- X
out = group_by(X, group) %>% summarise( count = n(), mean = mean(grade), var = var(grade), sd = sd(grade))
Xplot$mean <- c(rep(out$mean[1],3),rep(out$mean[2],2),rep(out$mean[3],3))
Xplot$sd <- c(rep(out$sd[1],3),rep(out$sd[2],2),rep(out$sd[3],3))
ggplot(Xplot) +
  geom_errorbar(aes(x= group, ymin= mean-sd, ymax = mean+sd, width=0.3, color=group)) +
  geom_point(aes(x=group, y= mean, color=group),size=5,shape=3) +
  geom_point(aes(x = group, y = grade), size=5) +
  xlab("Groups") +
  ylab("Grade") +
  theme_bw() +
  theme(axis.line = element_line(colour = "grey", size=0), panel.border = element_rect(colour = "white"))


fit <- aov(grade ~ group, data = X)
summary(fit)
#As the p-value is more than the significance level 0.05, we cannot conclude that there are significant differences between the groups highlighted with in the model summary.

fit2 <- aov(grade ~ group, data = X2)
summary(fit2)

fit3 <- aov(grade ~ group, data = X3)
summary(fit3)

fit4 <- aov(grade ~ group, data = X4)
summary(fit4)

##### Task 3 #####

X <- data.frame("strength"=c(3129,3000,2865,2890,3200,3300,2975,3150,2800,2900,2985,3050,2600,2700,2600,2765),"mixing"=rep(c("1","2","3","4"),c(4,4,4,4)) )

boxplot(strength ~ mixing,data=X, xlab="Mixing technique", ylab="Compressive Strength", frame=FALSE, col=5:8)
points(strength ~ mixing, data=X,pch=20)

out = group_by(X, mixing) %>% summarise( count = n(), mean = mean(strength), var = var(strength), sd = sd(strength))
(Xsummary = data.frame(out))

Xplot <- X
Xplot$mean <- c(rep(out$mean[1],4),rep(out$mean[2],4),rep(out$mean[3],4),rep(out$mean[4],4))
Xplot$sd <- c(rep(out$sd[1],4),rep(out$sd[2],4),rep(out$sd[3],4),rep(out$sd[4],4))
ggplot(Xplot) +
  geom_errorbar(aes(x= mixing, ymin= mean-sd, ymax = mean+sd, width=0.3, color=mixing)) +
  geom_point(aes(x= mixing, y= mean, color=mixing),size=5,shape=3) +
  geom_point(aes(x = mixing, y = strength), size=2) +
  xlab("Mixing technique") +
  ylab("Compressive Strength")

fit <- aov(strength ~ mixing, data = X)
summary(fit)

## BELOW VALID ONLY IF THERE IS SIGNIFICANCE OF DIFFERENT GROUP MEANS!!
# Multiple pairwise-comparison between the means of groups
# In one-way ANOVA test, a significant p-value indicates that some of the group means are different, but we don’t know which pairs of groups are different.
# It’s possible to perform multiple pairwise-comparison, to determine if the mean difference between specific pairs of group are statistically significant.


library(multcomp)
summary(glht(fit, linfct = mcp(mixing = "Tukey")))

plot(fit)

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
