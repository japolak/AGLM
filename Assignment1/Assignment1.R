
#################### R code used for Assignment 1 ######################

# Authors: Jakub Polak, Mark McMahon, Martin Kotuliak and Milan Kuzmanovic

# install.packages("knitr","dplyr","ggplot2","multcomp")
library(dplyr)
library(multcomp)
library(ggplot2)
library(knitr)


# Task 1 ------------------------------------------------------------------


a <- -498.683 / 140.988
b <- 4.885 * 6.677
c <- 9.112 / 6.900
d <- 1841257.15 / (1 - 0.5708)
e <- d - 1841257.15

(f = (e / 2) / (1841257.15 / (51-(2+1)) ))

(p = pf(f,2,48,lower.tail = FALSE))


# Task 2 ------------------------------------------------------------------


X <- data.frame("grade" = c(4,6,8,1,5,9,10,5),
                "group" = c("A","A","A","B","B","C","C","C"))
data.frame(group_by(X, group) %>%
             summarise( count = n(), mean = mean(grade), var = var(grade),
                        sd = sd(grade)))

boxplot(grade ~ group, data=X, col=2:5, frame = FALSE, xlab = "Groups", ylab="Grade")
points(grade ~ group, data=X,pch=20)

Xplot <- X
out = group_by(Xplot, group) %>%
  summarise( count = n(), mean = mean(grade), var = var(grade), sd = sd(grade))
Xplot$mean <- c(rep(out$mean[1],3),rep(out$mean[2],2),rep(out$mean[3],3))
Xplot$sd <- c(rep(out$sd[1],3),rep(out$sd[2],2),rep(out$sd[3],3))
ggplot(Xplot) +
  geom_errorbar(aes(x= group, ymin= mean-sd, ymax = mean+sd, width=0.3, color=group)) +
  geom_point(aes(x=group, y= mean, color=group),size=5,shape=3) +
  geom_point(aes(x = group, y = grade), size=2) +
  xlab("Groups") +
  ylab("Grade") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "white"))

fit <- aov(grade ~ group, data = X)
summary(fit)


# Task 3 ------------------------------------------------------------------


X <- data.frame("strength"=c(3129,3000,2865,2890,
                             3200,3300,2975,3150,
                             2800,2900,2985,3050,
                             2600,2700,2600,2765),
                "mixing"=rep(c("1","2","3","4"),c(4,4,4,4)) )

boxplot(strength ~ mixing,data=X, xlab="Mixing technique", ylab="Compressive Strength", frame=FALSE, col=c(2,3,5,6))
points(strength ~ mixing, data=X,pch=20)

Xplot <- X
out = group_by(Xplot, mixing) %>%
  summarise( count = n(), mean = mean(strength), var = var(strength), sd = sd(strength))
Xplot$mean <- c(rep(out$mean[1],4),rep(out$mean[2],4),rep(out$mean[3],4),rep(out$mean[4],4))
Xplot$sd <- c(rep(out$sd[1],4),rep(out$sd[2],4),rep(out$sd[3],4),rep(out$sd[4],4))
ggplot(Xplot) +
  geom_errorbar(aes(x= mixing, ymin= mean-sd, ymax = mean+sd, width=0.3, color=mixing)) +
  geom_point(aes(x= mixing, y= mean, color=mixing),size=5,shape=3) +
  geom_point(aes(x = mixing, y = strength), size=2) +
  xlab("Mixing technique") +
  ylab("Compressive Strength") +
  theme_bw() +
  theme(axis.line = element_line(colour = "black", size=.5),
        panel.border = element_rect(colour = "white"))

fit <- aov(strength ~ mixing, data = X)
par(mfrow = c(1,2))
plot(fit, which = c(1,2))
summary(fit)

summary(glht(fit, linfct = mcp(mixing = "Tukey")))


# Task 4 ------------------------------------------------------------------


munich <- read.csv("munich.csv")
munich$location <- factor(munich$location)

par(mfrow=c(2,2))
year_agg <- aggregate(munich$rent, list(munich$yearc),
                      function(x) c(mean=mean(x), sd=sd(x)))
plot(year_agg$Group.1, year_agg$x[,"mean"], xlab="Yearc",
     ylab="Rent: mean +- 1 sd", ylim=c(0, 1300))
segments(year_agg$Group.1-0.25, year_agg$x[,"mean"]+year_agg$x[,"sd"],
         year_agg$Group.1+0.25, year_agg$x[,"mean"]+year_agg$x[,"sd"],col="grey")
segments(year_agg$Group.1-0.25, year_agg$x[,"mean"]-year_agg$x[,"sd"],
         year_agg$Group.1+0.25, year_agg$x[,"mean"]-year_agg$x[,"sd"],col="grey")
segments(year_agg$Group.1, year_agg$x[,"mean"]+year_agg$x[,"sd"],
         year_agg$Group.1,year_agg$x[,"mean"]-year_agg$x[,"sd"],col="grey")

plot(rent~area, data=munich, xlab="Area", ylab="Rent")
plot(rent~location, data=munich, xlab="Location", ylab="Rent")
hist(munich$rent, xlab="Rent", main="")

f1 <- lm(rent~area*location + yearc, data=munich)
par(mfrow=c(2,2)) ; plot(f1)
summary(f1)

f2 <- lm(rent~area, data=munich)
summary(f2)
anova(f2, f1)

par(mfrow = c(1,3))
hist(munich$rent)
hist(munich$area)
hist(munich$yearc)

f3 <- lm(log(rent)~log(area)*location + yearc, data=munich)
par(mfrow=c(2,2))
plot(f3)
summary(f3)






