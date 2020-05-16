library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)
library(car)
library(sandwich)

medpar <- read.csv("medpar.csv")
head(medpar)
summary(medpar)

medpar$hmo <- factor(medpar$hmo, levels=c("0", "1"), labels=c("private", "hmo"))
medpar$white <- factor(medpar$white, levels=c("0", "1"), labels=c("non-white", "white"))
medpar$age80 <- factor(medpar$age80, levels=c("0", "1"), labels=c("<80", ">80"))
medpar$type <- factor(medpar$type, levels=c("1", "2", "3"), labels=c("elective", "urgent", "emergency"))
head(medpar)

ggplot(medpar, aes(x=type, y=los, fill=type)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()
ggplot(medpar, aes(x=hmo, y=los, fill=hmo)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()
ggplot(medpar, aes(x=white, y=los, fill=white)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()
ggplot(medpar, aes(x=age80, y=los, fill=age80)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()

mod0 <- aov(los ~ type, data=medpar)
summary(mod0)

mod1 <- glm(los ~ type, family="poisson", data=medpar)
summary(mod1)

mod2 <- glm(los ~ type + white + age80 + hmo, family="poisson", data=medpar)
summary(mod2)

anova(mod1, mod2, test="Chisq")
anova(mod1,mod2)

ggplot(subset(medpar, age80 == '>80'), aes(x=type, y=los, fill=type)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()
ggplot(subset(medpar, age80 == '<80'), aes(x=type, y=los, fill=type)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()
ggplot(medpar, aes(x=type:age80, y=los, fill=type)) + geom_boxplot() +stat_summary(fun.y=mean, color='black', show.legend = FALSE, geom = "point", shape=19, size=3) + theme_bw()

with(medpar, tapply(los, type, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
with(medpar, tapply(los, white, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
with(medpar, tapply(los, age80, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))
with(medpar, tapply(los, hmo, function(x) {
  sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))
}))

mod3 <- glm.nb(los ~ type + white + age80 + hmo, data=medpar)
summary(mod2)
summary(mod3)


mod4 <- glm(los ~ type + white + age80 + hmo, family="poisson", data=medpar, subset=medpar[-c(1452,1466),])
summary(mod4)
summary(mod1)
