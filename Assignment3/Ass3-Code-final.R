

library(ggplot2)
library(MASS)
library(reshape2)
library(tidyverse)
library(car)
library(sandwich)
library(MASS)


# Task 1 --------------------------------------------------------


# Importing data
freq <- read.csv("frequency.csv")

# Recoding "0" as "NA"
for (i in 1:nrow(freq)) {
  for (j in 1:ncol(freq)) {
    freq[i,j] <- ifelse(freq[i,j] == 0, NA, freq[i,j])
  }
}

# Recoding variable race
low <- as.numeric(names(table(freq$race)[which(table(freq$race) < 100)]))
for (i in 1:nrow(freq)) {
  freq$race[i] <- ifelse(freq$race[i] %in% low, 17, freq$race[i])
}

# Creating factors from nominal and ordinal variables
freq$Q2 <- factor(freq$Q2,
                  levels = c("1","2","3","4","5"),
                  labels = c("never", "rarely", "occasionally", "often", "always"))
freq$education <- factor(freq$education,
                         levels = c("1","2","3","4"),
                         labels = c("less than high school", "high school",
                                    "university degree", "graduate degree"))
freq$urban <- factor(freq$urban,
                     levels = c("1","2","3"),
                     labels = c("rural", "suburban", "urban"))
freq$gender <- factor(freq$gender,
                      levels = c("1","2","3"),
                      labels = c("male", "female", "other"))
freq$race <- factor(freq$race,
                    levels = c("11","16","17"),
                    labels = c("asian", "white","other"))
freq$married <- factor(freq$married,
                       levels = c("1","2","3"),
                       labels = c("never married","married","divorced"))
# Intecepts
beta_never <- log(sum(na.omit(as.numeric(freq$Q2) <= 1))/sum(na.omit(as.numeric(freq$Q2) > 1)))
beta_rarely <- log(sum(na.omit(as.numeric(freq$Q2) <= 2))/sum(na.omit(as.numeric(freq$Q2) > 2)))
beta_occasionally <- log(sum(na.omit(as.numeric(freq$Q2) <= 3))/sum(na.omit(as.numeric(freq$Q2) > 3)))
beta_often <- log(sum(na.omit(as.numeric(freq$Q2) <= 4))/sum(na.omit(as.numeric(freq$Q2) > 4)))
beta_never; beta_rarely; beta_occasionally; beta_often

# Verification
int_mod <- polr(factor(Q2) ~ 1, data = freq)
int_mod

# Backward selection
freq2 <- na.omit(freq)
mod.full <- polr(Q2~., data = freq2)
mod.fin <- step(mod.full, direction = "backward")

# Model fit
mod.empty <- polr(Q2~1, data = freq2)
anova(mod.empty, mod.fin, test = "Chisq")

# Significance
drop1(mod.fin, test = "Chisq")
out <- summary(mod.fin)
out$coefficients[1:8,c(1,3)] <- -out$coefficients[1:8,c(1,3)]
p <- (1 - pnorm(abs(out$coefficients[,3]), 0, 1))*2
OR <- c(exp(out$coefficients[,1]))
data.frame(round(cbind(out$coefficients, pvalue = p, OR = OR),5))


# Task 2 --------------------------------------------------------


coef(mod.fin)
q2_levels <- levels(freq2[,"Q2"])
all_coefs <- data.frame()

for (i in c(1, 2, 3, 4)) {
  if (i == 1) {
  df <- freq2  # So we don't overwrite the original data with our changes
  df[,"Q2"] = (!(df[,"Q2"] %in% q2_levels[1:i]))*1
  f = glm(Q2~ education + gender + age + race, data=df, family = "binomial")
  all_coefs <- summary(f)$coefficients[, 1:2]
  } else {
    df <- freq2  # So we don't overwrite the original data with our changes
    df[,"Q2"] = (!(df[,"Q2"] %in% q2_levels[1:i]))*1
    f = glm(Q2~ education + gender + age + race, data=df, family = "binomial")
    all_coefs <- cbind(all_coefs, summary(f)$coefficients[, 1:2])
  }
}

all_coefs <- all_coefs[-1, ]
complete <- cbind(out$coefficients[1:8,1:2], all_coefs)

final <- data.frame("m <= never" = c(complete[1,3:4], complete[2,3:4],
                                     complete[3,3:4], complete[4,3:4],
                                     complete[5,3:4], complete[6,3:4],
                                     complete[7,3:4], complete[8,3:4]),
                    "m <= rarely" = c(complete[1,5:6], complete[2,5:6],
                                      complete[3,5:6], complete[4,5:6],
                                      complete[5,5:6], complete[6,5:6],
                                      complete[7,5:6], complete[8,5:6]),
                    "m <= occasionally" = c(complete[1,7:8], complete[2,7:8],
                                            complete[3,7:8], complete[4,7:8],
                                            complete[5,7:8], complete[6,7:8],
                                            complete[7,7:8], complete[8,7:8]),
                    "m <= often" = c(complete[1,9:10], complete[2,9:10],
                                     complete[3,9:10], complete[4,9:10],
                                     complete[5,9:10], complete[6,9:10],
                                     complete[7,9:10], complete[8,9:10]))

names(final) <- c(paste0("m = never"), paste0("m = rarely"),
                  paste0("m = occasionally"), paste0("m = often"))

row.names(final) <- c(paste0("educationHS"," Est."),
                      paste0("educationHS"," Std."),
                      paste0("educationUD"," Est."),
                      paste0("educationUD"," Std."),
                      paste0("educationGD"," Est."),
                      paste0("educationGD"," Std."),
                      paste0("genderF"," Est."),
                      paste0("genderF"," Std."),
                      paste0("genderO"," Est."),
                      paste0("genderO"," Std."),
                      paste0("age"," Est."),
                      paste0("age"," Std."),
                      paste0("raceW"," Est."),
                      paste0("raceW"," Std."),
                      paste0("raceO"," Est."),
                      paste0("raceO"," Std."))

final


# Task 3 --------------------------------------------------------------


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


mod4 <- glm(los ~ type + white + age80 + hmo, family="poisson", data=medpar, subset=-c(1452,1466))
summary(mod4)
summary(mod1)


# Task 4 -------------------------------------------------------------


mod2 <- glm(los ~ type + white + age80 + hmo, family="poisson", data=medpar)
mod3 <- glm.nb(los ~ type + white + age80 + hmo, data=medpar)

print("Poisson Regression Model (Model 2)"); print(logLik(mod2))
print("Negative Binomial Regression Model (Model 3)"); print(logLik(mod3))
print(paste("Test statistic is ",(-2 * (logLik(mod2) - logLik(mod3)))[1]))

pchisq((-2 * (logLik(mod2) - logLik(mod3)))[1], df = 1, lower.tail = FALSE)
