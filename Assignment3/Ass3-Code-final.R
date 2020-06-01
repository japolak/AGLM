

# Task 1 --------------------------------------------------------


# Importing data
library(MASS)
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
freq$Q2 <- factor(freq$Q2)
freq$education <- factor(freq$education)
freq$urban <- factor(freq$urban)
freq$gender <- factor(freq$gender)
freq$race <- factor(freq$race)
freq$married <- factor(freq$married)

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
all_coefs <- matrix(ncol = 5, nrow=0)  # model var, beta label, beta value, standard error

layout(matrix(c(1,2,3,4,5,5), ncol=2, byrow=TRUE), heights = c(2,2,0.5))

vars <- c("education", "gender", "race", "age")
q2_levels <- levels(freq2[,"Q2"])

for(v in vars){
  par(mai=rep(0.55, 4))
  p <- nlevels(freq2[,v])
  p <- ifelse(p==0, 2, p)  # age isn't a factor var so we manually set this to 2 so we have 2 columns, one for intercept one for effect of1 unit increase
  glm_coefs = matrix(ncol = p, nrow=4)

  for(i in c(1, 2, 3, 4)){
    df <- freq2  # So we don't overwrite the original data with our changes
    df[,"Q2"] = (!(df[,"Q2"] %in% q2_levels[1:i]))*1

    f = glm(as.formula(paste0("Q2~", v)), data=df, family = "binomial")
    glm_coefs[i,] <- f$coef
    coefs <- f$coef
    coefs[2:p] <- coefs[2:p] + coefs[1]


    coef_df <- data.frame(summary(f)$coefficients[, 1:2])
    coef_df <- cbind(beta_label=rownames(coef_df), coef_df)
    rownames(coef_df) <- NULL
    coef_df <- cbind(variable=v, m_thresh=i, coef_df)
    all_coefs <- rbind(all_coefs, coef_df)
  }

  coefs_plot <- glm_coefs

  if(v=="age"){
    coefs_plot[,2] = coefs_plot[,1] + (coefs_plot[,2] * max(df[,"age"]))
  }
  else{
    coefs_plot[, 2:p] <- coefs_plot[, 2:p] + coefs_plot[, 1]
  }

  plot(coefs_plot[1,], type="b", ylim=c(min(coefs_plot)*1.1, max(coefs_plot)*1.1), lwd=2, pch=19, xlab=v, ylab="logit", col="red", xaxt="n")
  lines(coefs_plot[2,], type="b", col="blue", lwd=2, pch=19, xaxt="n")
  lines(coefs_plot[3,], type="b", col="purple", lwd=2, pch=19, xaxt="n")
  lines(coefs_plot[4,], type="b", col="green", lwd=2, pch=19, xaxt="n")

  if(v=="education"){
    labs <- c("Less than HS", "High School", "University", "Graduate")
  }
  else if(v=="gender"){
    labs <- c("Male", "Female", "Other")
  }
  else if(v=="race"){
    labs <- c("Asian", "White", "Other")
  }
  else{
    labs <- c(0, max(df[,"age"]))
  }
  axis(1, at=1:p, labels=labs)


}
par(mar=c(0,0,0,0))
plot(1, type = "n", axes=FALSE, xlab="", ylab="")
plot_colours <- c("red", "blue", "purple", "green")
legend(x = "bottom",inset = 0,cex=1,
       legend = c("never vs rarely, occasionally, often, always", "never, rarely vs occasionally, often, always",
                  "never, rarely, occasionally vs often, always", "never, rarely, occasionally, often vs always"),
       col=plot_colours, lwd=2, pch=19, ncol=2)



# Getting tables together
complete_table <- data.frame()
# Table of full model coefs
full_model_c <- data.frame(summary(mod.fin)$coefficients[,1:2])
full_model_c[, "beta_label"] <- rownames(full_model_c)
rownames(full_model_c) <- NULL
full_c <- reshape(full_model_c, varying = c("Value", "Std..Error"), v.names = "Full model",
                  idvar = c("beta_label"),
                  timevar = "Beta", direction="long", times = c("Estimate", "Std..Error"))
rownames(full_c) <- NULL

# Education coefs

for(v in vars){
  c_table <- all_coefs[all_coefs[,"variable"] == v, -1]

  z <- reshape(c_table, varying = c("Estimate", "Std..Error"), v.names = "Value",
               idvar = c("m_thresh", "beta_label"),
               timevar = "Beta", direction="long", times = c("Estimate", "Std..Error"))
  rownames(z) <- NULL

  label_order = paste0(v, levels(freq2[,v]))
  label_order[1] = "(Intercept)"
  z <- z[order(z$m_thresh, match(z$beta_label, label_order)), ]

  final_table <- reshape(z, idvar = c('beta_label', 'Beta'), direction = 'wide',
                         timevar = 'm_thresh', sep = '_model_')

  final_table <- merge(full_c,final_table, by = c('beta_label', "Beta"))
  complete_table <- rbind(complete_table, final_table)
}

print(complete_table)


# Task 3 --------------------------------------------------------------


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
