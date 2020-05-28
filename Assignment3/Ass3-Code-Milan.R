
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

                 