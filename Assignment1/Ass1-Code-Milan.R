
############################## Assignment 1 ################################


# Task 1 

pf(13.699, df1 = 2, df2 = 48, lower.tail = F)

# Task 3

data3 <- data.frame(mix = factor(rep(c("one", "two", "three", "four"), each = 4), 
                                 levels = c("one", "two", "three", "four")),
                    strength = c(3129, 3000, 2865, 2890, 3200, 3300, 2975, 3150,
                                 2800, 2900, 2985, 3050, 2600, 2700, 2600, 2765))
head(data3, 16)
str(data3)

boxplot(strength ~ mix, data = data3)
model3 <- aov(strength ~ mix, data = data3)
summary(model3)

# Task 4

data4 <- read.csv("munich.csv")
data4$location <- as.factor(data4$location)
head(data4)
str(data4)
summary(data4)

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y, use="pairwise.complete.obs")
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt)
}
pairs(data4, lower.panel = panel.cor, pch = 18)

model4.1 <- lm(rent ~ area*location + yearc, data = data4)
summary(model4.1)
plot(model4.1)
# 1) TA plot questionable
# 2) QQ-plot shows long-tailed distribution of the residuals
# 3) Scale-location plot shows clear violation of homoscedasticity assumption
# 4) Leverage plot shows one potentially influential point

model4.2 <- lm(rent ~ area, data = data4)
summary(model4.2)
plot(model4.2)
# 1) TA plot questionable
# 2) QQ-plot shows long-tailed distribution of the residuals
# 3) Scale-location plot shows clear violation of homoscedasticity assumption
# 4) Leverage plot shows no potentially influential points


