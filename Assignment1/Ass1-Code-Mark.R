# task 1
p <- 2
n <- 51

(intercept_t <- -498.683/140.988)
(x1_est <- 6.677 * 4.885)
(x2_se <- 9.112/6.9)


ssr <- 1841257.15

(sst <- (ssr)/(1 - 0.5708))

# SST = SSReg + SSR
(ssreg <- sst - ssr)

# R2:
# 57.08% of the varaince in the data is explained by our model


(f_stat <- (ssreg/p) / (ssr / (n-(p+1))))

pf(f_stat,2,48,lower.tail = FALSE)


# task 2


# task 3
c1 <- c(3129, 3200, 2800, 2600)
c2 <- c(3000, 3300, 2975, 3150)
c3 <- c(2800, 2900, 2985, 3050)
c4 <- c(2600, 2700, 2600, 2765)



boxplot(m[,2:5], xlab="Mixing technique", ylab="Compressive Strength")

library(reshape)

m <- data.frame(index=c(1,2,3,4), c1, c2, c3, c4)
m2 <- melt(m, id.vars="index")

aov(value~index, data=m2)

?melt



# task 4
munich <- read.csv("munich.csv")
munich$location <- factor(munich$location)


pairs(munich)

f <- lm(rent~area*location + yearc, data=munich)
par(mfrow=c(2,2))
plot(f1)
summary(f1)

f2 <- lm(rent~area, data=munich)

anova(f, f2)

