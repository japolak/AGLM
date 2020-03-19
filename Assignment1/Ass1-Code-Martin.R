# =========== 1 ===========


intercept_t <- -498.683 / 140.988
x1_est <- 4.885 * 6.677
x2_s_e <- 9.112 / 6.9

R_2 <- 0.5708
SSR <- 1841257.15
SST <- SSR / (1 - R_2)
SSReg <- R_2 * SST

f_value <- (SSReg/2)/(SSR/(51-3))

# =========== 2 ===========

class_group <- c(rep("A", 3), rep("B", 2), rep("C", 3))
grades <- c(4, 6, 8, 1, 5, 9, 10, 5)
classroom <- data.frame(class_group=class_group, grades=grades)

mod0 <- lm(grades ~ class_group, classroom)

# =========== 3 ===========

mixing <- c(rep(1, 4), rep(2, 4), rep(3, 4), rep(4, 4))
measurements <- c(3129,3000,2865,2890,3200,3300,2975,3150,2800,2900,2985,3050,2600,2700,2600,2765)
concrete <- data.frame(mixing=mixing, measurements=measurements)

ggplot(aes(x=mixing, y=measurements, fill=factor(mixing)), data=concrete) +
  geom_boxplot() + theme_bw()

# =========== 4 ===========

apps <- read.csv("munich.csv")
apps$location <- factor(apps$location)

mod <- lm(rent ~location+area+area:location+yearc, data=apps)
