x1 <- -0.868
z_x1 <- -2.365
x1_se <- x1 / z_x1
or_x1 <- exp(x1)



x2 <- 2.404
or_x2 <- exp(x2)
x2_se <- 0.601

l_ci_x2 <- exp(x2-qnorm(0.975)*x2_se)
h_ci_x2 <- exp(x2+qnorm(0.975)*x2_se)

l_ci_x3 <- 0.010
h_ci_x3 <- 0.074

x3 <- (log(l_ci_x3) + log(h_ci_x3)) / 2
x3_se <- (- log(l_ci_x3) + log(h_ci_x3)) /(2*qnorm(0.975))
z_x3 <- x3/x3_se

or_x3 <- exp(x3)
