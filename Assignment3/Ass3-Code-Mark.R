
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
coef(mod.fin)


##  Task 2

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
