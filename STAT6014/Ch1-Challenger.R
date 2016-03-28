# Challenger data
library(broom)
library(aod)

# Load data set
challenger <- read.table("data/ch1-challenger.txt", header = TRUE)

# fit logistic regression model
fit <- glm(failure ~ temp, data = challenger, family = binomial)
summary(fit)
exp(fit$coefficients["temp"]) # Odd ratio for 1 degree increase 84.26% -- <16% reduction in odd

# Testing Global Null Hypothesis: BETA=0
anova(fit,test = "LRT") # LRT test
anova(fit,test = "Rao") # Score test
wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 2) # Wald test

