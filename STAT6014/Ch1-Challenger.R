# Challenger data
# Load data set
challenger <- read.table("data/ch1-challenger.txt", header = TRUE)

# fit logistic regression model
fit <- glm(failure ~ temp, data = challenger, family = binomial)
summary(fit)
exp(fit$coefficients["temp"]) # Odd ratio for 1 degree increase 84.26% -- <16% reduction in odd
