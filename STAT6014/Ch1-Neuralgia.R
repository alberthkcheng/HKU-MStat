# Neuralgia data
# https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_logistic_sect060.htm
library(broom)
library(dplyr)
library(tidyr)
library(aod)

neuralgia <- read.table("data/ch1-neuralgia.txt", header = TRUE)
neuralgia$treatment <- relevel(neuralgia$treatment, ref = "P")

fit <- glm(npain/n~treatment, family = binomial, data = neuralgia, weights = n)
summary(fit)
anova(fit,test = "LRT") # LRT test
anova(fit,test = "Rao") # Score test

neuralgia <- neuralgia %>% rowwise() %>% mutate(trial = list(c(rep(1,npain),rep(0,n - npain)))) %>% unnest()

fit <- glm(trial~treatment, family = binomial, data = neuralgia)
summary(fit)
newdata <- data.frame(treatment = c("A","B","P"))
prediction <-  predict(fit,newdata = newdata)
cbind(newdata,log_odd = prediction, odd_ratio = exp(prediction), p_value = predict(fit,newdata = newdata,type = "response"))
# Odds Ratio A:B
exp(prediction[1] - prediction[2])
# Equivalent to 
neuralgia$treatment <- relevel(neuralgia$treatment, ref = "B")
fit2 <- glm(trial~treatment, family = binomial, data = neuralgia)
summary(fit2)
exp(coef(fit2)["treatmentA"])
# 25% reduction by treatment A compared to treatment B

# Wald Confidence interval
exp(confint.default(fit))

# Testing Global Null Hypothesis: BETA=0
anova(fit,test = "LRT") # LRT test
anova(fit,test = "Rao") # Score test
wald.test(b = coef(fit), Sigma = vcov(fit), Terms = 2:3) # Wald test

