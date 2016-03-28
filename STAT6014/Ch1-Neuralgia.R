# Neuralgia data
# https://support.sas.com/documentation/cdl/en/statug/63347/HTML/default/viewer.htm#statug_logistic_sect060.htm

neuralgia <- read.table("data/ch1-neuralgia.txt", header = TRUE)
neuralgia$treatment <- relevel(neuralgia$treatment, ref="P")

fit <- glm(npain/n~treatment, family = binomial, data = neuralgia)
summary(fit)
cbind(neuralgia,log_odd=predict(fit), odd_ratio = exp(predict(fit)), p_value = predict(fit,type="response"))
# Odds Ratio A:B
exp(predict(fit)[1]-predict(fit)[2])
# Equivalent to 
neuralgia$treatment <- relevel(neuralgia$treatment, ref="B")
fit <- glm(npain/n~treatment, family = binomial, data = neuralgia)
summary(fit)
exp(coef(fit)["treatmentA"])
# 25% reduction by treatment A compared to treatment B