library(tidyr)
library(dplyr)
library(aod) # wald.test, general wald test
library(lmtest) # waldtest, glm specific

data <- read.table("data/ch3-1.txt", header=TRUE)
data2 <- data %>% mutate(NON.PRISON=TOTAL-PRISON) %>% select(-TOTAL) %>% gather(IS.PRISON, FREQ, PRISON:NON.PRISON, factor_key = TRUE) %>% mutate(CRIME=relevel(CRIME,ref="OTHER"),PRIORARREST=relevel(PRIORARREST,ref="SOME"),IS.PRISON=relevel(IS.PRISON,ref="NON.PRISON"))

# Null model
fit0 <- glm(IS.PRISON ~ 1, data=data2, family = binomial, weights = FREQ)

fit1 <- glm(IS.PRISON ~ CRIME * PRIORARREST, data=data2, family = binomial, weights = FREQ)
summary(fit1)

fit2 <- glm(IS.PRISON ~ CRIME + PRIORARREST, data=data2, family = binomial, weights = FREQ)
summary(fit2)

fit3 <- glm(IS.PRISON ~ CRIME, data=data2, family = binomial, weights = FREQ)
summary(fit3)
# Variance Covariance matrix
vcov(fit3)
# LRT Test
anova(fit3, test = c("LRT"))
lmtest::lrtest(fit3)
# Score Test
anova(fit3, test = c("Rao"))
# Wald Test
wald.test(b=coef(fit3), Sigma=vcov(fit3), Terms=2)
wald.test(b=coef(fit3), Sigma=vcov(fit3), L=matrix(c(0,1),nrow=1))
lmtest::waldtest(fit3,test="Chisq")
# Profile likelihood CI
confint(fit3)
exp(confint(fit3))
# Wald CI
confint.default(fit3)
exp(confint.default(fit3))
