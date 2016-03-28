# Aircraft data
# Load data set
mine <- read.table("data/ch1-mine.txt",header=TRUE)

fit <- glm(y~.,data = mine, family = "poisson")
summary(fit)
deviance(fit)

fit2 <- glm(y~.-x3,data = mine, family = "poisson")
deviance(fit2)
summary(fit2)
anova(fit2,fit, test="LRT")
drop1(fit2,test="LRT")

fit3 <- glm(y~x2,data = mine, family = "poisson")
anova(fit3,fit2,test="LRT")

