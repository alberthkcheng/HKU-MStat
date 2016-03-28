# Prison data
# Load data set
prison <- read.table("data/ch1-prison.txt",header = TRUE)
fit <- glm(prison~ibus+iprior,data=prison,weights = n,family=binomial)
summary(fit)

fit.full <- glm(prison~ibus*iprior,data=prison,weights = n,family=binomial)
summary(fit.full)

# Goodness-of-Fit statistics
anova(fit, fit.full,test = "LRT") # LRT test
anova(fit, fit.full,test = "Rao") # Pearson

fit2<-glm(prison~ibus,data=prison,weights = n,family=binomial)
lrt <- deviance(fit2) - deviance(fit)
df <- 1
pchisq(lrt,df,lower.tail = FALSE)
