library(sas7bdat)
attendance <- read.sas7bdat("data/ch2-attendance.sas7bdat")
attendance$PROG <- factor(attendance$PROG, levels =  c(3,1,2))
fit <- glm(DAYSABS ~ MATH + PROG, data = attendance, family=poisson)
summary(fit)

dev_over_df <- 1774/310 # Goodness of fit - lack of fit

fit2 <- MASS::glm.nb(DAYSABS ~ MATH + PROG, data = attendance)
summary(fit2)
drop1(fit2, test = "LRT")
