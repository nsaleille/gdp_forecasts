# http://cran.r-project.org/web/packages/vars/vignettes/vars.pdf
#
# also see http://bayes.squarespace.com/bmr/ !!!!!

library("vars")
data("Canada")
summary(Canada)

plot(Canada, nc = 2, xlab = "")
adf1 <- summary(ur.df(Canada[, "prod"], type = "trend", lags = 2))
adf1
adf2 <- summary(ur.df(diff(Canada[, "prod"]), type = "drift", lags = 1))
adf2
