# principles and practice of sem (4th ed.), rex kline
# recursive path model of illness, figure 7.5, table 4.2

date()
library(lavaan)

# input the correlations in lower diagnonal form
rothLower.cor <- '
 1.00
  -.03 1.00
   .39  .07 1.00
  -.05 -.23 -.13 1.00
  -.08 -.16 -.29  .34 1.00 '
# name the variables and convert to full correlation matrix
rothFull.cor <- getCov(rothLower.cor, names = c("exercise","hardy","fitness","stress","illness"))
# display the correlations
rothFull.cor
# add the standard deviations and convert to covariances
rothFull.cov <- cor2cov(rothFull.cor, sds = c(66.50,38.00,18.40,33.50,62.48))
# display the covariances
rothFull.cov

# specify path model
roth.model <- '
# regressions
 fitness ~ exercise
 stress ~ hardy
 illness ~ fitness + stress'
# unanalyzed association between exercise and hardy
# automatically specified

# fit initial model to data
# variances and covariance of measured exogenous
# variables are free parameters
# variances calculated with N - 1 in the denominator instead of N
model <- sem(roth.model,
       sample.cov = rothFull.cov,
       sample.nobs = 373, fixed.x = FALSE, sample.cov.rescale = FALSE)
summary(model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(model)
residuals(model, type = "raw")
residuals(model, type = "standardized")
residuals(model, type = "cor")
modindices(model)