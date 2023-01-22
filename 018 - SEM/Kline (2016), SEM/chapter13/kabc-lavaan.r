# principles and practice of sem (4th ed.), rex kline
# two-factor model of the kabc-i, figure 9.7, table 13.1

date()
library(lavaan)

# input the correlations in lower diagnonal form
kabcLower.cor <- '
 1.00
 .39 1.00
 .35  .67 1.00
 .21  .11  .16 1.00
 .32  .27  .29  .38 1.00
 .40  .29  .28  .30  .47 1.00
 .39  .32  .30  .31  .42  .41 1.00
 .39  .29  .37  .42  .58  .51  .42 1.00 '
# name the variables and convert to full correlation matrix
kabcFull.cor <- getCov(kabcLower.cor, names = c("hm","nr","wo","gc","tr","sm","ma","ps"))
# display the correlations
kabcFull.cor
# add the standard deviations and convert to covariances
kabcFull.cov <- cor2cov(kabcFull.cor, sds = c(3.40,2.40,2.90,2.70,2.70,4.20,2.80,3.00))
kabcFull.cov

# specify cfa model
kabc.model <- '
# latent variables
Sequent =~ hm + nr + wo
Simultan =~ gc + tr + sm + ma + ps '
# indicators hm and gc automatically
# specified as reference variables
# unanalyzed association between the two factors
# automatically specified

# fit model to data
model <- sem(kabc.model,
       sample.cov=kabcFull.cov,
       sample.nobs=200)
summary(model, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(model)
residuals(model, type = "raw")
residuals(model, type = "standardized")
residuals(model, type = "cor")
modindices(model)