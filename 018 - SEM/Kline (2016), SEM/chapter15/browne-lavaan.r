# principles and practice of sem (4th ed.), rex kline
# latent growth model of performance on an air traffic controller task
# figures 15.3, 15.5, table 15.3

date()
library(lavaan)

# input the correlations in lower diagnonal form
browneLowercor <- '
1.00
.77 1.00
.59 .81 1.00
.50 .72 .89 1.00
.48 .69 .84 .91 1.00
.46 .68 .80 .88 .93 1.00
.50 .46 .36 .26 .28 .28 1.00 '
# name the variables and convert to full correlation matrix
browneFullcor <-
 getCov(browneLowercor, names = c("tr1","tr2","tr3","tr4","tr5","tr6","ability"))
# display the correlations
browneFullcor
# add the standard deviations and convert to covariances
browneFullcov <-
 cor2cov(browneFullcor, sds = c(7.60,8.44,8.95,9.21,9.49,9.62,5.62))
# create mean vector
browneMeans = c(11.77,21.39,27.50,31.02,32.58,34.20,.70)
# display covaiances and means
browneFullcov
browneMeans

#specify change model
browneChange <- '
# latent growth factors
Initial =~ 1*tr1 + 1*tr2 + 1*tr3 + 1*tr4 + 1*tr5+ 1*tr6
# keyword NA means free parameter for trials 3-6
Shape =~ 0*tr1 + 1*tr2 + NA*tr3 + NA*tr4 + NA*tr5+ NA*tr6
# error correlations
tr1~~tr2
tr2~~tr3
tr3~~tr4
tr4~~tr5
tr5~~tr6 '

#specify prediction model
brownePredict <- '
# latent growth factors
Initial =~ 1*tr1 + 1*tr2 + 1*tr3 + 1*tr4 + 1*tr5+ 1*tr6
# keyword NA means free parameter for trials 3-6
Shape =~ 0*tr1 + 1*tr2 + NA*tr3 + NA*tr4 + NA*tr5+ NA*tr6
# error correlations
tr1~~tr2
tr2~~tr3
tr3~~tr4
tr4~~tr5
tr5~~tr6
# regress latent growth factors on ability
Initial ~ ability
Shape ~ ability '

# fit model to data

# change model
# function growth() assumes a mean structure,
# fixes intercepts of observed variables to zero,
# but freely estimates latent variable means or intercepts
browneChangefit <- 
 growth(browneChange, sample.cov = browneFullcov, sample.mean = browneMeans, sample.nobs=250)
summary(browneChangefit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(browneChangefit) 
residuals(browneChangefit, type = "raw")
residuals(browneChangefit, type = "standardized")
residuals(browneChangefit, type = "cor")

# prediction model
brownePredictfit <- 
 growth(brownePredict, sample.cov = browneFullcov, sample.mean = browneMeans, sample.nobs=250)
summary(brownePredictfit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(brownePredictfit) 
residuals(brownePredictfit, type = "raw")
residuals(brownePredictfit, type = "standardized")
residuals(brownePredictfit, type = "cor")




