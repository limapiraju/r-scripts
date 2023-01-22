# principles and practice of sem (4th ed.), rex kline
# fully latent model of thought strategies and job satisfaction
# figures 10.6, 14.1-14.2, table 14.1

date()
library(lavaan)

# input the correlations in lower diagnonal form
houghtonLower.cor <- '
1.000
.668 1.000
.635  .599 1.000
.263  .261   .164 1.000
.290  .315   .247  .486 1.000
.207  .245   .231  .251  .449 1.000
-.206 -.182  -.195 -.309 -.266 -.142 1.000 
-.280 -.241  -.238 -.344 -.305 -.230  .753 1.000
-.258 -.244 -.185  -.255 -.255 -.215  .554  .587 1.000 
.080  .096  .094  -.017  .151  .141 -.074 -.111  .016 1.000
.061  .028 -.035  -.058 -.051 -.003 -.040 -.040 -.018 .284 1.000
.113  .174  .059   .063  .138  .044 -.119 -.073 -.084 .563  .379 1.000 '
# name the variables and convert to full correlation matrix
houghtonFull.cor <- 
 getCov(houghtonLower.cor, names = c("wk1","wk2","wk3","hap","md1","md2","pr1","pr2","app","bel","st","ima"))
# display the correlations
houghtonFull.cor
# add the standard deviations and convert to covariances
houghtonFull.cov <- 
 cor2cov(houghtonFull.cor, sds = c(.939,1.017,.937,.562,.760,.524,.585,.609,.731,.711,1.124,1.001))
houghtonFull.cov

# specify cfa model
houghtonCFA.model <- '
# measurement part
Constru =~ bel + st + ima
Dysfunc =~ pr1 + pr2 + app
WellBe =~ hap + md1 + md2
JobSat =~ wk1 + wk2 + wk3
# error covariance
hap ~~ md2 '

# specify sr model
houghtonSR.model <- '
# measurement part
Construc =~ bel + st + ima
Dysfunc =~ pr1 + pr2 + app
WellBe =~ hap + md1 + md2
JobSat =~ wk1 + wk2 + wk3
# error covariance
hap ~~ md2 
# structural part
Dysfunc ~ Construc
WellBe ~ Construc + Dysfunc
JobSat ~ Construc + Dysfunc + WellBe '

# fit cfa model to data
cfamodel <- sem(houghtonCFA.model, 
         sample.cov=houghtonFull.cov,
         sample.nobs=263)
summary(cfamodel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(cfamodel)
residuals(cfamodel, type = "raw")
residuals(cfamodel, type = "standardized")
residuals(cfamodel, type = "cor")
modindices(cfamodel)

# fit sr model to data
srmodel <- sem(houghtonSR.model,
         sample.cov=houghtonFull.cov,
         sample.nobs=263)
summary(srmodel, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitted(srmodel)
residuals(srmodel, type = "raw")
residuals(srmodel, type = "standardized")
residuals(srmodel, type = "cor")
modindices(srmodel)
