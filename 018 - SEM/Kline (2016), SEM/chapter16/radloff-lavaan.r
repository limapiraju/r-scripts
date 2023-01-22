# principles and practice of sem (4th ed.), rex kline
# model of depression analyzed over white and african-american samples
# figure 16.2, table 16.5
# models 1-6

date()
library(lavaan)

# set R working directory to the folder on your computer with
# the external data file
setwd("C:/Users/Rex/Desktop/Radloff")

radloffdata <- read.table("radloff-lavaan.txt", sep="\t", header = FALSE)
colnames(radloffdata) <- c("x1", "x2", "x3", "x4", "x5", "g")
for(i in 1:5) radloffdata[,i] <- ordered(radloffdata[,i])
radloffdata[,6] <- factor(radloffdata[,6], labels = c("wh", "aa"))
summary(radloffdata)

# model 1
# configural invariance
# includes just the minimal constraints needed for identification

Model1 <- "
dep =~ c(1, 1)*x1 + x2 + x3 + x4 + x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + t3
x2 | c(t21, t21)*t1 + t2 + t3
x3 | c(t31, t31)*t1 + t2 + t3
x4 | c(t41, t41)*t1 + t2 + t3
x5 | c(t51, t51)*t1 + t2 + t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, NA)*x1
x2 ~~ c(1, NA)*x2
x3 ~~ c(1, NA)*x3
x4 ~~ c(1, NA)*x4
x5 ~~ c(1, NA)*x5
"

outModel1 <- cfa(Model1, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

# model 2
# configural invariance
# error covariance between items 1 and 2, african-american group only

Model2 <- "
dep =~ c(1, 1)*x1 + x2 + x3 + x4 + x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + t3
x2 | c(t21, t21)*t1 + t2 + t3
x3 | c(t31, t31)*t1 + t2 + t3
x4 | c(t41, t41)*t1 + t2 + t3
x5 | c(t51, t51)*t1 + t2 + t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, NA)*x1
x2 ~~ c(1, NA)*x2
x3 ~~ c(1, NA)*x3
x4 ~~ c(1, NA)*x4
x5 ~~ c(1, NA)*x5
x1 ~~ c(0, NA)*x2
"

outModel2 <- cfa(Model2, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

# model 3
#  weak invariance
#  equal pattern coefficients
#  error covariance between items 1 and 2, african-american group only

Model3 <- "
dep =~ c(1,1)*x1 + c(f21, f21)*x2 + c(f31, f31)*x3 + c(f41, f41)*x4 + c(f51, f51)*x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + t3
x2 | c(t21, t21)*t1 + t2 + t3
x3 | c(t31, t31)*t1 + t2 + t3
x4 | c(t41, t41)*t1 + t2 + t3
x5 | c(t51, t51)*t1 + t2 + t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, NA)*x1
x2 ~~ c(1, NA)*x2
x3 ~~ c(1, NA)*x3
x4 ~~ c(1, NA)*x4
x5 ~~ c(1, NA)*x5
x1 ~~ c(0, NA)*x2
"

outModel3 <- cfa(Model3, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

# model 4
# strong invariance
# equal pattern coefficients, thresholds
# error covariance between items 1 and 2, african-american group only

Model4 <- "
dep =~ c(1,1)*x1 + c(f21, f21)*x2 + c(f31, f31)*x3 + c(f41, f41)*x4 + c(f51, f51)*x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + c(t13, t13)*t3
x2 | c(t21, t21)*t1 + c(t22, t22)*t2 + c(t23, t23)*t3
x3 | c(t31, t31)*t1 + c(t32, t32)*t2 + c(t33, t33)*t3
x4 | c(t41, t41)*t1 + c(t42, t42)*t2 + c(t43, t43)*t3
x5 | c(t51, t51)*t1 + c(t52, t52)*t2 + c(t53, t53)*t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, NA)*x1
x2 ~~ c(1, NA)*x2
x3 ~~ c(1, NA)*x3
x4 ~~ c(1, NA)*x4
x5 ~~ c(1, NA)*x5
x1 ~~ c(0, NA)*x2
"

outModel4 <- cfa(Model4, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

# model 5
# strict invariance
# equal error variances
# equal pattern coefficients, thresholds
# error covariance between items 1 and 2, african-american group only

Model5 <- "
dep =~ c(1,1)*x1 + c(f21, f21)*x2 + c(f31, f31)*x3 + c(f41, f41)*x4 + c(f51, f51)*x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + c(t13, t13)*t3
x2 | c(t21, t21)*t1 + c(t22, t22)*t2 + c(t23, t23)*t3
x3 | c(t31, t31)*t1 + c(t32, t32)*t2 + c(t33, t33)*t3
x4 | c(t41, t41)*t1 + c(t42, t42)*t2 + c(t43, t43)*t3
x5 | c(t51, t51)*t1 + c(t52, t52)*t2 + c(t53, t53)*t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, 1)*x1
x2 ~~ c(1, 1)*x2
x3 ~~ c(1, 1)*x3
x4 ~~ c(1, 1)*x4
x5 ~~ c(1, 1)*x5
x1 ~~ c(0, NA)*x2
"

outModel5 <- cfa(Model5, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

# model 6
# partial strict invariance
# equal error variances except for x3
# equal pattern coefficients, thresholds
# error covariance between items 1 and 2, african-american group only

Model6 <- "
dep =~ c(1,1)*x1 + c(f21, f21)*x2 + c(f31, f31)*x3 + c(f41, f41)*x4 + c(f51, f51)*x5
x1 | c(t11, t11)*t1 + c(t12, t12)*t2 + c(t13, t13)*t3
x2 | c(t21, t21)*t1 + c(t22, t22)*t2 + c(t23, t23)*t3
x3 | c(t31, t31)*t1 + c(t32, t32)*t2 + c(t33, t33)*t3
x4 | c(t41, t41)*t1 + c(t42, t42)*t2 + c(t43, t43)*t3
x5 | c(t51, t51)*t1 + c(t52, t52)*t2 + c(t53, t53)*t3
dep ~~ NA*dep
dep ~ c(0, NA)*1
x1 ~~ c(1, 1)*x1
x2 ~~ c(1, 1)*x2
x3 ~~ c(1, NA)*x3
x4 ~~ c(1, 1)*x4
x5 ~~ c(1, 1)*x5
x1 ~~ c(0, NA)*x2
"

outModel6 <- cfa(Model6, data=radloffdata, group="g", parameterization="theta", estimator="wlsmv")

summary(outModel1, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel1)
residuals(outModel1, type="raw")
residuals(outModel1, type="cor")
residuals(outModel1, type="standardized")

summary(outModel2, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel2)
residuals(outModel2, type="raw")

summary(outModel3, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel3)
residuals(outModel3, type="raw")

summary(outModel4, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel4)
residuals(outModel4, type="raw")

summary(outModel5, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel5)
residuals(outModel5, type="raw")

summary(outModel6, fit=TRUE, standardized=TRUE, rsquare=TRUE)
fitted(outModel6)
residuals(outModel6, type="raw")

# scaled (corrected) chi-square difference tests

anova(outModel1, outModel2)
anova(outModel2, outModel3)
anova(outModel3, outModel4)
anova(outModel4, outModel5)
anova(outModel4, outModel6)
