# Mair (2018), Chapter 02

# Dados de Vaughn-Coaxum et al. (2016), usando um Inventário de Depressão para Crianças
library("MPsychoR")
data("YouthDep")
item1 <- YouthDep[, 1]
levels(item1) <- c("0", "1", "1")
item2 <- YouthDep[, 14]
levels(item2) <- c("0", "1", "1")
table(item1, item2)

# Um coeficiente de correlação adequado para dados dicotômicos e baseado nesta estratégia
# normal subjacente é a correlação tetracórica, computável pelo pacote psych (Revelee, 2007)
library("psych")
tetcor <- tetrachoric(cbind(item1, item2))
tetcor

# Versão para itens politômicos (correlação policórica)
item1 <- YouthDep[, 1]
item2 <- YouthDep[, 14]
polcor <- polychoric(cbind(item1, item2))
polcor

# Correção de continuidade (células com 0 substituídas por 0.5), como no qui-quadrado
DepItems <- YouthDep[,1:26]
Depnum <- data.matrix(DepItems) - 1 ## convert to numeric
Rdep <- polychoric(Depnum)

# Correlação tetracórica para itens dicotômicos (e eingenvalues)
data("Rmotivation")
vind <- grep("ext|int", colnames(Rmotivation))
Rmotivation1 <- Rmotivation[, vind]
Rmot1 <- tetrachoric(Rmotivation1, smooth = FALSE)
tail(round(eigen(Rmot1$rho)$values, 3))

Rmot <- tetrachoric(Rmotivation1)
tail(round(eigen(Rmot$rho)$values, 3))


# EFA usando psych (Revelle, 2007)
motFA <- fa(Rmot$rho, nfactors = 2, rotate = "none", fm = "ml")
print(motFA$loadings, cutoff = 0.2)

# Comunalidades
round(motFA$communality, 2)

# EFA com rotação oblíqua
Rmot2 <- tetrachoric(Rmotivation[,1:36])
motFA3 <- fa(Rmot2$rho, nfactors = 3, rotate = "oblimin",
             fm = "ml")
motFA3$loadings

# Matriz de correlação
round(motFA3$Phi, 3)


# Calculando eigenvalues e scree plot
Rdep <- polychoric(Depnum)$rho
evals <- eigen(Rdep)$values
scree(Rdep, factors = FALSE)

# variância explicada
(evals/sum(evals)*100)[1:2]

# análise paralela de Horn
set.seed(123)
resPA <- fa.parallel(Depnum, fa = "pc", cor = "poly", fm = "ml")

# very simple structure e MAP
resvss <- vss(Rdep, fm = "ml", n.obs = nrow(Depnum), plot = FALSE)
resvss

# índices de ajuste: RMSA, RMSEA, TLI
fadep <- fa(Depnum, 1, cor = "poly", fm = "ml")
summary(fadep)

# 
resnf <- nfactors(Depnum, n = 8, fm = "ml", cor = "poly")
resnf

# EFA Bayesiana
install.packages("BayesFM")
library("MPsychoR")
library("corrplot")
library("BayesFM")
data("Privacy")
Privstd <- scale(Privacy)
corrplot(cor(Privstd))


Nid <- 2 ## minimum number of variables per factor
pmax <- trunc(ncol(Privstd)/Nid) ## maximum number of factors
pmax

set.seed(123)
Rsim <- simul.R.prior(pmax, nu0 = pmax + c(1, 2, 5, 7, 10))
plot(Rsim)


Ksim <- simul.nfac.prior(nvar = ncol(Privstd), Nid = Nid,
                         Kmax = pmax, kappa = c(.1, .2, .5, 1))
plot(Ksim)


# CFA
library("MPsychoR")
library("lavaan")

data("Rmotivation")

vind <- grep("ext|int", colnames(Rmotivation)) ## ext/int items
Rmot <- na.omit(Rmotivation[, vind])
mot_model <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4 + ext5 + ext6 + ext7 + ext8 + ext9 + ext10 + ext11 + ext12
intrinsic =~ int1 + int2 + int3 + int4 + int5'
fitMot <- lavaan::cfa(mot_model, data = Rmot,
                      ordered = names(Rmot))



library("semPlot")
semPaths(fitMot, what = "est", edge.label.cex = 0.7,
         edge.color = 1, esize = 1, sizeMan = 4.5, asize = 2.5,
         intercepts = FALSE, rotation = 4, thresholdColor = "red",
         mar = c(1, 5, 1.5, 5), fade = FALSE, nCharNodes = 4)

# variância do erro dos indicadores, theta
inspect(fitMot, what = "est")$theta

# cargas fatoriais não padronizadas e padronizadas, lambdas
inspect(fitMot, what = "est")$lambda
inspect(fitMot, what = "std")$lambda

# matriz de covariância e de correlação, psy
inspect(fitMot, what = "est")$psi
inspect(fitMot, what = "std")$psi

# estimativas dos parâmetros com valores p e ICs
parameterEstimates(fitMot, standardized = TRUE)

# medidas de qualidade do ajuste
summary(fitMot, standardized = TRUE, fit.measures = TRUE)


# Modelos CFA de ordem superior
vind <- c(1:4, 13:16, 32:35)
Rmot2 <- na.omit(Rmotivation[, vind])


mot_model3 <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4
hybrid =~ hyb1 + hyb2 + hyb3 + hyb4
intrinsic =~ int1 + int2 + int3 + int4
motivation =~ extrinsic + hybrid + intrinsic'
fitMot3 <- lavaan::cfa(mot_model3, data = Rmot2,
                       ordered = names(Rmot2))

summary(fitMot3, standardized = TRUE, fit.measures = TRUE)


# CFA com covariáveis - MIMIC (multiple indicator, multiple independent causes)
vind <- c(1:4, 13:16, 32:35, 39:41)
Rmot3 <- na.omit(Rmotivation[, vind])
mot_model4 <- '
extrinsic =~ ext1 + ext2 + ext3 + ext4
hybrid =~ hyb1 + hyb2 + hyb3 + hyb4
intrinsic =~ int1 + int2 + int3 + int4
motivation =~ extrinsic + hybrid + intrinsic
motivation ~ npkgs + phd'
fitMot4 <- lavaan::cfa(mot_model4, data = Rmot3,
                       ordered = names(Rmot3[1:12]))

parameterEstimates(fitMot4)[16:17,]

# CFA multigrupo
library("semTools")
data("Bergh")
GP_model <- 'GP =~ EP + HP + DP + SP'
minvfit <- measurementInvariance(GP_model, data = Bergh,
                                 group = "gender", estimator = "MLR")

summary(minvfit$fit.configural, standardized = TRUE,
        fit.measures = TRUE)


# Uma maneira de restringir cargas para serem iguais entre grupos
GP_model <-'GP =~ c(v1,v1)*EP + c(v2,v2)*HP + c(v3,v3)*DP + SP'
fitBase <-lavaan::cfa(GP_model, data = Bergh, group = "gender",
                      estimator = "MLR")

# Maneira alternativa, usando group.equal e group.partial
GP_model <- 'GP =~ EP + HP + DP + SP'
fitBase <- lavaan::cfa(GP_model,data = Bergh, group = "gender",
                       group.equal = c("loadings"),
                       group.partial = c("GP=~ SP"), estimator = "MLR")



fitBase1 <- lavaan::cfa(GP_model, data = Bergh,
                        group = "gender", group.equal = c("loadings", "intercepts"),
                        group.partial = c("GP=~SP", "DP~1", "HP~1", "SP~1"),
                        estimator = "MLR")

summary(fitBase1, standardized = TRUE,
        fit.measures = TRUE)

# Restringindo cargas do fator SP a zero para mulheres e deixando livres
# para variar em homens
GP_model2 <- 'GP =~ c(v1,v1)*EP + c(v2,v2)*HP + c(v3,v3)*DP + c(NA, 0)*SP'
fitIO <- lavaan::cfa(GP_model2, data = Bergh, group = "gender",
                     group.equal = c("intercepts"),
                     group.partial = c("DP~1", "HP~1", "SP~1"),
                     estimator = "MLR")

summary(fitIO, standardized = TRUE,
        fit.measures = TRUE)


fitMarg <- lavaan::cfa(GP_model, data = Bergh,group = "gender",
                       group.equal = c("loadings", "intercepts"),
                       group.partial = c("DP~1", "HP~1", "SP~1"),
                       estimator = "MLR")

# Comparando modelos aninhados
anova(fitMarg, fitBase1)


# CFA longitudinal
library("MPsychoR")
library("lavaan")
data("SDOwave")
model_sdo1 <- '
SDO1996 =~ 1*I1.1996 + a2*I2.1996 + a3*I3.1996 + a4*I4.1996
SDO1998 =~ 1*I1.1998 + a2*I2.1998 + a3*I3.1998 + a4*I4.1998
SDO1996 ~~ SDO1998
## intercepts
I1.1996 ~ int1*1; I1.1998 ~ int1*1
I2.1996 ~ int2*1; I2.1998 ~ int2*1
I3.1996 ~ int3*1; I3.1998 ~ int3*1
I4.1996 ~ int4*1; I4.1998 ~ int4*1
## residual covariances
I1.1996 ~~ I1.1998
I2.1996 ~~ I2.1998
I3.1996 ~~ I3.1998
I4.1996 ~~ I4.1998
## latent means: 1996 as baseline
SDO1996 ~ 0*1
SDO1998 ~ 1'
fitsdo1 <- cfa(model_sdo1, data = SDOwave, estimator = "MLR")


parameterEstimates(fitsdo1)[22:23,]






model_sdo2 <- '
## 1st CFA level, constant loadings across time
SDOD1996 =~ 1*I1.1996 + d1*I2.1996
SDOD1998 =~ 1*I1.1998 + d1*I2.1998
SDOD1999 =~ 1*I1.1999 + d1*I2.1999
SDOE1996 =~ 1*I3.1996 + a1*I4.1996
SDOE1998 =~ 1*I3.1998 + a1*I4.1998
SDOE1999 =~ 1*I3.1999 + a1*I4.1999
## 2nd CFA level, constant loadings across time
SDO1996 =~ 1*SDOD1996 + sd1*SDOE1996
SDO1998 =~ 1*SDOD1998 + sd1*SDOE1998
SDO1999 =~ 1*SDOD1999 + sd1*SDOE1999
## Constant 1st level intercepts
I1.1996 ~ iI1*1; I1.1998 ~ iI1*1; I1.1999 ~ iI1*1
I2.1996 ~ iI2*1; I2.1998 ~ iI2*1; I2.1999 ~ iI2*1
I3.1996 ~ iI3*1; I3.1998 ~ iI3*1; I3.1999 ~ iI3*1
I4.1996 ~ iI4*1; I4.1998 ~ iI4*1; I4.1999 ~ iI4*1
## residual covariances:
I1.1999 ~~ I1.1998; I1.1996 ~~ I1.1998; I1.1999 ~~ I1.1996
I2.1999 ~~ I2.1998; I2.1996 ~~ I2.1998; I2.1999 ~~ I2.1996
I3.1999 ~~ I3.1998; I3.1996 ~~ I3.1998; I3.1999 ~~ I3.1996
I4.1999 ~~ I4.1998; I4.1996 ~~ I4.1998; I4.1999 ~~ I4.1996
## latent means
SDO1996 ~ 0*1 ## 1996 baseline year
SDO1998 ~ 1 ## 1998 vs. 1996
SDO1999 ~ 1 ## 1999 vs. 1996
'
fitsdo2 <- cfa(model_sdo2, data = SDOwave, estimator = "MLR")


parameterEstimates(fitsdo2)[43:45,]


# CFA multinível
data("FamilyIQ")
modelIQ <- '
level: 1
numeric =~ wordlist + cards + matrices
perception =~ figures + animals + occupation
level: 2
general =~ wordlist + cards + matrices + figures + animals +
occupation'
fitIQ <- cfa(modelIQ, data = FamilyIQ, cluster = "family",
             std.lv = TRUE)
fitIQ


# Bayesian CFA
install.packages("blavaan")
library("blavaan")
dpriors()[c("lambda", "itheta", "ipsi")]

library("MPsychoR")
data("Bergh")
GP_model <- 'GP =~ EP + HP + DP + SP'
set.seed(123)
fitBCFA <- bcfa(GP_model, data = Bergh, burnin = 2000,
                sample = 10000, n.chains = 2,
                jagcontrol = list(method = "rjparallel"))

# trace e autocorrelation plots
plot(fitBCFA, pars = 1:2, plot.type = "trace")
plot(fitBCFA, pars = 1:2, plot.type = "autocorr")

summary(fitBCFA)







