# Mair (2018), Chapter 04

rm(list = ls());ls()

setwd("C:/Users/User/Desktop/")

###
'
IRT
- IRT models, classification according to:
a. The nature of the input data (dichotomous vs. polytomous items);
b. The dimensionality of the underlying latent trait (unidimensional vs. multidimensional);

- how to assess dimensionality?
a. Categorical PCA: princals;
b. EFA on tetrachoric/polychoric correlations;
c. Item factor analysis (IFA);
'

# Dados de cálculos matemáticos
library("MPsychoR")

install.packages("mirt")
library("mirt")
data("zareki")
zarsub <- zareki[, grep("subtr", colnames(zareki))]

# PCA categórica (Princals) para testar dimensionalidade
install.packages("Gifi")
library("Gifi")
prinzar <- princals(zarsub)
plot(prinzar, main = "Zareki Loadings")

# EFA tetracórica
library("psych")
subt <- tetrachoric(zarsub)$rho
evals <- eigen(subt)$values
scree(subt, factors = FALSE)


subt_factors <- nfactors(subt, n = 4, fm = "wls", cor = "tetra")
subt_factors

# análise paralela de Horn
set.seed(123)
subt_PA <- fa.parallel(zarsub, fa = "pc", cor = "tet", fm = "wls")
subt_PA

# IFA
install.packages('Rcpp')
library(Rcpp)

fitifa1 <- mirt(zarsub, 1, verbose = FALSE)
fitifa2 <- mirt(zarsub, 2, verbose = FALSE, TOL = 0.001)
anova(fitifa1, fitifa2, verbose = FALSE)



# Unidimensional Dichotomous IRT Models - The Rasch Model
install.packages("eRm")
library("eRm")
fitrasch1 <- RM(zarsub)

# parâmetros de facilidade
round(fitrasch1$betapar, 3)

# ordenando do mais fácil para o mais difícil
round(sort(-fitrasch1$betapar), 3)

# testando a invariância da mensuração (parâmetros) para diferentes subgrupos de participantes
# esse é um pressuposto dos modelos Rasch
timecat <- factor(zareki$time <= median(zareki$time),
                  labels = c("fast", "slow"))
fitLR <- LRtest(fitrasch1, timecat)
fitLR

# ajuste acima foi ruim; invariância foi violada!
# para testar fontes de misfit, a estatística de Wald é útil
Waldtest(fitrasch1, timecat) # item 5 é o problema de novo!

# avaliando o misfit graficamente
plotGOF(fitLR, ctrline = list(col = "gray"), conf = list())
fitrasch1

# reajustando o modelo, agora sem o item 5
fitrasch2 <- RM(zarsub[, -5])
LRtest(fitrasch2, timecat)

# testes não paramétricos de pressupostos do modelo Rasch
set.seed(123)
T1 <- NPtest(as.matrix(zarsub[, -5]), n = 1000, method = "T1")
T1

T11 <- NPtest(as.matrix(zarsub[, -5]),n = 1000, method = "T11")
T11

# dificuldades dos itens, modelo final
round(sort(-fitrasch2$betapar), 2)

# CCIs
plotjointICC(fitrasch2, xlab = "Subtraction Trait",
             main = "ICCs Subtraction Items")

# segundo passo: derivando os parâmetros de habilidades dos participantes
zarppar <- person.parameter(fitrasch2)

# anova comparando parâmetros das habilidades entre escolas
zareki$theta <- zarppar$theta.table[,1]
summary(aov(theta ~ class, data = zareki))



# Two-Parameter Logistic Model

# Exemplo com o knowledge characteristics do Work Design Questionnaire
install.packages("ltm")
library("ltm")

install.packages("MPsychoR")
library("MPsychoR")

data("RWDQ")

# z1, on the right-hand side of the formula interface, is a generic
# placeholder for the single latent dimension. 
fit2pl1 <- ltm(RWDQ ~ z1)

# Parâmetros dos itens
head(coef(fit2pl1))

# Eliminando o item 22, que parece ser problemático (i.e., dificuldade < -9)
RWDQ1 <- RWDQ[,-1]
fit2pl2 <- ltm(RWDQ1 ~ z1)
head(coef(fit2pl2))

# Avaliando o ajuste dos itens por meio da estatística Q1
# nenhum valor p foi significativo; logo, ajustes são bons
item.fit(fit2pl2)

# ICCs dos cinco primeiros itens
plot(fit2pl2, item = 1:5, legend = TRUE)

# Discriminação dos itens
round(coef(fit2pl2)[1:5, 2], 3)

# Como o modelo ajustou bem, podemos calcular os parâmetros das pessoas
ppars <- ltm::factor.scores(fit2pl2,
                            resp.patterns = RWDQ1)$score.dat[, "z1"]

ppars

# Three-Parameter Logistic Model

# face recognition experiment; Verbal Paired-Associates Memory Test
data("Wilmer")
VPMT <- Wilmer[,3:27]

# Ajustando o modelo 3-PL
fit3pl <- tpm(VPMT)
round(head(coef(fit3pl)), 3)

# ICCs dos itens
plot(fit3pl, item = 1:6, legend = TRUE)


# Unidimensional Polytomous IRT Models

# Rating Scale Model

# Children’s Empathic Attitudes Questionnaire, medida de empatia
# 0 = não, 1 = talvez, 2 = sim

data("CEAQ")
itceaq <- CEAQ[,1:16] - 1

# ajustando o RSM aos dados
fitrsm <- RSM(itceaq)
ppar <- person.parameter(fitrsm)
ifit0 <- eRm::itemfit(ppar)
ifit0

# Eliminando o item 10, que teve muito misfit
ind <- match("ceaq10", colnames(itceaq))
itceaq1 <- itceaq[,-ind]
fitrsm1 <- RSM(itceaq1)
ppar1 <- person.parameter(fitrsm1)
ifit1 <- eRm::itemfit(ppar1)

# Agora o item problemático é o 15, que foi eliminado a seguir
ind <- match("ceaq15", colnames(itceaq1))
itceaq2 <- itceaq1[, -ind]
fitrsm2 <- RSM(itceaq2)
ppar2 <- person.parameter(fitrsm2)
ifit2 <- eRm::itemfit(ppar2)

# Andersen's LR test
install.packages("mice")
library("mice")
set.seed(222)
imp <- mice(CEAQ)
gradevec <- complete(imp)$grade

# binarizando série e calculando o teste
levels(gradevec) <- c("grade56","grade56","grade78","grade78")
LRtest(fitrsm2, gradevec)

# convertendo os parâmetros em limiares
thpar <- thresholds(fitrsm2)
thpar

# Mapa item pessoa
plotPImap(fitrsm2, latdim = "Empathy",
          main = "Person-Item Map CEAQ")

# plotando ICC
plotICC(fitrsm2, item = 1:6, legend = TRUE)





# Partial Credit Model and Generalizations

# Adult Self-Transcendence Inventory, mensura sabedoria

library("MPsychoR")
library("eRm")

data("ASTI")
PGitems <- ASTI[ ,c(11,14,15,17,18,23)] ## extract PG items
fitpcm <- PCM(PGitems)
thresholds(fitpcm)

# Goodness-of-fit indices
plotPImap(fitpcm, latdim = "Presence/Growth",
          main = "Person-Item Map ASTI")

# Generalized PCM
library("ltm")
data("ASTI")
STitems <- ASTI[ ,c(2,4,7,13,16,24,25)] ## ST items
stpcm <- gpcm(STitems, constraint = "rasch") ## PCM
stgpcm <- gpcm(STitems) ## GPCM
anova(stpcm, stgpcm) ## LR-test



# Graded Response Model
fitgrm <- grm(STitems)
ppargrm <- ltm::factor.scores(fitgrm)

plot(fitgrm, type="OCCu")



# 4.4 Item and Test Information

plot(nrmwp, type = "infotrace", main = "Item Information")



# IRT Sample Size Determination

# Monte Carlo simulation
install.packages("SimDesign")
library("SimDesign")

# Step 1. Specify the population parameters of an IRT model.
m <- 20 # número de itens
n <- c(50, 75, 100, 150, 200, 300) # número de participantes
design <- as.data.frame(n)
set.seed(222)
poppars <- rbind(alpha = round(rlnorm(m, 0, 0.25), 2), # discriminação
                 d = round(rnorm(m), 2)) # dificuldade


# Step 2. Simulate data according to the population parameterization (“generate” step).

irtGenerate <- function(condition, fixed_objects = FALSE) {
  n <- condition$n
  a <- fixed_objects['alpha', ]
  d <- fixed_objects['d', ]
  dat <- simdata(a, d, n, itemtype = '2PL')
  return(dat)
}

# Step 3. Fit the model on the data (“analyze” step).
irtAnalyze <- function(condition, dat, fixed_objects = NULL) {
  mod <- mirt(dat, 1, itemtype = '2PL', verbose = FALSE)
  simpars <- coef(mod, simplify = TRUE, digits = Inf)$items
  irtpars <- c(a = simpars[,1], d = simpars[,2])
  return(irtpars)
}


# 4. Determine how well the population parameters are recovered (“summarize” step).
irtSummarize <- function(condition, results,
                         fixed_objects = NULL) {
  apop <- fixed_objects['alpha', ]
  dpop <- fixed_objects['d', ]
  simrmse <- RMSE(results, c(apop, dpop))
  out <- c(RMSE = simrmse)
  return(out)
}

# Fazendo a simulação com as funções recém-criadas
set.seed(222)
simres <- runSimulation(design, replications = 100,
                        parallel = TRUE, generate = irtGenerate,
                        analyse = irtAnalyze, summarise = irtSummarize,
                        packages = c('mirt'), fixed_objects = poppars)
simres


# RMSE
colind <- grep(".a.", colnames(simres))
sima <- as.data.frame(simres[, colind])
nvec <- as.numeric(levels(simres$n))
matplot(nvec, log(sima), type = "l", col = 1, lty = 1,
        ylab = "log(RMSE)", xlab = "sample size",
        main = "2-PL Monte Carlo", xaxt = "n")
axis(1, at = nvec)

# RMSE médio
meanRMSE <- rowMeans(sima)
names(meanRMSE) <- n
round(meanRMSE, 2)



# Differential Item Functioning (DIF)

# Abordagem 1: Logistic Regression DIF Detection

install.packages("lordif")
library("lordif")
library("MPsychoR")
data("YouthDep")
cdi <- YouthDep[,1:26] ## extract CDI items
cdiDIF <- lordif(cdi, YouthDep$race, criterion = "Chisqr")

cdiDIF$stats[1:3, 1:5]

plot(cdiDIF, labels = c("White", "Black", "Asian", "Latino"))

head(cdiDIF$ipar.sparse, 10)

ppar <- cdiDIF$calib.sparse$theta


# Abordagem 2: Tree-Based DIF Detection

library("psychotree")
library("psychotools")
data("MathExam14W")
itmath <- as.list.data.frame(MathExam14W$solved)
covars <- MathExam14W[,3:9]
mex <- data.frame(solved = itemresp(itmath), covars)
mex <- subset(mex, nsolved > 0 & nsolved < 13)
mex$tests <- ordered(mex$tests)
mex$nsolved <- ordered(mex$nsolved)
mex$attempt <- ordered(mex$attempt)

set.seed(1)
mrt <- raschtree(solved ~ group + tests + nsolved + gender +
                   attempt + study + semester, data = mex, vcov = "info",
                 minsize = 50, ordinal = "l2", nrep = 1e5)

plot(mrt)


round(itempar(mrt)[,1:4], 2)










