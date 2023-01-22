# Mair (2018), Chapter 03

rm(list = ls());ls()

setwd("C:/Users/User/Desktop/")

# 1. Regressão multivariada por meio de MANOVA
# dois itens de agradabilidade e de abertura à experiência como preditores
# preconceito étnico e preconceito contra pessoas com deficiência como critérios
library("MPsychoR")
data("Bergh")
fitmvreg <- lm(cbind(EP, DP) ~ A1 + A2 + O1 + O2, data = Bergh)

# chamando a função Manova, do pacote car, para implementar as estatísticas multivariadas
library("car")
Manova(fitmvreg)

# por default, a função Manova retorna o traço de Pillai
# summary permite ver outras estatísticas multivariadas
summary(Manova(fitmvreg))

# 2. Regressão multivariada por meio de um framework de path analysis
# ~ significa is regressed on
library("lavaan")
mvreg.model <- '
EP ~ b11*A1 + b12*A2 + b13*O1 + b14*O2
DP ~ b21*A1 + b22*A2 + b23*O1 + b24*O2'
fitmvreg2 <- sem(mvreg.model, data = Bergh)

# Representação do diagrama de caminhos
library("semPlot")
semPaths(fitmvreg2, what = "est", edge.label.cex = 1,
         layout = "tree", residuals = FALSE, edge.color = 1,
         esize = 1, rotation = 3, sizeMan = 8, asize = 2.5,
         fade = FALSE, optimizeLatRes = TRUE)

# parâmetros de interesse da análise de caminhos
parameterEstimates(fitmvreg2)[c(1:8, 11),]


# Modelos de moderação
# efeito da intensificação do trabalho (X) sobre a apreciação cognitiva (Y),
# avaliando se o clima participativo (Z) modera esta relação

library("MPsychoR")
data("Paskvan")

wintense.c <- scale(Paskvan$wintense, scale = FALSE) ## center
fit.YX <- lm(cogapp ~ wintense.c, data = Paskvan) ## Y on X
round(summary(fit.YX)$coefficients, 4)

pclimate.c <- scale(Paskvan$pclimate, scale = FALSE) ## center
fit.YZ <- lm(cogapp ~ pclimate.c, data = Paskvan) ## Y on Z
round(summary(fit.YZ)$coefficients, 4)

# Moderação pode ser usada por meio da função lm, inserindo termo de interação
# ou usando a função moderate.lm, por meio do pacote QuantPsyc (Fletcher, 2012)
# por default, esta última opção já centra X e Z
install.packages("QuantPsyc")
library("QuantPsyc")
fit.mod <- moderate.lm(x = wintense, z = pclimate, y = cogapp,
                       data = Paskvan)
round(summary(fit.mod)$coefficients, 4)

# com X e Z centralizados, a1 é a mudança na apreciação cognitiva (Y) com base na mudança
# em uma unidade em intensificação do trabalho (X) para indivíduos com o clima de participação
# (Z) médio, em oposição a Z = 0 na versão não centralizada dos preditores

# análise de slopes simples para melhor explorar a interação significativa
# -1SD, M, +1SD
fit.ss <- sim.slopes(fit.mod, Paskvan$pclimate)
round(fit.ss, 4)

# simple slope plot usa a função graph.mod
graph.mod(ssmod = fit.ss, x = wintense, y = cogapp, 
          data = Paskvan)


# Modelos de mediação
# relação entre intensificação do trabalho (X) e exaustão emocional (Y),
# com a apreciação cognitiva (M) agindo como mediadora

# 1. Mediação usando o pacote mediation (Tingley et al., 2014)
install.packages("mediation")
library("mediation")
fit.MX <- lm(cogapp ~ wintense, data = Paskvan)
fit.YXM <- lm(emotion ~ wintense + cogapp, data = Paskvan)

# objetos da função lm são usados para alimentar a função mediate
# treat = X; mediator = M
# ACME (average causal mediation effect) = ab
# average direct effect = c'
# total effect = ab + c' = c
# proportion of the mediated effect = ab / c
set.seed(123)
fitmed <- mediation::mediate(fit.MX, fit.YXM,
                             treat = "wintense", mediator = "cogapp",
                             sims = 999, boot = TRUE, boot.ci.type = "bca")
summary(fitmed)


# 2. Mediação usando o pacote lavaan
# a seguir, o c' foi substituído por c
library("lavaan")
med.model <- '
emotion ~ c*wintense + b*cogapp
cogapp ~ a*wintense
ind := a*b
tot := ind+c
prop := ind/tot'
set.seed(123)
fitmedsem <- lavaan::sem(med.model, Paskvan, se = "bootstrap",
                         bootstrap = 999)
parameterEstimates(fitmedsem, zstat = FALSE, pvalue = FALSE,
                   boot.ci.type = "bca.simple")[c(7,1,8,9),]

# gráfico de caminhos
semPaths(fitmedsem)


# Modelos de moderação-mediação combinados

# condition process analysis, where the goal is to model the
# conditional mechanisms by which a variable transmits its effects on another

# mediação moderada: o efeito indireto da intensificação do trabalho (X) sobre a
# exaustão emocional (Y) através da apreciação cognitiva (M) é contingente ao
# clima participativo (Z) [moderação no estágio 1 da mediação].
# Assim, Z modera o efeito entre X e M, representando um efeito indireto condicional,
# onde a magnitude do efeito de mediação depende dos níveis do moderador

# implementando o modelo acima por meio do lavaan
quantile(Paskvan$pclimate)
## 0% 25% 50% 75% 100%
## 1.0 2.0 3.0 3.5 5.0
medmod.model <- '
## set of regressions
cogapp ~ a1*wintense + a2*pclimate + a3*wintense:pclimate
emotion ~ c*wintense + b*cogapp
## conditional indirect effects
cie.q1 := (a1 + a3*2)*b ## first quartile
cie.q2 := (a1 + a3*3)*b ## median
cie.q3 := (a1 + a3*3.5)*b ## third quartile
'
set.seed(123)
fitmedmod <- lavaan::sem(medmod.model, data = Paskvan,
                         se = "bootstrap", bootstrap = 999)


# diagrama de caminho
semPaths(fitmedmod, layout = "spring", asize = 2.5,
         sizeMan = 10, residuals = FALSE, nCharNodes = 7,
         edge.label.cex = 1)

# estimativas dos parâmetros
parameterEstimates(fitmedmod, zstat = FALSE, pvalue = FALSE,
                   boot.ci.type = "bca.simple")[c(3, 4, 14:16),]


# SEM
# itens de agradabilidade e de abertura a experiências, três indicadores cada
# preconceito generalizado, composto de itens relacionados a preconceito étnico,
# sexismo, preconceito contra gays e lésbicas e preconceito contra pessoas com deficiência
# modelo estrutural e de mensuração é ajustado usando SEM
'
formula type	                  operator    	mnemonic
latent variable definition	    =~          	is measured by
regression	                    ~           	is regressed on
(residual) (co)variance	        ~~        	  is correlated with
intercept	                      ~ 1         	intercept
'

library("MPsychoR")
library("lavaan")
data("Bergh")
Bergh.model <- 'GP =~ EP + HP + DP + SP
Agree =~ A1 + A2 + A3
Open =~ O1 + O2 + O3
GP ~ Agree + Open'
fitGP <- sem(Bergh.model, data = Bergh, estimator = "MLR")


# diagrama de caminhos
semPaths(fitGP, what = "std", edge.label.cex = 0.7, esize = 1,
         intercepts = FALSE, rotation = 4, edge.color = 1, asize = 2.5,
         sizeMan = 5, mar = c(1, 1.5, 1.5, 3), fade = FALSE)


# resumo do modelo
# Covariances Std.lv = correlação
summary(fitGP, standardized = TRUE, fit.measures = TRUE)


# SEM multigrupo
# forçando os interceptos a serem iguais entre homens em mulheres
fit.free <- sem(Bergh.model, group = "gender",
                group.equal = c("intercepts"),
                group.partial = c("DP~1", "HP~1", "SP~1"),
                data = Bergh, estimator = "MLR")

# forçando as cargas a serem iguais entre grupos, exceto as de SP em GP
fit.load <- sem(Bergh.model, group = "gender",
                group.equal = c("loadings", "intercepts"),
                group.partial = c("GP=~SP", "DP~1", "HP~1", "SP~1"),
                data = Bergh, estimator = "MLR")

# forçando interceptos a serem iguais e coeficientes de caminhos iguais entre grupos
fit.prestrict <- sem(Bergh.model, group = "gender",
                     group.equal = c("intercepts", "regressions"),
                     group.partial = c("DP~1", "HP~1", "SP~1"),
                     data = Bergh, estimator = "MLR")

# comparando modelos 1 e 3, uma vez que o 3 está aninhado no modelo 1
anova(fit.free, fit.prestrict)

# comparando modelos 1 e 2. como eles não estão aninhados um no outro, o modo de 
# comparação é diferente. aqui, a comparção é por meio de AIC/BIC, usando o pacote nonnest2
# para a comparação ser bem sucedida, o estimador precisa ser o maximum likelihood
install.packages("nonnest2")
library("nonnest2")
fit.load1 <- update(fit.load, estimator = "ML")
fit.prestrict1 <- update(fit.prestrict, estimator = "ML")
compIC <- icci(fit.load1, fit.prestrict1)
compIC

vuongtest(fit.load1, fit.prestrict1)


# Comentários
# SEM = covariance based
# PLS models = variance based


# Modelos de curvas crescimento latente (LGM)
# consumo de cigarros
# no LGM, nós tipicamente fixamos as cargas do intercepto latente em 1
# a primeira carga da forma latente é fixada em 0, refletindo nosso nível inicial
# no tempo 1. Assim, o fator do intercepto será baseado na mensuração no tempo 1
library("lavaan")

install.packages("aspect")
library("aspect")
data("duncan")
model_shape <- '
inter =~ 1 * CIG_T1 + 1 * CIG_T2 + 1 * CIG_T3 + 1 * CIG_T4
shape =~ 0 * CIG_T1 + 1 * CIG_T2 + CIG_T3 + CIG_T4'
fitCig1 <-growth(model_shape, data = duncan, estimator = "WLS")

summary(fitCig1, header = FALSE)


semPaths(fitCig1, what = "std",edge.label.cex = 0.7, esize = 1,
         edge.color = 1, sizeMan = 6, asize = 2.5, intercepts = FALSE,
         rotation = 4, mar = c(3, 5, 3.5, 5), fade = FALSE)

# Forçando os dados a se ajustarem a um modelo de tendência de crescimento linear
model_lin <- '
inter =~ 1*CIG_T1 + 1*CIG_T2 + 1*CIG_T3 + 1*CIG_T4
linear =~ 0*CIG_T1 + 1*CIG_T2 + 2*CIG_T3 + 3*CIG_T4'
fitCig2 <- growth(model_lin, data = duncan, estimator = "WLS")
parameterEstimates(fitCig2)[21, ]

round(fitMeasures(fitCig2)[c("rmsea", "cfi", "srmr")], 3)

# Forçando os dados a se ajustarem a um modelo de tendência de crescimento quadrático
model_quad <- '
inter =~ 1*CIG_T1 + 1*CIG_T2 + 1*CIG_T3 + 1*CIG_T4
linear =~ 0*CIG_T1 + 1*CIG_T2 + 2*CIG_T3 + 3*CIG_T4
quad =~ 0*CIG_T1 + 1*CIG_T2 + 4*CIG_T3 + 9*CIG_T4'
fitCig3 <- growth(model_quad, data = duncan, estimator = "WLS")

parameterEstimates(fitCig3)[28:29,]

round(fitMeasures(fitCig3)[c("rmsea", "cfi", "srmr")], 3)

# Modelo com tendência linear é significativamente pior que o primeiro modelo
# onde os parâmetros T3 e T4 foram livremente estimados
anova(fitCig1, fitCig2)