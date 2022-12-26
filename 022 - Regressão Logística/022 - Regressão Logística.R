# 022 - Regressão Logística

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/022 - Regressão Logística/"
setwd(path)


# Psicometria Online Academy

#######################################
#######################################
# 
#      REGRESSÃO LOGÍSTICA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("lm.beta")
# install.packages("car")
# install.packaages("rstatix")
library(haven)
library(psych)
library(lm.beta)
library(car)
library(rstatix)


# file.choose()
dados <- read_spss(file = "Dados RegLog.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describe(dados, omit = TRUE)
descritivos


# Regressão Logística

# Para fazer a regressão logística, vamos usar a função *glm()*. 
# Nela, passaremos os dados que estamos usando e uma fórmula. 
# Esta fórmula tem o seguinte formato: 
# variável dependente ~ variável_independente_1 + variável_independente_1,

#  É o argumento family = "binomial" que define que a regressão será logística
fit <- glm(IDEA_SUIC ~ Sexo + EXT + SOC + CONSC + NEURO + ABEXP, 
           data = dados,
           family = "binomial")
summary(fit)

# Nós podemos alterar o método de entrada dos preditores

step_fit <- step(object = fit,
                 direction = "backward")
summary(step_fit, correlation = TRUE)

anova(step_fit, fit) 
pchisq(2.2192, 3) # deviance, df comparando modelos

# Também é possível adicionar e remover preditorees manualmente
fit_4pred <- update(fit, . ~ . -ABEXP) # . indica que deve-se manter valores
fit_3pred <- update(fit_4pred, . ~ . -CONSC) # - indica que valor será removido do modelo

anova(fit_3pred, fit_4pred); pchisq(2.62, 1)


# Vamos verificar a significância do ajuste do modelo completo

# O ajuste do modelo é indicado pela diferença do modelo base (Null deviance) e
# e o modelo com os preditores (Residual deviance). Ambas são medidas de -2LogLikelihood
# e seguem a distribuição do qui-quadrado

# Para saber se o modelo é estatisticamente significativo precisamos testar o 
# qui-quadrado da diferença entre os dois modelos manualmente.
qui_q_fit <- list("Qui-quadrado_modelo" = fit$null.deviance - fit$deviance,
                  "gl_modelo" = fit$df.null - fit$df.residual)

qui_q_fit$prob <- 1 - pchisq(qui_q_fit$`Qui-quadrado_modelo`, qui_q_fit$gl_modelo)

qui_q_fit # Teste de significância do ajuste do modelo

# Calculando o pseudo R²

# Vamos utilizar a função disponibilizada por 
# Field, Milies e Field (2012) Discovering statistics using R

# Rode o código abaixo e uma nova função aparecerá no enviroment

logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <- 1 - dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2 ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2 ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2 ", round(R.n, 3), "\n")
}

logisticPseudoR2s(fit)


# Na regressão logística, os preditores podem ser convertidos em odds-ratios com
# a função exp()
odds_ratio <- exp(fit$coefficients)
round(odds_ratio, 3)


## Verificando pressupostos

diagnostico <- data.frame(
  prob_prevista = fitted(fit), # probabilidade prevista
  z_resid = rstandard(fit), # resíduos padronizados
  df_fit = dffits(fit), # diferença de ajuste, se o caso for deletado
  cook = cooks.distance(fit), # Disância de cook
  leverage = hatvalues(fit) # Alavancagem
)

head(diagnostico)

# Realizando o bootstrapping com o pacote car
fit_boot <- Boot(object = fit, R = 5000, method = "case")

ci_odds_ratio <- exp( Confint(fit_boot,
                              level=.95,
                              type = "bca"))
ci_odds_ratio <- round(ci_odds_ratio, 3)
ci_odds_ratio
