# 005 - Correlação

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/005 - Correlação/"
setwd(path)

library(MASS)

mu <- c(0, 0)
sigma <- matrix(c(1, 0.5,
                  0.5, 1),
                2, 2)

data <- data.frame(mvrnorm(n = 100, mu, sigma))

# Pearson
cor.test(x = data$X1, y = data$X2, method = "pearson")

# Spearman
cor.test(x = data$X1, y = data$X2, method = "spearman")

# Kendall
cor.test(x = data$X1, y = data$X2, method = "kendall")

# correlacao parcial
mu <- c(0, 0, 0)
sigma <- matrix(c(1, 0.5, 0.4,
                  0.5, 1, 0.3, 
                  0.4, 0.3, 1),
                3, 3)

data <- data.frame(mvrnorm(n = 100, mu, sigma))


library(psych)
partial <- partial.r(data)
corr.p(partial, n = 10)

# Psicometria Online Academy

#######################
#######################
# 
#     CORRELAÇÃO
# 
#######################
#######################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
library(haven)
library(psych)

dados <- read_spss(file = "Correlacao.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas

# Vamos criar um novo objeto somente com as variáveis que serão utilizadas
vars <- dados[7:10]

descritivos <- describe(vars)
descritivos

## Correlação

cor <- corr.test(x = vars, # Dados
                 use = "complete", # Como vamos lidar com *missings*
                 method = "pearson") # Tipo de corelação

cor

# Se quisermos separar a matriz de correlação:

cor$r

# Somente a matriz de valores de significância:
cor$p


## R-to-Z

fisherz(rho = cor$r)

# Ponto-biserial

cor.test(x = dados$Felicidade, y = as.numeric(dados$Filhos))

