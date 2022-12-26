# 006 - Teste t

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/006 - Teste t/"
setwd(path)

# Teste t independente
fundao <- rnorm(25, 105, 15)
frente <- rnorm(25, 99.5, 15)

# Student
t.test(fundao, frente, alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = TRUE, conf.level = 0.95)

# Welch
t.test(fundao, frente, alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# tamanho de efeito 
qis <- c(fundao, frente)
grupos <- c(rep(1, 25), rep(2, 25))

library(psych)
cohen.d(qis, grupos, alpha = .05, std = TRUE,
        sort = NULL, dictionary = NULL, MD = FALSE,
        data = NULL)

# Teste t pareado
com_rotulo <- rnorm(100, 3.21, 1.49)
sem_rotulo <- rnorm(100, 1.90, 1.07)
t.test(com_rotulo, sem_rotulo, alternative = c("two.sided"), mu = 0, 
       paired = TRUE, var.equal = FALSE, conf.level = 0.95)

vd <- c(com_rotulo, sem_rotulo)
vi <- c(rep(1, 100), rep(2, 100))

cohen.d(vd, vi, alpha = .05, std = TRUE,
        sort = NULL, dictionary = NULL, MD = FALSE,
        data = NULL)


# Psicometria Online Academy

#######################################
#######################################
# 
#     TESTE T AMOSTRA ÚNICA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
library(haven)
library(psych)

amostra_unica <- read_spss(file = "Amostra unica.sav")

head(amostra_unica)

## Estatísticas descritivas
describe(amostra_unica$Satis_Clientes)

# Teste t

# A função t.test() realiza todos os tipos de teste t.
# Para definir um teste de amostr única, é necessário apenas fornecer os dados e
# o argumento mu, que é a média com a qual os dados serão comparados.

# Satisfação de clientes comparado com a média "9". 

t.test(amostra_unica$Satis_Clientes, mu = 9)



#######################################
#######################################
# 
#     TESTE T AMOSTRAS INDEPENDENTES
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("MKinfer")
library(haven)
library(psych)
library(MKinfer)

# file.choose()
dados <- read_spss(file = "Medidas Independentes e Dependentes.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
describe(dados, omit = TRUE) # omit = TRUE elimina os factors da tabela

describeBy(dados, dados$Filhos, omit = TRUE)

# Teste t independente

# A função t.test() realiza todos os tipos de teste t.
# Para definir um teste independente utilizamos ao argumento formula, da seguinte forma:

#  formula = Variável_dependente ~ variável_independente

teste_t <- t.test(formula = SaúdeMental ~ Filhos, data = dados)
teste_t

efeito <- cohen.d(x = dados$SaúdeMental, group = dados$Filhos )
efeito

## Comparação entre saúde mental e ter filhos, com boostrapping
boot.t.test(formula = SaúdeMental ~ Filhos, data = dados)



#######################################
#######################################
# 
#     TESTE T DE MEDIDAS REPETIDAS
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("effectsize")
# install.packages("MKinfer") # Para fazer boostrapping
library(haven)
library(psych)
library(effectsize)
library(MKinfer)

# file.choose()
dados <- read_spss(file = "Medidas Independentes e Dependentes.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
describe(dados, omit = TRUE)

# Teste t de medidas repetidas

# A função t.test() realiza todos os tipos de teste t.
# Para definir um teste dependente utilizamos os argumentos x e y para selecionar 
# as variáveis e o argumento paired= TRUE para informar que o teste é dependente.

teste_t_mr <- t.test(x = dados$Felicidade_T1, y= dados$Felicidade_T2, paired= TRUE)
teste_t_mr

# effectsize::cohens_d
efeito <- cohens_d(x = dados$Felicidade_T1, y = dados$Felicidade_T2, paired = TRUE)
efeito

boot.t.test(x = dados$Felicidade_T1, y = dados$Felicidade_T2, paired = TRUE, R = 5000)


