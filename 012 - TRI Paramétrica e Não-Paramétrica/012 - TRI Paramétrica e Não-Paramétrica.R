# 012 - TRI Parametrica e Nao-Parametrica

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/012 - TRI Paramétrica e Não-Paramétrica/"
setwd(path)


# Carregando pacotes
install.packages("latticeExtra")
install.packages("mokken")
library(latticeExtra)
library(mokken)
library(mirt)

### Lendo os dados
Banco <- read.csv("Data Repository_Disability.csv")
Banco <- Banco[9:13]

### Definindo o modelo
model1 <- mirt(Banco, 1, itemtype = "graded")

### Coeficientes do modelo
coef(model1, simplify = TRUE, IRTpars = TRUE)

### CCIs de todos os itens
plot(model1, type = "trace")

### CCI do primeiro item
itemplot(model1, 1, type = "trace", theta_lim = c(-3, 3))

### Curva de informacao total do instrumento
plot(model1, type = "infoSE")

### TRI nao parametrica - Analise de Escala de Mokken - AEM
aisp(Banco)

### Coeficientes de escalabilidade H para itens eo tipo de escala geral
coefH(Banco)

### Tipo de monotonicidade
summary(check.monotonicity(Banco))

### Funcoes de resposta para todos os itens
plot(check.monotonicity(Banco))

### Analise de nao interseccao das funcoes de resposta dos itens
restscore.list <- check.restscore(Banco)
plot(restscore.list)