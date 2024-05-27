# 009 - Curva ROC

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/009 - Curva ROC/"
setwd(path)

'''
eh a capacidade de uma ferramenta avaliativa para discriminar pessoas
positivas para determinada condicao de pessoas negativas para essa
mesma condicao

A AUC da curva ROC indica quao bem o instrumento (ou o participante)
discrimina pessoas com psicopatologia de pessoas sem essa mesma
condicao (ou discrimina itens vistos de 
nao vistos em uma tarefa de memoria de reconhecimento)

'''

tutorial <- read.table("Gronlund et al. (2014), eyewitness identification and ROC analysis, tutorial 1.txt", header = TRUE)

install.packages("pROC")
library(pROC)

auc(controls = tutorial$FalseID,
    cases = tutorial$CorrectID[!is.na(tutorial$CorrectID)],
    direction = "<", ci = TRUE, partial.auc = c(1, 0.83))
