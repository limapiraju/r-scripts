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



# Psicometria Online Academy

#######################################
#######################################
# 
#         CURVA ROC
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("pROC")
# install.packages("ggplot2")
library(haven)
library(psych)
library(pROC)
library(ggplot2)

# file.choose()
dados <- read_spss(file = "ROC.sav")
dados <- as_factor(dados)
head(dados)

## Estatísticas descritivas
describeBy(dados$ESCORE, dados$Grupo)

# ROC

# Para fazer a curva ROC, vamos usar a função *roc()* e criar um novo objeto com todas as informações necessárias. 
roc_tag <- roc(Grupo ~ ESCORE, data = dados)
roc_tag

# A partir do objeto roc_tag vamos pedir as coordenadas da curva e 
# os intervalos de confiança da área sob a curva.
coordenadas <- coords(roc_tag)


# Em seguida, vamos usar a função *plot.roc()*. 
par(pty= "s")
plot.roc(roc_tag, legacy.axes = TRUE)


#Também é possível utilizar a função *ggroc()* e o *ggplot2* para criar um gráfico de forma mais elaborada.

ggroc(roc_tag, legacy.axes = TRUE, size= 1.5, color= "blue") +
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="black", linetype="dashed") +
  coord_fixed() +
  theme_bw()


