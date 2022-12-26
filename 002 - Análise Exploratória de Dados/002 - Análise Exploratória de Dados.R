# 002 - Análise Exploratória de Dados

# Baseado no Capitulo 2 de Faiad, Baptista e Primi (2021)

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/002 - Análise Exploratória de Dados/"
setwd(path)

# abrindo os dados
dados <- read.csv("Kliegl et al. (2019).csv")

# descrevendo os dados
# install.packages("psych")
library(psych)
describe(dados)

# exportando os dados descritivos para o Excel
install.packages("openxlsx")
library(openxlsx)
DSC <- describe(dados)
write.xlsx(DSC, "estatisticas_descritivas.xlsx", rowNames = FALSE)

# dados perdidos

# NMAR - not missing at random (perda sistematica)
# MCAR - missing completely at random
# MAR - missing at random

mean(is.na(dados))

easy_1 <- dados[2] 
mean(is.na(easy_1))

# representacao grafica dos dados ausentes
install.packages("naniar")
library(naniar)

vis_miss(dados)

# opcoes de imputacao
install.packages("TestDataImputation")
library(TestDataImputation)

a <- c(10, NA, 3, 5, 8, 14, NA, 2, NA, 3, NA)
new_a <- ImputeTestData(a, Mvalue = "NA", max.score = 14, method = "LW")
new_a

# Alternativas: IM (item mean), PM (participant mean)

# outliers univariados
boxplot(dados$difficult_1,
        xlab = expression(bold("Difficult Items")),
        ylab = expression(bold("No. of Items Recalled")),
        cex.lab = 1.5,  # axes labels
        cex.axis = 1.0  # axes ticks
        )

subdados <- dados[2:11]
boxplot(subdados)

summary(dados$difficult_1)          # valores usados na construcao do boxplot
boxplot.stats(subdados$T1_easy)$out # detectando os valores dos outliers

# escores z
T1_easy_z <- scale(dados$T1_easy)
T1_easy_z > abs(2.5)

# outliers multivariados
install.packages("MVN")
library(MVN)
mvn(dados[,2:11], showOutliers = TRUE) # normalidade multivariada e outliers multivariados

# normalidade

# normal padrao: M = 0, DP = 1, Skew = 0, Kur = 0,263
shapiro.test(dados$T1_easy)
hist(dados$T1_easy)

# linearidade

# homoscedasticidade
memoria <- c(2, 13, 17, 13, 8, 5, 11, 2, 10, 14, 9, 12, 11, 18, 14)
grupo <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
levene.test()

# colinearidade e multicolinearidade
bem <- c(16, 20, 21, 22, 23, 22, 27, 25, 27, 28)
satcoj <- c(19, 22, 24, 24, 25, 25, 26, 26, 28, 32)
cor(bem, satcoj)

# Psicometria Online Academy

#######################################
#######################################
# 
#            CURVA NORMAL
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("nortest")
library(haven)
library(psych)
library(nortest)

# file.choose()
dados <- read_spss(file = "Distribuicao normal.sav")

head(dados)

## Estatísticas descritivas

# Existem várias formas de se calcular a assimetria e curtose. Por padrão a função
# describe() usa um método diferente do SPSS. Para usar o mesmo procedimento que o 
# SPSS, mude o argumento type para 2.

describe(dados$SaúdeMental, type = 2)


# Função para calcular escore padrão de assimetria e curtose

# Rode as linhas abaixos para criar uma nova função.
#  Esta função é baseada na fórmula que o SPSS utiliza, conforme postado por
# Howard Seltamn em https://www.stat.cmu.edu/~hseltman/files/spssSkewKurtosis.R
spssSkewKurtosis <- function(x) {
  w = length(x)
  m1 = mean(x)
  m2 = sum((x - m1) ^ 2)
  m3 = sum((x - m1) ^ 3)
  m4 = sum((x - m1) ^ 4)
  s1 = sd(x)
  skew = w * m3 / (w - 1) / (w - 2) / s1 ^ 3
  sdskew = sqrt(6 * w * (w - 1) / ((w - 2) * (w + 1) * (w + 3)))
  kurtosis = (w * (w + 1) * m4 - 3 * m2 ^ 2 * (w - 1)) / ((w - 1) * (w - 2) * (w - 3) * s1 ^ 4)
  sdkurtosis = sqrt(4 * (w ^ 2 - 1) * sdskew ^ 2 / ((w - 3) * (w + 5)))
  z.skew = skew / sdskew
  z.kurtosis = kurtosis / sdkurtosis
  mat = matrix(c(skew, kurtosis, sdskew, sdkurtosis, z.skew, z.kurtosis), 2,
             dimnames = list(c("skew", "kurtosis"), c("estimate", "se", "z-value")))
  return(mat)
}

spssSkewKurtosis(dados$SaúdeMental)

# Testes de normalidade

lillie.test(dados$SaúdeMental) # Este é o teste K-S que o SPSS utiliza

shapiro.test(dados$SaúdeMental)



# Visualizando os dados

# Gerando o Boxplot
boxplot(dados$SaúdeMental,
        main = "Boxplot",
        xlab = "",
        ylab = expression(bold("Saúde Mental")))

# Gerando o histograma
hist(dados$SaúdeMental)

hist(dados$SaúdeMental, breaks = 21)

hist(dados$SaúdeMental,
     breaks = 21,
     xlab = expression(bold("Saúde Mental")),
     ylab = expression(bold("Frequência")),
     main = "Meu Histograma")