# 023 - Qui-Quadrado

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/023 - Qui-Quadrado/"
setwd(path)


# Psicometria Online Academy

#######################################
#######################################
# 
#     QUI-QUADRADO DE ADERÊNCIA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("janitor")
library(haven)
library(psych)
library(janitor)

# file.choose()
dados <- read_spss(file = "Qui-Quadrado Aderencia.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
freq <- tabyl(dat = dados, "Preferencia")
freq <- adorn_pct_formatting(freq)
freq

# Qui-quadrado de Aderência

# Para realizar o qui-quadrado de aderência, vamos fazer uma tabela do número de 
# observações através da função *table()*. Em seguida, vamos usar a função 
# *chisq.test()* para fazer o cálculo, usando a tabela que criamos e a 
# probabilidade esperada em cada categoria.

table(dados)
chi <- stats::chisq.test(table(dados), p = c(1/3, 1/3, 1/3)) 
# O argumento p pode ser omitido se as probabilidades esperadas forem iguais


# tamanho de efeito
psych::chi2r(chi2 = as.numeric(chi$statistic), n = nrow(dados))


#######################################
#######################################
# 
#   QUI-QUADRADO DE INDEPENDÊNCIA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("janitor")
# install.packages("effectsize")
library(haven)
library(psych)
library(janitor)
library(effectsize)

# file.choose()
dados <- read_spss(file = "Qui-Quadrado 2X2.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
#  O pacote janitor oferece boas oppções para formatar tabelas de frequência:
freq <- tabyl(dat = dados, var1 = Ansiedade, var2 = Risco_Acentuado)
freq_total <- adorn_totals(freq, where = c('row', 'col'))

#  Também é possível pedir uma tabela de porcentagens:
adorn_percentages(freq_total, denominator = "all")


# Qui-quadrado de Independência

# Para realizar o qui-quadrado de independência, é necessário criar uma tabela cruzada
# com as funções *table()* ou *tabyl()* (sem os valores totais).
# A função *chisq.test()* pode ser passada sem nenhum argumento adicional.


qui.q <- chisq.test(freq)
qui.q

# O objeto qui.q é uma lista que contém informações de interesse:
# Valores esperados
qui.q$expected

# Resíduos brutos
qui.q$residuals

# Resíduos padronizados
qui.q$stdres

# Podemos converter o valor do qui-quadrado para r com o pacote psych
efeito <- chi2r(chi2 = as.numeric(qui.q$statistic), n = nrow(dados))
efeito

# O odds-ratio pode ser calculado com o pacote effectsize
oddsratio(x = dados$Ansiedade, y = dados$Risco_Acentuado)


#######################################
#######################################
# 
#   QUI-QUADRADO DE INDEPENDÊNCIA 4x2
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("janitor")
# install.packages("effectsize")
library(haven)
library(psych)
library(janitor)
library(effectsize)

# file.choose()
dados <- read_spss(file = "Qui-Quadrado 4X2.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
#  O pacote janitor oferece boas oppções para formatar tabelas de frequência:
freq <- tabyl(dat = dados, var1 = Ansiedade, var2 = Estilos_parentais)
freq_total <- adorn_totals(freq, where = c('row', 'col'))

#  Também é possível pedir uma tabela de porcentagens:
adorn_percentages(freq_total, denominator = "all")


# Qui-quadrado de Independência

# Para realizar o qui-quadrado de independência, é necessário criar uma tabela cruzada
# com as funções *table()* ou *tabyl()* (sem os valores totais).
# A função *chisq.test()* pode ser passada sem nenhum argumento adicional.


qui.q <- chisq.test(freq)
qui.q

# O objeto qui.q é uma lista que contém informações de interesse:
# Valores esperados
qui.q$expected

# Resíduos brutos
qui.q$residuals

# Resíduos padronizados
qui.q$stdres

# V de Cramer
cramers_v(x = dados$Ansiedade, y = dados$Estilos_parentais)

# Podemos converter o valor do qui-quadrado para r com o pacote psych
efeito <- chi2r(chi2 = as.numeric(qui.q$statistic), n = nrow(dados))
efeito

# Comparação Autoritativo x Indulgente
aut_indul <- dados[dados$Estilos_parentais == "Autoritativo" | dados$Estilos_parentais=="Indulgente",]

freq_aut_indul <- tabyl(dat = aut_indul, var1 = Ansiedade, var2 = Estilos_parentais)

oddsratio(x = aut_indul$Ansiedade, y = aut_indul$Estilos_parentais)

