# 024 - Testes Não-Paramétricos

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/024 - Testes Não-Paramétricos/"
setwd(path)


# Psicometria Online Academy

#######################################
#######################################
# 
#           MANN-WHITNEY
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("rstatix")
# install.packages("coin")
library(haven)
library(psych)
library(rstatix)
library(coin)

# file.choose()
dados <- read_spss(file = "Mann-Whitney e Kruskall-Wallis.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
dados$postos_Atencao_Concentrada <- rank(dados$Atenção_Concentrada)
describeBy(dados, group = dados$Exercício, omit= TRUE)

# Teste de Mann-Whitney

# Embora a função tenha o nome de Wilcoxon, no caso de grupos independentes ela relata a estatística 
# teste de Mann-Whitney, que é uma transformação do teste da soma dos postos de Wilcoxon.

# Normalmente, se relata a estatística teste com base no grupo com a menor soma 
# dos postos (no nosso exemplo, seria o grupo Exercício=="Não"). Mas o R sempre realata a 
# estatística com base no primeiro grupo (no exemplo, é o grupo Exercício=="Sim"): 

# Esta diferença não afeta o valor da significância.

resultado <- wilcox_test(Atenção_Concentrada ~ Exercício, data = dados, paired = FALSE)
resultado

# É possível obter o valor z do resultado com a função qnorm
qnorm(p = resultado$p/2)

# O tamanho de efeito r pode ser obtido diretamente com a função wilcox_effsize()
efeito <- wilcox_effsize(Atenção_Concentrada ~ Exercício, data = dados, paired = FALSE)
efeito


ggplot(dados, aes(x= Exercício, y= Atenção_Concentrada)) + geom_boxplot()



#######################################
#######################################
# 
#           KRUSKAL-WALLIS
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("rstatix")
library(haven)
library(psych)
library(rstatix)

# file.choose()
dados <- read_spss(file = "Mann-Whitney e Kruskall-Wallis.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
dados$postos_Atencao_Concentrada <- rank(dados$Atenção_Concentrada)
describeBy(dados, group = dados$Uso_maconha, omit= TRUE)

# Teste de Kruskal-Wallis

# A função kruskal.test() recebe um argumento do tipo fórmula:
#  *medida* ~ *grupos*.

resultado <- kruskal_test(Atenção_Concentrada ~ Uso_maconha, data = dados)
resultado

# Testes post hoc
post_hoc <- wilcox_test(Atenção_Concentrada ~ Uso_maconha, data = dados, p.adjust.method = "bonferroni")
post_hoc

# tamanho de efeito (r de Pearson)
efeito <- wilcox_effsize(Atenção_Concentrada ~ Uso_maconha, data = dados)
efeito




#######################################
#######################################
# 
#       WILCOXON SIGNED-RANK
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("rstatix")
# install.packages("tidyr")
library(haven)
library(psych)
library(rstatix)
library(tidyr)

# file.choose()
dados <- read_spss(file = "Wilcoxon e ANOVA de Friedman.sav")

head(dados)

## Estatísticas descritivas
describe(dados)

# Antes de realizar o teste dos postos assinalados de Wilcoxon é necessário alterar os dados para o formato longo
# No formato longo, todas as medidas repetidas (variáveis dependentes) ficam na mesma coluna e 
# criaremos uma segunda coluna para indicar a variável independente, ou seja,
# o momento da observação (pré, pós ou follow up).
# Desta forma, dados do mesmo participante aparecerão em três linhas diferentes, logo
# precisaremos de mais uma coluna para identificação dos participantes:
dados$ID <- 1:nrow(dados)

# Utilizaremo a função pivot_longer(), do pacote tidyr para alterar o formato dos dados

dados_lg <- pivot_longer(data = dados, 
                         names_to = "momento", # Nova coluna com a var. independente
                         names_prefix = "Depressão_", # Retira o prefixo indicado pelo argumento
                         values_to = "depressão", # Nova coluna com a var. dependente
                         cols = c(Depressão_pré , 
                                  Depressão_pós)) # variáveis que serão convertidas para o formato longo

head(dados_lg)

# Teste dos postos assinalados de Wilcoxon

# A função wilcox.test() será utilizada, mas cada variável deve ser passada como
# um argumento separado e deve-se indicar que o teste é pareado com paired = TRUE

resultado <- wilcox_test(depressão ~ momento, data= dados_lg, paired = TRUE)
resultado

as.factor(dados_lg$momento)

# Calculando o tamanho de efeito
efeito <- wilcox_effsize(depressão ~ momento,
                         data = dados_lg,
                         paired = TRUE, 
                         ci = TRUE,
                         ci.type = "bca",
                         nboot = 1000)
efeito



#######################################
#######################################
# 
#         ANOVA DE FRIEDMAN
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("tidyr")
# install.packages("rstatix")
library(haven)
library(psych)
library(tidyr)
library(rstatix)

# file.choose()
dados <- read_spss(file = "Wilcoxon e ANOVA de Friedman.sav")
head(dados)

## Estatísticas descritivas
describe(dados)

# Antes de realizar a ANOVA de Friedman é necessário alterar os dados para o formato longo
# No formato longo, todas as medidas repetidas (variáveis dependentes) ficam na mesma coluna e 
# criaremos uma segunda coluna para indicar a variável independente, ou seja,
# o momento da observação (pré, pós ou follow up).
# Desta forma, dados do mesmo participante aparecerão em três linhas diferentes, logo
# precisaremos de mais uma coluna para identificação dos participantes:
dados$ID <- 1:nrow(dados)

# Utilizaremo a função pivot_longer(), do pacote tidyr para alterar o formato dos dados

dados_lg <- pivot_longer(data = dados, 
                         names_to = "momento", # Nova coluna com a var. independente
                         values_to = "depressão", # Nova coluna com a var. dependente
                         cols = -ID) # variáveis que serão convertidas para o formato longo

head(dados_lg)
# ANOVA de Friedman

# A função friedman.test() pode ser escrita com uma fórmula da seguinte maneira:
# *var. dependente* ~ *var. independente* | *participantes*

# As mesmas informações podem ser passadas com os argumentos
#  y= VD, groups= VI, blocks=participantes.
dados_lg$momento <- as.factor(dados_lg$momento)

resultado <- friedman_test(depressão ~ momento | ID, data= dados_lg)
resultado

post_hoc <- wilcox_test(depressão ~ momento, data= dados_lg, paired= TRUE, p.adjust.method = "bonferroni")
post_hoc

# Calculando o tamanho de efeito
efeito <- wilcox_effsize(depressão ~ momento, data= dados_lg, paired= TRUE,
                         ci= TRUE, ci.type= "bca", nboot= 1000)
efeito