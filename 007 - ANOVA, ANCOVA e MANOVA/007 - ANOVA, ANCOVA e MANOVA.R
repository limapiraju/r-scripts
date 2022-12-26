# 007 - ANOVA, ANCOVA e MANOVA

# Baseado no capitulo 5 de Faiad, Baptista e Primi (2021)

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/007 - ANOVA, ANCOVA e MANOVA/"
setwd(path)

# instalando pacotes
install.packages("car")
install.packages("heplots")
library(psych)
library(car)
library(heplots)

# criando alguns dados
happiness <- c(3, 2, 1, 1, 4, 5, 2, 4, 2, 3, 7, 4, 5, 3, 6)
therapy <- c(rep(1, 5), rep(2, 5), rep(3, 5))
data <- data.frame(happiness, therapy)
data$therapy <- as.factor(data$therapy)
levels(data$therapy) <- c("Control", "15 min", "30 min")

# Pressuposto de normalidade
shapiro.test(data$happiness)
qqnorm(data$happiness)
qqline(data$happiness, col = "red")

# Pressuposto de homoscedasticidade
leveneTest(data$happiness, group = factor(data$therapy),
           center = "mean") # default = median; default SPSS = mean

# Descrevendo os dados
describeBy(data$happiness, data$therapy)

error.bars.by(data$happiness,
              data$therapy,
              eyes = FALSE, by.var = TRUE, ylim = c(0, 10))

# ANOVA unifatorial, F de Fisher
anova1 <- aov(data$happiness ~ data$therapy)
summary(anova1)

eta_manual <- 20.13 / (23.60 + 20.13)
eta_manual

omega_manual <- (20.13 - 2 * (1.97)) / (23.60 + 20.13 + 1.97)
omega_manual

# ANOVA unifatorial, F de Welch
oneway.test(data$happiness ~ data$therapy, data = data, var.equal = FALSE)

# Tukey, post hoc
TukeyHSD(anova1)


# ANCOVA
# criando alguns dados
happiness <- c(3, 2, 5, 2, 2, 2, 7, 2, 4, 7, 5, 3, 4, 4, 7, 5, 4, 
               9, 2, 6, 3, 4, 4, 4, 6, 4, 6, 2, 8, 5)
love <- c(4, 1, 5, 1, 2, 2, 7, 4, 5, 5, 3, 1, 2, 2, 6, 4, 2, 1, 3,
          5, 4, 3, 3, 2, 0, 1, 3, 0, 1, 0)
therapy <- c(rep(1, 9), rep(2, 8), rep(3, 13))
data <- data.frame(therapy, happiness, love)
data$therapy <- as.factor(data$therapy)
levels(data$therapy) <- c("Control", "15 min", "30 min")

# ancova
ancova <- aov(data$happiness ~ data$therapy + data$love)
summary(ancova)


# Manova
library(MASS)

# simulando alguns dados
mu1 <- c(105, 20)
mu2 <- c(100, 18)
mu3 <- c(98, 16)
sigma <- matrix(c(1, 0.5, 0.5, 1),2,2)

grupo1 <- data.frame(mvrnorm(n = 100, mu1, sigma))
grupo2 <- data.frame(mvrnorm(n = 100, mu2, sigma))
grupo3 <- data.frame(mvrnorm(n = 100, mu3, sigma))

colnames(grupo1) <- c("intelig?ncia", "mem?ria")
colnames(grupo2) <- c("intelig?ncia", "mem?ria")
colnames(grupo3) <- c("intelig?ncia", "mem?ria")

library(tidyverse)
dim(grupo1)
data2 <- bind_rows(grupo1, grupo2, grupo3)
dim(data2)

grupo <- c(rep("Grupo 1", 100), rep("Grupo 2", 100), rep("Grupo 3", 100))
data2$grupo <- as.factor(grupo)

# manova
Y <- cbind(data2$intelig?ncia, data2$mem?ria)
mardia(Y)       # normalidade multivariada
boxM(Y, data2$grupo)  # homogeneidade das matrizes de variancia-covariancia


mnv <- manova(Y ~ data2$grupo)
summary(mnv, intercept = TRUE)
summary(mnv, intercept = TRUE, test = "Wilks")
summary(mnv, intercept = TRUE, test = "Hotelling")
summary(mnv, intercept = TRUE, test = "Roy")
etasq(mnv)

describeBy(Y, data2$grupo)



# Psicometria Online Academy

#######################################
#######################################
# 
#         ANOVA ONE-WAY
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
dados <- read_spss(file = "Data One-Way e Fatorial.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describeBy(dados$FS, group = dados$Estciv)
descritivos


# Realizando a ANOVA
# a felicidade subjetiva (FS) difere entre solteiros, namorandos e casados?
anova_estciv <- oneway.test(formula = FS ~ Estciv,
                            data = dados,
                            var.equal = FALSE # F de Welch
                            )
anova_estciv

# Realizando os testes post hoc

post_hoc <- pairwise_t_test(formula = FS ~ Estciv,
                            data = dados,
                            p.adjust.method = "bonferroni")
post_hoc

# Valores possíveis para p.adjust.method:
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
# Use ?p.adjust para ver as diferenças entre as opções 


# Calculando tamanho de efeito para os testes post hoc
efeito_post_hoc <- cohens_d(formula = FS ~ Estciv,
                            data = dados, 
                            ci= TRUE,
                            ci.type = "bca",
                            nboot = 5000)

efeito_post_hoc

# Convertendo d de Cohen para r de Pearson
d2r(efeito_post_hoc$effsize)




#######################################
#######################################
# 
#         ANOVA FATORIAL
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("rstatix")
# install.packages("boot")
library(haven)
library(psych)
library(rstatix)
library(boot)

# file.choose()
dados <- read_spss(file = "Data One-Way e Fatorial.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describeBy(dados$FS,
                          # lista para incluir mais de 1 VI em describeBy
                          group = list(dados$Estciv, dados$Sexo)
                          )
descritivos


# Realizando a ANOVA
# os niveis de felicidade subjetiva (FS) variam em funcao do sexo
# e do estado civil dos participantes?
result_anova <- aov(formula = FS ~ Estciv * Sexo,
                    data = dados)

# apresentando os resultados da ANOVA de uma maneira mais amigavel
Anova(result_anova, type = "III")

# tamanhos de efeito
efeito_anova <- partial_eta_squared(result_anova)
efeito_anova

# Realizando os testes post hoc
# comparacao dos estados civis, separadamente, por sexo 
post_hoc1 <- by(data = dados,                   # banco de dados
               INDICES = dados$Sexo,            # VI que divide banco em by
               FUN = pairwise_t_test,           # funcao a ser executada
               formula = FS ~ Estciv,           # contraste de interesse
               p.adjust.method = "bonferroni")  # correcao para comparacoes multiplas

post_hoc1

# Valores possíveis para p.adjust.method:
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
# Use ?p.adjust para ver as diferenças entre as opções 


# Realizando os testes post hoc
# comparacao dos sexos, separadamente, por estados civis
post_hoc2 <- by(data = dados,                    # banco de dados
                INDICES = dados$Estciv,          # VI que divide banco em by
                FUN = pairwise_t_test,           # funcao a ser executada
                formula = FS ~ Sexo,             # contraste de interesse
                p.adjust.method = "bonferroni")  # correcao para comparacoes multiplas

post_hoc2


# tamanhos de efeito
# post hoc 1
efeito_post_hoc1 <- by(data = dados,
                       INDICES = dados$Sexo,
                       FUN = cohens_d,
                       formula = FS ~ Estciv)

efeito_post_hoc1

# post hoc 2
efeito_post_hoc2 <- by(data = dados,
                       INDICES = dados$Estciv,
                       FUN = cohens_d,
                       formula = FS ~ Sexo) 

efeito_post_hoc2

# Opção de post hoc para casos de com heterogeneidade das variâncias
post_hoc_gh <- by(data = dados,
                  INDICES = dados$Sexo,
                  FUN = games_howell_test, # games-howell
                  formula = FS ~ Estciv)

post_hoc_gh

# Realizando o bootstrappping

# Primeiramente precisamos criar uma função para extrair os valores sobre que serão reamostrados
mean_boot <- function(data, indices)
{
  val = na.omit(data[indices, ]) # indices são os casos sorteados para o bootstrapping
  
  # Abaixo indicamos os valores que devem ser calculados em cada reamostragem
  mean_1 <- mean(val$FS[val$Estciv == "Solteiro(a)"]) - mean(val$FS[val$Estciv == "Casado(a)"])
  mean_2 <- mean(val$FS[val$Estciv == "Solteiro(a)"]) - mean(val$FS[val$Estciv == "Namorando/Noivo"])
  mean_3 <- mean(val$FS[val$Estciv == "Namorando/Noivo"]) - mean(val$FS[val$Estciv == "Casado(a)"])
  
  # Os valores retornados por esta função
  return(c(
    "Solteira-Casada" = mean_1,
    "Solteira-Namorando" = mean_2,
    "Casada-Namorando" = mean_3
  ))
}

# O bootstrapping
bootstrap_mean <- boot(data = dados,
                       R = 5000,
                       statistic = mean_boot)

# Criando uma tabela mais apresentável para os resultados
resultados_boot <-  rbind(
  "Solteira-Casada" = boot.ci(bootstrap_mean, type = "bca", index = 1)$bca[4:5],
  "Solteira-Namorando" = boot.ci(bootstrap_mean, type = "bca", index = 2)$bca[4:5],
  "Casada-Namorando" = boot.ci(bootstrap_mean, type = "bca", index = 3)$bca[4:5]
)

colnames(resultados_boot) <-  c("IC 95% Inferior", "IC 95% Superior")
resultados_boot <- round(resultados_boot, digits = 3)

resultados_boot



#######################################
#######################################
# 
#               ANCOVA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("rstatix")
# install.packages("boot")
library(haven)
library(psych)
library(rstatix)
library(boot)

# file.choose()
dados <- read_spss(file = "Data One-Way e Fatorial.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describeBy(dados$FS,
                          group = list(dados$Estciv, dados$Sexo))
descritivos


# Realizando a ANCOVA

# O modelo de ANCOVA assume que a covariável não possui uma interação com a VI.
# Logo, temos que inserir a covariável no modelo e omitir o termo de interação.

result_ancova <- aov(formula = FS ~ Estciv + Sexo,
                     data = dados)
Anova(result_ancova, type= "III")

efeito_ancova <- partial_eta_squared(result_ancova)
efeito_ancova

# Realizando os testes post hoc

post_hoc <- pairwise_t_test(formula = FS ~ Estciv,
                            data = dados,
                            p.adjust.method = "bonferroni")

# Valores possíveis para p.adjust.method:
# c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY",
#   "fdr", "none")
# Use ?p.adjust para ver as diferenças entre as opções 

post_hoc


efeito_post_hoc <- cohens_d(formula = FS ~ Estciv, data = dados)
efeito_post_hoc

# Opção de post hoc para casos de com heterogeneidade das variâncias
post_hoc_gh <- by(data = dados, INDICES = dados$Sexo, FUN = games_howell_test,
                  formula = FS ~ Estciv)
post_hoc_gh

# Realizando o bootstrappping

# A realização do bootstraping da ancova segue a mesma lógica do procedimento da anova fatorial.

# Primeiramente precisamos criar uma função para extrair os valores sobre que serão reamostrados
mean_boot <- function(data, indices)
{
  val = na.omit(data[indices, ]) # indices são os casos sorteados para o bootstrapping
  
  # Abaixo indicamos os valores que devem ser calculados em cada reamostragem
  mean_1 <- mean(val$FS[val$Estciv == "Solteiro(a)"]) - mean(val$FS[val$Estciv == "Casado(a)"])
  mean_2 <- mean(val$FS[val$Estciv == "Solteiro(a)"]) - mean(val$FS[val$Estciv == "Namorando/Noivo"])
  mean_3 <- mean(val$FS[val$Estciv == "Namorando/Noivo"]) - mean(val$FS[val$Estciv == "Casado(a)"])
  
  # Os valores retornados por esta função
  return(c(
    "Solteira-Casada" = mean_1,
    "Solteira-Namorando" = mean_2,
    "Casada-Namorando" = mean_3
  ))
}

# O bootstrapping
bootstrap_mean <- boot(data = dados,
                       R = 5000,
                       statistic = mean_boot)

# Criando uma tabela para os resultados
resultados_boot <-  rbind(
  "Solteira-Casada" = boot.ci(bootstrap_mean, type = "bca", index = 1)$bca[4:5],
  "Solteira-Namorando" = boot.ci(bootstrap_mean, type = "bca", index = 2)$bca[4:5],
  "Casada-Namorando" = boot.ci(bootstrap_mean, type = "bca", index = 3)$bca[4:5]
)

colnames(resultados_boot) <-  c("IC 95% Inferior", "IC 95% Superior")
resultados_boot <- round(resultados_boot, digits = 3)

resultados_boot



#######################################
#######################################
# 
#       ANOVA MEDIDAS REPETIDAS
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
dados <- read_spss(file = "ANOVA MR.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describe(dados, omit = TRUE)
descritivos


# Realizando a ANOVA de Nedidas Repetidas

# Antes de realizar a ANOVA de medidas repetidas é necessário alterar os dados para o formato longo
# No formato longo, todas as medidas repetidas (variáveis dependentes) ficam na mesma coluna e 
# criaremos uma nova coluna para indicar a variável independente, ou seja,
# o momento da observação (T1, T2 e T3).
# Desta forma, dados do mesmo participante aparecerão em três linhas diferentes, logo
# precisaremos de mais uma coluna para identificação dos participantes:
dados$ID <- 1:nrow(dados)

# Utilizaremo a função pivot_longer(), do pacote tidyr para alterar o formato dos dados

dados_lg <- pivot_longer(data = dados, 
                         names_to = "momento",  # Nova coluna com a var. independente
                         names_prefix = "Satis_Trab_", #retira os prefixos em comum dos nomes das variáveis
                         values_to = "satis_trab", # Nova coluna com a var. dependente
                         cols = c(Satis_Trab_T1, 
                                  Satis_Trab_T2, 
                                  Satis_Trab_T3)) # variáveis que serão convertidas para o formato longo

head(dados_lg)

# Vamos utilizar a função anova_test(), do pacote rstatix. Ela assume os seguintes argumentos:
# 
# data: nossos dados, em formato longo

# dv: nome da variável que contém os resultados

# wid: nome da variável que identifica os sujeitos

# within: nome da variável que identifica as medições, a VI

resultado <- anova_test(data = dados_lg,
                        dv = satis_trab,
                        wid = ID,
                        within = momento,
                        effect.size = "pes")
resultado

# Este resultado apresenta dados do modelos, teste de Mauchly e 
# correções de Greenhouse-Geisser e Hyunh-Feldt para desvios de esfericidade

# Testes post-hoc
post_hoc <- pairwise_t_test(data = dados_lg,
                            formula = satis_trab ~ momento,
                            paired = TRUE)
post_hoc

# Tamanhos de efeito para testes post hoc
efeito <- cohens_d(data = dados_lg,
                   formula = satis_trab ~ momento,
                   paired = TRUE)
efeito




#######################################
#######################################
# 
#               MANOVA
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
dados <- read_spss(file = "MANOVA.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describeBy(dados,
                          group = list(dados$Sexo, dados$Filhos),
                          omit = TRUE)
descritivos


# Realizando a MANOVA

#  Vamos unir as varáveis dependentes no mesmo objeto
VDs <- cbind(dados$Perseverança, dados$Flexibilização)

resultado <- aov(VDs ~  Sexo * Filhos, data = dados)

summary.manova(resultado, intercept = TRUE) # Traço de Pilai é o padrão 
summary.manova(resultado, intercept = TRUE, test = "Wilks")
summary.manova(resultado, intercept = TRUE, test = "Hotelling")
summary.manova(resultado, intercept = TRUE, test = "Roy")

# Resultados univariados
summary(resultado, intercept = TRUE)

# Tamanhos de efeito das diferenças
cohens_d(data = dados, formula = Flexibilização ~ Sexo, ci = TRUE)
cohens_d(data = dados, formula = Flexibilização ~ Filhos, ci = TRUE)
