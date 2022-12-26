# 008 - Regressao Linear Simples e Múltipla

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/limap/OneDrive/Área de Trabalho/Python para Psicólogos/scripts-R/008 - Regressão Linear Simples e Múltipla/"
setwd(path)

# Calculo de tamanho amostral
install.packages("WebPower")  

# Carregando a biblioteca
require(WebPower)

# Tamanho de amostra considerando diferentes cenarios
inicio = 100; termino = 800; passo = 10
n <- seq(inicio, termino, passo)

# Nivel de significÃ¢ncia alfa
alfa = 0.05

# Tamanho do efeito f2 
R2completo = 0.1; R2reduzido = 0.05
f2 <- (R2completo - R2reduzido) / (1 - R2completo)

# Numero de preditores modelo completo (Pcompleto) e reduzido (Preduzido)
Pcompleto = 6; Preduzido = 3
res <- wp.regression(n, Pcompleto, Preduzido, f2, alfa)

# Grafico de analise de poder do teste
plot(res)



# Simulacaoo dos Dados
set.seed(123) 

# Define n como o tamanho da amostra a ser considerada na simulacao
n <- 600

#  Variaveis independentes (exogenas)
'''
Distribuicoes de probabilidade sao usadas na simulacao para definir
as VIs de acordo com sua distribuicao natureza e nivel de mensuracao

# Exemplo - QI Inteliencia tende a ter uma distribuicao normal na 
populacao com media igual a 100 (m = 100) e desvio padrao igual a 15 (dp = 15)

# Adotando QI como a variavel X1, podemos simula-la utilizando a
funcao rnorm(n,m,dp) que gera n valores aleatorios que seguem uma
distribuicao normal com media igual a m e desvio padrao igual a dp 

# Ex. X1 <- rnorm(n,m = 100,sd = 14) # o valor de n ja foi definido acima
'''

# Continuas com Distribuicao normal
# Parametros n = tamanho da amostra, m = media e dp = desvio padrao
# A funcao round(x, digits = 0) arredonda os valores para o numero
# de digitos definido em digits

X1 <- round(rnorm(n = n, m = 100, sd = 14), 1)     # VI - Inteligencia
X2 <- round(rnorm(n = n, m = 20, sd = 4), 1)       # VI - Motivacao
X3 <- round(rnorm(n = n, m = 10, sd = 2), 1 )      # VI - Ansiedade

# Contagens com Distribuicao Poisson
# Parametros n = tamanho da amostra e
# lambda = equivalente a m = dp em uma distribuicao normal,
# que caracteriza uma distribuicao Poisson

# Numero de recursos percebidos como uteis 
X4 <- rpois(n = n, lambda = 10)       #VI - N recursos utilizados (equivale a m = 10 e dp = 10)

# Variavel Moderadora 

# Distribuicao binomial indicando probabilidade de ocorrencia de um "sucesso" 
# como modelo para variaveis dicotomicas
W <- rbinom(n, 1, 0.5) # W - Design instrucional Personalizado da Plataforma
# Personalizado = 1  vs. Padrao = 0

# Variaveis Controle

C1 <- round(rnorm(n, 70, 10), 1)      # Indice de Rendimento Academico (Semestre Anterior)
C2 <- round(rnorm(n, 22, 2), 0)       # Idade em anos
C3 <- rbinom(n, 1, 0.4)             # Proporcao por Sexo 1 = Feminino 0 = Masculino
C4 <- rbinom(n, 2, 0.2)             # Proporcao de que Trabalho/Estagio 2 = Estudante; 1 = Estagio; 0 = Atividade Remunerada;

# Variaveis Mediadoras (Endogenas)
# Neste caso, variaveis mediadoras definidas pelas VIs e moderadas por W e X3

# M ~ X1 + X2 + X3 + X4  + (1 + X1 + X2 + X3 + X4) * W + erro

# Modelo M1 - Atitudes Positivas EAD

a1.1 <-  .4 # Inteligencia (efeito de X1 em M1)
a1.2 <-  .8 # Motivacao (efeito de X2 em M1)
a1.3 <- -.5 # Ansiedade (efeito X3 em M1)

a1.13 <- 0.2 # Interacao Inteligencia vs Ansiedade
a1.23 <- 0.2 # Interacao Motivacao vs Ansiedade

a1.4 <-  .3 # Recursos (efeito de X4 on M1)
a1.5 <-  .2 # Design Instrucional (efeito de X5 on M1)
a1.6 <-  .01 # Design x Inteligencia (Mod efeito de X1 on M1)
a1.7 <-  .5 # Design x  Motivacao (Mod efeito de X2 on M1)
a1.8 <- -.6 # Design x  Ansiedade (Mod efeito X3 on M1)
a1.9 <-  .2 # Design x Recursos (Mod efeito de X4 on M1)

# Erro aleatorio do modelo 
erro_a1 <- rnorm(n, 0,1)

# Modelo da variavel mediadora M1 - representando 
M1 <- a1.1 * scale(X1) + a1.2 * scale(X2) + a1.3 * scale(X3) + 
  a1.4 * scale(X4) + a1.5 * W + a1.6 * scale(X1) * W + a1.7 * scale(X2) * 
  W + a1.8 * scale(X3) * W + a1.9 * scale(X4) * W + scale(erro_a1) + 
  a1.13 * scale(X1) * scale(X3)  + a1.23 * scale(X2) * scale(X3)

# Arredondamento 
M1 <- round(M1, 1)

# Retornando para a escala anterior
M1 <- round(M1 * sd(M1) + 25, 1)

summary(M1)
hist((M1))

M1 <- as.numeric(M1)

# Modelo M2 - Resolucao de Problemas 

a2.1 <-  .8 # Inteligencia (efeito de X1 on M1)
a2.2 <-  .2 # Motivacao (efeito de X2 on M1)
a2.3 <- -.3 # Ansiedade (efeito X3 on M1)

a2.13 <- 0.5 # Interacao Intelegencia vs Ansiedade
a2.23 <- 0.5 # Interacao Motivacao vs Ansiedade

a2.4 <-  .6 # Recursos (efeito de X4 on M1)
a2.5 <-  .3 # Design Instrucional (efeito de X5 on M1)
a2.6 <-  .1 # Design x Inteligencia (Mod efeito de X1 on M1)
a2.7 <-  .1 # Design x  Motivacao (Mod efeito de X2 on M1)
a2.8 <- -.1 # Design x  Ansiedade (Mod efeito X3 on M1)
a2.9 <-  .2 # Design x Recursos (Mod efeito de X4 on M1)

# Erro aleatorio do modelo 
erro_a2 <- rnorm(n, 0, 1)

# Modelo da variavel mediadora M1 - representando 
M2 <- a2.1 * scale(X1) + a2.2 * scale(X2) + a2.3 * scale(X3) + 
  a2.4 * scale(X4) + a2.5 * W + a2.6 * scale(X1) * W + a2.7 * 
  scale(X2) * W + a2.8 * scale(X3) * W + a2.9 * scale(X4) * W + 
  a2.13 * scale(X1) * scale(X3) + a2.23 * scale(X2) * scale(X3) + 
  scale(erro_a2) 

# Retornando para a escala anterior
M2 <-round((M2 * sd(M2)) + 50, 1)

summary(M2)
hist((M2))

M2 <- as.numeric(M2)

# Variavel Dependente (Endogena)

# O modelo para a VD considera que esta e definida pelas VDs, VMs, VCs e
# influencia da W

# Efeitos Diretos com Moderacao de X3 (Ansiedade)
cdash1 <- .6 
cdash2 <- .5 
cdash3 <- -.5  
cdash4 <-  .3  
cdash13 <-  0.3
cdash23 <-  0.3

# Trajetorias b (M sobre Y) - com moderacao de W e X3
b1 <- .6 
b2 <- .7 
b1x3 <- -0.3
b2x3 <- -0.4
b1w <- -0.5
b2w <- -0.5

# Moderacoes de W em C
ct1 <- .6
ct2 <- .2
ct3 <- .2
ct4 <- -.4

# Controles 

cw <-   0.5
cw3 <- -0.4
cw2 <-  0.3
cw1 <-  0.3
cw <-   0.5

# Modelo VD
Y <- (cdash1 * scale(X1) + cdash2 * scale(X2) + cdash3 * scale(X3) + 
       cdash4 * scale(X4) + cdash13 * scale(X1) * scale(X3) + 
       cdash23 * scale(X2) * scale(X3) + b1 * scale(M1) + b2 * scale(M2) + 
       b1x3 * scale(M1) * scale(X3) + b2x3 * scale(M2) * scale(X3) + 
       b1w * scale(M1) * W + b2w * scale(M2) * W + cw * W +
       cw1 * scale(X1) * W + cw2 * scale(X2) * W + cw3 * scale(X3) * W + 
       ct1 * scale(C1) + ct2 * scale(C2) + ct3 * C3 + ct4 * C4 + 
       scale(rnorm(n, 0, 3)))

Y <- round(Y, 1)
Y <- round(1.5 * Y * sd(Y) + rnorm(n, 60, 2), 1)

hist(Y)
summary(Y)
Y <- as.numeric(Y)

# Criacao do Banco de Dados .csv
# Padroniza as variaveis para analise de moderacao 
Yscale <- scale(Y)
X1scale <- scale(X1)
X2scale <- scale(X2)
X3scale <- scale(X3)
X4scale <- scale(X4)
C1scale <- scale(C1)
C2scale <- scale(C2)
M1scale <- scale(M1)
M2scale <- scale(M2) 

#Criando Banco de dados
data <- cbind(Y, X1, X2, X3, X4, C1, C2, C3, C4, W, M1, M2, 
              Yscale, X1scale, X2scale, X3scale, X4scale, C1scale,
              C2scale, M1scale, M2scale)
data <- as.data.frame(data)

# Nomeando Variaveis Categoricas
data$W <- factor(as.factor(data$W), c(0, 1),
                 labels = c("Padrao", "Personalizado"))
data$C3 <- factor(as.factor(data$C3), c(0, 1),
                  labels = c("Masculino", "Feminino"))
data$C4 <- factor(as.factor(data$C4),c(0, 1, 2),
                  labels = c("Ativ.Remunerada", "Estagio", "Estudante"))


# Adicionando Rotulo de variaveis 
names(data) <- c("Rendimento Escolar", "Inteligencia", "Motivacao",
                 "Ansiedade", "Numero de Recursos", "Indice Academico",
                 "Idade","Sexo","Trabalha Atualmente",
                 "Design Instrucional","Atitude Positiva EAD",
                 "Resolucao de Problemas","Rendimento EscolarZ",
                 "InteligenciaZ","MotivacaoZ","AnsiedadeZ",
                 "Numero de Recursos","Indice AcademicoZ","IdadeZ",
                 "Atitude Positiva EADZ","Resolucao de ProblemasZ")

# Organizando ordem das variaveis
data <- data[,c("Rendimento Escolar", "Inteligencia","Motivacao",
                "Ansiedade", "Idade", "Sexo", "Trabalha Atualmente",
                "Design Instrucional","Atitude Positiva EAD",
                "Resolucao de Problemas", "Rendimento EscolarZ",
                "InteligenciaZ","MotivacaoZ","AnsiedadeZ",
                "Numero de Recursos", "Indice AcademicoZ","IdadeZ",
                "Atitude Positiva EADZ","Resolucao de ProblemasZ",
                "Numero de Recursos","Indice Academico")]

# Salvando Arquivo
write.csv(data, "data.csv", fileEncoding = "utf-8")

# Estatisticas Descritivas Variavel Desfecho 
install.packages("jmv")
library(jmv)

jmv::descriptives(
  data = data,
  vars = vars('Rendimento Escolar'),
  hist = TRUE,
  dens = TRUE,
  box = TRUE,
  violin = TRUE,
  dot = TRUE,
  qq = TRUE,
  sd = TRUE,
  skew = TRUE,
  kurt = TRUE,
  sw = TRUE)

# Analise de Regressao Multipla
jmv::linReg(
  data = data,
  dep = 'Rendimento Escolar',
  covs = vars(Motivacao, Ansiedade),
  blocks = list(
    list(
      "Ansiedade",
      "Motivacao")),
  refLevels = list(),
  r2Adj = TRUE,
  modelTest = TRUE,
  anova = TRUE,
  ci = TRUE,
  stdEst = TRUE,
  ciStdEst = TRUE,
  norm = TRUE,
  qqPlot = TRUE,
  resPlots = TRUE,
  durbin = TRUE,
  collin = TRUE,
  cooks = TRUE,
  emMeans = ~ Ansiedade + Motivacao,
  emmTables = TRUE)


# Analise Regressao Multipla Hierarquica com Moderacao
jmv::linReg(
  data = data,
  dep = 'Rendimento Escolar',
  covs = vars(Idade, Motivacao, Inteligencia, Ansiedade),
  factors = Sexo,
  blocks = list(
    list(
      "Idade",
      "Sexo",
      "Inteligencia"),
    list(
      "Ansiedade",
      "Motivacao"),
    list(
      c("Motivacao", "Ansiedade"))),
  refLevels = list(
    list(
      var="Sexo",
      ref="Feminino")),
  r2Adj = TRUE,
  modelTest = TRUE,
  anova = TRUE,
  ci = TRUE,
  stdEst = TRUE,
  ciStdEst = TRUE,
  norm = TRUE,
  qqPlot = TRUE,
  resPlots = TRUE,
  durbin = TRUE,
  collin = TRUE,
  cooks = TRUE,
  emMeans = ~ Ansiedade + Motivacao + Ansiedade:Motivacao,
  emmTables = TRUE)

# Analise Moderacao com Bootstrap 
medmod::mod(
  data = data,
  dep = 'Rendimento Escolar',
  mod = 'Motivacao',
  pred = 'Ansiedade',
  estMethod = "bootstrap",
  ci = TRUE,
  simpleSlopeEst = TRUE,
  simpleSlopePlot = TRUE)

# Modelo de Mediacao Simples
medmod::med(
  data = data,
  dep = 'Rendimento Escolar',
  med = 'Atitude Positiva EAD',
  pred = 'Ansiedade',
  estMethod = "bootstrap",
  ci = TRUE,
  pm = TRUE,
  paths = TRUE,
  label = TRUE,
  estPlot = TRUE)



# Psicometria Online Academy

#######################################
#######################################
# 
#      REGRESSÃO LINEAR SIMPLES
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("car")
# install.packages("lm.beta")
# install.packages("ggplot2")
library(haven)
library(psych)
library(car)
library(lm.beta)
library(ggplot2)

# file.choose()
dados <- read_spss(file = "Dados RegLog.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describe(dados, omit = TRUE)
descritivos


# Regressão Linear

# Para fazer a regressão linear, vamos usar a função *lm()*. 
# Nela, passaremos os dados que estamos usando e uma fórmula. 
# Esta fórmula tem o seguinte formato: 
# variável dependente ~ variável_independente_1 + variável_independente_2,

fit <- lm(data = dados, FELIC ~ SOC)
summary(fit)

# Para ver os betas padronizados, vamos usar a função *lm.beta()* do pacote *lm.beta*.
betas <- lm.beta(fit)$standardized.coefficients
betas

# Realizando o bootstrapping com o pacote car
fit_boot <- Boot(object = fit,
                 R = 5000,
                 method = "case")

Confint(fit_boot,
        level = .95,
        type = "bca") # Para obter IC


ggplot(dados, aes(x= SOC, y=FELIC)) +
  geom_point() + # opcional
  geom_smooth(method = "lm", se = TRUE)


#######################################
#######################################
# 
#      REGRESSÃO LINEAR MÚLTIPLA
# 
#######################################
#######################################

# Instale e carregue os pacotes que serão utilizados
# install.packages("haven")
# install.packages("psych")
# install.packages("lm.beta")
# install.packages("car")
# install.packaages("rstatix")
# install.packages("olsrr")
library(haven)
library(psych)
library(lm.beta)
library(car)
library(rstatix)
library(olsrr)

# file.choose()
dados <- read_spss(file = "Dados RegLog.sav")
dados <- as_factor(dados)

head(dados)

## Estatísticas descritivas
descritivos <- describe(dados, omit = TRUE)
descritivos


# Regressão Linear

# Para fazer a regressão linear, vamos usar a função *lm()*. 
# Nela, passaremos os dados que estamos usando e uma fórmula. 
# Esta fórmula tem o seguinte formato: 
# variável dependente ~ variável_independente_1 + variável_independente_1,

fit <- lm(data = dados,
          FELIC ~ EXT + SOC + CONSC + NEURO + ABEXP)
summary(fit)

# Nós podemos alterar o método e o critério de entrada dos preditores

step_fit_aic <- ols_step_backward_aic(model = fit, details = TRUE)

# também existem as opções ols_step_forward...
step_fit_p <- ols_step_backward_p(model = fit, prem = 0.05, details = TRUE)


# Também é possível adicionar e remover preditorees manualmente
fit_4pred <- update(fit, . ~ . -ABEXP)
fit_3pred <- update(fit_4pred, . ~ . -CONSC)

anova(fit, fit_4pred, fit_3pred)

summary(fit_4pred);summary(fit_3pred)

# Para ver os betas padronizados, vamos usar a função *lm.beta()* do pacote *lm.beta*.
betas <- lm.beta(fit)$standardized.coefficients
betas


## Verificando pressupostos

# Podemos fazer o teste de Durbin-Watson com a função *durbinWatsonTest()* do pacote *car*.
durbinWatsonTest(fit)


# A distância de Mahalanobis pode ser verificada pela função 
# *mahalanobis_distance()* do pacote *rstatix*. 
# Vamos passar os dados das variáveis que estamos usando na regressão.
mahalanobis_d <-  mahalanobis_distance(data = dados[c("EXT", "SOC","CONSC","NEURO","ABEXP","FELIC")])
mahalanobis_d[mahalanobis_d$is.outlier==TRUE,]

diagnostico <- data.frame(
  valor_previsto = fitted(fit), # probabilidade prevista
  z_resid = rstandard(fit), # resíduos padronizados
  df_fit = dffits(fit), # diferença de ajuste, se o caso for deletado
  df_beta = dfbeta(fit), # diferença nos preditores, se o caso for deletado
  cook = cooks.distance(fit), # Disância de cook
  leverage = hatvalues(fit) # Alavancagem
)
diagnostico <- round(diagnostico, 3)

head(diagnostico)


# Realizando o bootstrapping com o pacote car
fit_boot <- Boot(object = fit,
                 R = 5000,
                 method = "case")
summary(fit_boot)

Confint(fit_boot,
        level = .95,
        type = "bca") # Para obter IC

