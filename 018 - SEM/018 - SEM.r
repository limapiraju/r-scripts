# 018 - SEM

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psic�logos/scripts-R/018 - SEM/"
setwd(path)

####INSTALANDO E CARREGANDO PACOTES
install.packages('simsem') #Instala o pacote simsem
install.packages('lavaan') #Instala o pacote lavaan
library(simsem) #Carrega o pacote simsem para iniciar a simulacao de dados
library(lavaan) #Carrega o pacote lavaan para realizar analises SEM

####SIMULANDO BANCO DE DADOS

##Modelo de mensuracao
loading <- matrix(0, 6, 2) #Cria uma matriz com 6 linhas e 2 colunas preenchida com 0
loading[1:4, 1] <- NA # Especifica par?metros livres com NA e cargas fatoriais do fator 1
loading[4:6, 2] <- NA # Especifica par?metros livres com NA e cargas fatoriais do fator 2
loadingValues <- matrix(0, 6, 2)  # Cria a matriz de dados com 6 linhas e 2 colunas
loadingValues[1:3, 1] <- -0.7 #Especifica carga fatorial de 0.7 para os itens de 1 a 3 do fator 1
loadingValues[4, 1] <- -0.25 #Especifica carga fatorial de 0.25 para o item 4 no fator 1
loadingValues[4:6, 2] <- -0.7 #Especifica carga fatorial de 0.7 dos itens 4 a 6  no fator 2
LY <- bind(loading, loadingValues) #Insere os dados de cargas fatoriais no  objeto LY
cor.fatores <- matrix(NA, 2, 2) #Cria uma matriz 2x2 preenchida com NA nomeada cor.fatores
diag(cor.fatores) <- 1 #Substitui a diagonal da matriz cor.fatores preenchida com NA por 1
RPS <- binds(cor.fatores, 0) #Substitui os valores de NA por 0 na matriz cor.fatores
RTE <- binds(diag(6)) #Cria uma matriz 6x6 preenchida com 1 na diagonal e 0 nos demais campos
VY <- bind(rep(NA,6),2) #Cria um vetor para representar a variancia residual com o valor 2
##Modelo de relacoes
path <- matrix(0, 2, 2) #Cria uma matriz com 2 linhas e 2 colunas preenchida com 0
path[2, 1:2] <- NA #Especifica par?metros livres com NA
path.start <- matrix(0, 2, 2) #Cria uma matriz com 2 linhas e 2 colunas preenchida com 0
path.start[2, 1] <- .5 # Substitui a linha 2 da coluna 1 por 0.5
BE <- bind(path, path.start) #Cria uma matriz para representar o modelo de relacoes
##Realizando a simulacao
modelo_simulado <- model(LY = LY, RPS = RPS, RTE = RTE, BE = BE, modelType = "Sem") ##Cria o modelo a ser simulado
dat <- generate(modelo_simulado,2000, set.seed = 666) #Gera o banco simulado
## Transformando as variaveis do banco dat em variaveis em escala Likert
dat$y1<-cut(dat$y1,breaks=c(4), labels=c("1","2","3","4"))
dat$y1<-as.numeric(dat$y1)
dat$y2<-cut(dat$y2,breaks=c(4), labels=c("1","2","3","4"))
dat$y2<-as.numeric(dat$y2)
dat$y3<-cut(dat$y3,breaks=c(4), labels=c("1","2","3","4"))
dat$y3<-as.numeric(dat$y3)
dat$y4<-cut(dat$y4,breaks=c(4), labels=c("1","2","3","4"))
dat$y4<-as.numeric(dat$y4)
dat$y5<-cut(dat$y5,breaks=c(4), labels=c("1","2","3","4"))
dat$y5<-as.numeric(dat$y5)
dat$y6<-cut(dat$y6,breaks=c(4), labels=c("1","2","3","4"))
dat$y6<-as.numeric(dat$y6)
## Exportando dados em formato .csv a ser usado no Jasp
write.csv(dat,"C:/Users/User/Desktop/tutoriais-main/19_Uma introdução à Modelagem por Equações Estruturais/", row.names = TRUE) ##Matriz 'dat' sera salva em "c:/" mantendo o nome das variaveis

####REALIZANDO ANALISES SEM
###Modelo 1 - Sem cargas cruzadas
##Especificando o modelo 1 sem cargas cruzadas
modelo1 <- '
f1 =~ y1 + y2 + y3
f2 =~ y4 + y5 + y6
f2 ~ f1
'
##Rodando a analise SEM do modelo 1
fit1<-sem(modelo1, dat,estimator="WLSMV", mimic="Mplus",
          ordered=c("y1","y2","y3","y4","y5","y6"),std.lv=TRUE)
summary(fit1,standardized=TRUE,fit.measures=TRUE)
##Solicitando os indices de modificacao do modelo 1
modindices(fit1)

###Modelo 2 - Com cargas cruzadas
##Especificando modelo 2 com a carga cruzada do item y4
modelo2 <- '
f1 =~ y1 + y2 + y3 + y4
f2 =~ y4 + y5 + y6
f2 ~ f1
'
##Rodando a analise SEM do modelo 2
fit2<-sem(modelo2, dat,estimator="WLSMV", mimic="Mplus",
          ordered=c("y1","y2","y3","y4","y5","y6"),std.lv=TRUE)
summary(fit2,standardized=TRUE,fit.measures=TRUE)

####COMPARANDO O AJUSTE ENTRE MODELOS
lavTestLRT(fit1, fit2, test = "satorra.2000") #Diferenca de ajuste entre os modelos usando o teste de Satterthwaite para estimador WLSMV

###INFORMACOES DE LICENCIAMENTO
##Script de analise e simulacao de dados desenvolvido por Hauck, Costa e Cortez (2020)
##Uso permitido para fins comerciais, educacionais e de pesquisa, desde que citada a fonte e mantida a atribuicao de licenciamento igual.
##Registrado com uma Licen?a Creative Commons Atribui??o-CompartilhaIgual 4.0 Internacional.