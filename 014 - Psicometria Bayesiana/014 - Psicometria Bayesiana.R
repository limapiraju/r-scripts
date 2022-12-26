# 014 - Psicometria Bayesiana

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/014 - Psicometria Bayesiana/"
setwd(path)


######### Escore de soma====
### Gerar dados aleatorios de padrao de resposta de um estudante em 100 testes,
### com 4 questoes cada. O vi?e verdadeiro do estudante eh igual a 0,50.
set.seed(123)
n     <- 100
probs <- .5
x     <- rbinom(n, 4, probs)
  
### Ajustar os dados a um modelo binomial
install.packages("fitdistrplus")
library(fitdistrplus)
fitdistrplus::fitdist(x, "binom", fix.arg = list('size' = 4), start = list("prob" = .7))

### Transformar os dados para um vetor binario
bin <- as.vector(sapply(seq_along(x), function(g) rep(c(0,1), c(4-x[g], x[g]))))
## Ajustar os dados a um modelo de Bernoulli
fitdistrplus::fitdist(bin, "binom", fix.arg=list('size'=1), start=list("prob"=.7))

### Usar o modelo Bayesiano
install.packages("remotes")
remotes::install_github("vthorrf/bsem")
install.packages("R2jags")
library(R2jags)
require(bsem)
modeloBayes <- bern.score(bin)
## Recuperar a estimativa pontual
modeloBayes$abil
## Recuperar HDI da estimativa
modeloBayes$abilHDI[,1]
## Distribui??o posteriori da estimativa
plot(density(modeloBayes$abilFull[,1]), main="Distribui??o posteriori da aptid?o",
     xlab=expression(theta), ylab="Densidade")

######### Modelagem por Equa??es Estruturais Bayesiana====
require(psych)
data(bfi)
ocean <- bfi[complete.cases(bfi),1:25] # Selecionar apenas os itens do question?rio

set.seed(123)
amostra <- sample(1:nrow(ocean), 300, replace=F)
myData <- ocean[amostra,]

factors <- rep(1:5, each=5) # Definir quais itens fazem parte de qual fator

MEEB <- BSEM(myData, factors)

MEEB$output

MEEB$corr

plot(MEEB$abil)

######### MEEB - Dois par?metros log?sticos====
myData <- myData - 1
min(myData) == 0
RevData <- reverse.code(c(-1,1,1,1,1, 
                          1,1,1,-1,-1, 
                          -1,-1,1,1,1, 
                          1,1,1,1,1, 
                          1,-1,1,1,-1), myData)

k <- max(myData)
MEEBIRT <- BSEMIRT(RevData, factors, k=k)

MEEBIRT$output

MEEBIRT$corr

plot(MEEBIRT$abil)

######### Compara??o====
MEEB$dic; MEEBIRT$dic

####====---- FIM ----====####