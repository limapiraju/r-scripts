# 001 - Introducao ao R

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/001 - Introdução ao R/"
setwd(path)

# citando o R
citation()

# o que uma funcao faz?
?read.table

# qual funcao realiza uma dada tarefa?
help.search("data input")

# em qual pacote uma funcao esta?
find("lowess")

# demos de funcoes do R
demo(persp)
demo(graphics)
demo(Hershey)
demo(plotmath) # operacoes matematicas


# lattice: lattice graphics for panel plots or trellis graphs
# MASS: package associated with Venables and Riples book entitled Modern Applied Statistics using S-PLUS
# mgcv: generalized additive models
# nlme: mixed-effects models (both linear and non-linear)
# nnet: feed-forward neural networks and multinomial log-linear models
# spatial: functions for kriging and point pattern analysis
# survival: survival analysis, including penalised likelihood

# pacotes usados no livro The R Book

# install.packages("akima")
# install.packages("boot")
# install.packages("car")
# install.packages("lme4")
# install.packages("meta")
# install.packages("mgcv")
# install.packages("nlme")
# install.packages("deSolve")
# install.packages("R2jags")
# install.packages("RColorBrewer")
# install.packages("RODBC")
# install.packages("rpart")
# install.packages("spatstat")
# install.packages("spdep")
# install.packages("tree")


# quais variaveis foram criadas na sessao?
objects()

# quais pacotes e dataframes estao anexados no momento?
search()

# calculos matematicos

log(42 / 7.3)
2 + 3; 5 * 7; 3 - 7
z <- 3.5 - 8i
Re(z); Im(z) # parte real e parte imaginaria

x <- sqrt((Re(z) ** 2) + Im(z) ** 2)
Mod(z)

floor(5.7) # maior inteiro antes desse valor
ceiling(5.7) # proximo inteiro

rounded <- function(x) floor(x + 0.5) # funcao que arredonda
rounded(5.7); round(5.4)
trunc(5.7) # trunca valor
round(5.7, 0) # arredonda em numero de decimais definido

choose(5, 2) # coeficiente binomial, n!/(x! (n - x)!)
runif(10) # numeros aleatorios entre 0 e 1 de uma distribuicao uniforme

22 %/% 3 # quociente inteiro
22 %% 3 # modulo da divisao

# ! & | operadores logicos (not, and, or)

x <- c(5, 3, 7, 8)
x <- as.integer(x)

gender <- factor(c("female", "male", "female", "male", "female"))
class(gender)
mode(gender)

data <- read.table("daphnia.txt", header = T)
attach(data)
head(data)

class(data$Water)
data$Water <- as.factor(data$Water)
data$Detergent <- as.factor(data$Detergent)

is.factor(data$Water)
is.factor(data$Detergent)

levels(data$Detergent)
nlevels(data$Detergent) == length(levels(data$Detergent))

tapply(Growth.rate, Detergent, mean) # VD, VI, estatistica

# reordena fatores
Detergent <- factor(Detergent, levels = c("BrandD", "BrandC", "BrandB", "BrandA"))
tapply(Growth.rate, Detergent, mean)



# operadores logicos

x <- sqrt(2)
x * x == 2
x * x - 2
all.equal(x, 2) # testa se dois objetos sao aproximadamente iguais

x <- c(NA, FALSE, TRUE)
names(x) <- as.character(x)
outer(x, x, "&")
outer(x, x, "|")

x <- 0:6
x < 4
all(x > 0)
any(x > 0)
sum(x < 4)
(x < 4) * runif(7)

# sequencias

seq(0, 1.5, 0.1) # seq de 0 a 1.5 em incrementos de 0.1

N <- c(55, 76, 92, 103, 84, 88, 121, 91, 65, 77, 99)
seq(from = 0.04, by = 0.01, along = N)
seq(from = 0.04, to = 0.28, along = N)
sequence(c(4, 3, 4, 4, 4, 5))
rep(9, 5) # numero 9 cinco vezes
rep(5, 9) # numero 5 nove vezes
rep(1:4, 2)
rep(1:4, each = 2) # de 1 a 4, repetindo cada numero 2 vezes seguida
rep(1:4, each = 2, times = 3)
rep(1:4, 1:4) # 1, uma vez; 2, duas vezes; 3, tres vezes; 4, quatro vezes
rep(1:4, c(4, 1, 4, 2))
rep(c("cat","dog","gerbil","goldfish","rat"), c(2, 3, 2, 1, 3))


gl(4, 3, 24)
Temp <- gl(2, 2, 24, labels = c("Low", "High"))
Soft <- gl(3, 8, 24, labels = c("Hard","Medium","Soft"))
M.user <- gl(2, 4, 24, labels = c("N", "Y"))
Brand <- gl(2, 1, 24, labels = c("X", "M"))
data.frame(Temp, Soft, M.user, Brand)


# teste e coercao
geometric <- function(x){
  if(!is.numeric(x)) stop ("Input must be numeric")
  if(min(x) <= 0) stop ("Input must be greater than zero")
  exp(mean(log(x))) }

geometric(c("a","b","c"))
geometric(c(0:5))
geometric(c(1:5))


# valores ausentes e coisas que nao sao numeros
a <- Inf - Inf
b <- Inf / Inf
c <- Inf
is.infinite(a)
is.infinite(c)

d <- c(4, NA, 7)
mean(d, na.rm = TRUE) # na.rm = remove NA
which(is.na(d)) # posicao de NA

d[! is.na(d)] # retorna outra sequencia sem NA

y1 <- c(1, 2, 3, NA)
y2 <- c(5, 6, NA, 8)
y3 <- c(9, NA, 11, 12)
y4 <- c(NA, 14, 15, 16)
full.frame <- data.frame(y1, y2, y3, y4)
reduced.frame <- full.frame[!is.na(full.frame$y1),]


# vetores e subscritos
peas <- c(4, 7, 6, 5, 6, 7)
class(peas)
length(peas)
mean(peas)
min(peas)
max(peas)
quantile(peas)

teste <- scan()

trim <- function(x) sort(x)[-c(1, 2,length(x) - 1,length(x))]
trim(peas)

x <- 0:10
sum(x)
sum(x < 5)
sum(x[x < 5])

y <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11)
sum(rev(sort(y))[1:3])


# funcoes vetorizadas

x <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11)
y <- c(8, 3, 5, 7, 6, 6, 8, 9, 2, 3, 9, 4, 10, 4, 11) 
mean(x)
max(x)
min(x)
sum(x)
median(x)
range(x)
var(x)
cor(x, y)
sort(x)
rank(x)
order(x)
quantile(x)
cumsum(x)
cumprod(x)
cummax(x)
cummin(x)
pmax(x,y,z)
pmin(x,y,z)
colMeans(x)
colSums(x)
rowMeans(x)
rowSums(x)
fivenum(x)

counts <- rnbinom(10000, mu = 0.92, size = 1.1) # binomial negativa
table(counts)

data <- read.table("daphnia.txt", header = TRUE)
tapply(data$Growth.rate, data$Detergent, mean, na.rm = TRUE, trim = 0) # trim = 0.2
tapply(data$Growth.rate, data$Detergent, function(x) sqrt(var(x) / length(x)))
tapply(data$Growth.rate,list(data$Detergent, data$Daphnia),mean)
aggregate(Growth.rate ~ Water, data, mean)

x <- runif(10)
y <- runif(10)
z <- runif(10)
pmin(x, y, z)
pmax(x, y, z)

xv <- rnorm(1000, 100, 10)
which(abs(xv - 108) == min(abs(xv - 108)))
closest <- function(xv,sv){
  xv[which(abs(xv - sv) == min(abs(xv - sv)))]}
closest(xv, 108)

listinha <- runif(10)
sorted <- sort(listinha) # ordem crescente
ranked <- rank(listinha) # posto dos valores
ordered <- order(listinha)

names <- c("Williams","Jones","Smith","Williams","Jones","Williams")
table(names)
unique(names)
duplicated(names)
names[!duplicated(names)]

setA <- c("a", "b", "c", "d", "e")
setB <- c("d", "e", "f", "g")
union(setA, setB)
intersect(setA, setB)
setdiff(setA, setB)
setdiff(setB, setA)

# matrizes e arrays
y <- 1:24
dim(y) <- c(2, 4, 3) # dimensoes 2 x 4 x 3
dim(y) <- c(3, 2, 4) # dimesnoes 3 x 2 x 4

X <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 3) # default = column-wise
class(X)
attributes(X)

vector <- c(1, 2, 3, 4, 4, 3, 2, 1)
V <- matrix(vector, byrow = TRUE, nrow = 2)

dim(vector) <- c(4,2)
is.matrix(vector)

X <- matrix(rpois(20, 1.5),nrow = 4)
rownames(X) <- rownames(X,do.NULL = FALSE, prefix = "Trial.")
drug.names <- c("aspirin", "paracetamol", "nurofen", "hedex", "placebo")
colnames(X) <- drug.names
dimnames(X) <- list(NULL,paste("drug",1:5, sep = " "))
X

(X <- matrix(1:24,nrow = 4))
apply(X, 1, sum)
apply(X, 2, sum)
apply(X, 1, sample)


# texto, string e padrao de correspondencias
pets <- c("cat","dog","gerbil","terrapin")
length(pets)
nchar(pets)
is.factor(pets)

df <- data.frame(pets)
is.factor(df$pets)

text <- "The quick Brown Fox jumps over the lazy dog"
toupper(text)
tolower(text)

first <- c(5,8,3,5,3,6,4,4,2,8,8,8,4,4,6)
second <- c(8,6,4,2)
match(first, second) # retorna indices em que o primeiro vetor aparecem no segundo

text <- c("arm", "leg", "head", "foot", "hand", "hindleg", "elbow")
sub("o", "O", text)
gsub("h", "H", text)


# datas e horarios
Sys.time()
as.numeric(Sys.time())
time.list <- as.POSIXlt(Sys.time())
unlist(time.list)

data <- read.table("dates.txt", header = TRUE)
attach(data)
head(data)
mode(date)
class(date)

y2 <- as.POSIXlt("2015-10-22")
y1 <- as.POSIXlt("2018-10-22")
y1 - y2

# escrevendo suas proprias funcoes

se <- function(x) sqrt(var(x)/length(x))

ci95 <- function(x) {
  t.value <- qt(0.975,length(x)-1)
  standard.error <- se(x)
  ci <- t.value*standard.error
  cat("95% CI = [", round(mean(x) -ci, 2), ", ", round(mean(x) +ci, 2),"]\n") }

x <- rnorm(150, 25, 3)
ci95(x)

xv <- rnorm(30)

sem <- numeric(30)
sem[1] <- NA
for(i in 2:30) sem[i] <- se(xv[1:i])

plot(1:30, sem, ylim = c(0, 0.8),
     ylab = "Erro Padr?o da M?dia", xlab = "Tamanho Amostral (N)", pch = 16)

lines(2:30, 1 / sqrt(2:30))



error.bars <- function(yv,z,nn){
  xv <-
    barplot(yv,ylim=c(0,(max(yv)+max(z))),names=nn,ylab=deparse(substitute(yv)
    ))
  g=(max(xv)-min(xv))/50
  for (i in 1:length(xv)) {
    lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
  }}

comp <- read.table("competition.txt", header = TRUE)
attach(comp)
names(comp)

comp$clipping <- as.factor(comp$clipping)

se <- rep(28.75,5)
labels <- as.character(levels(comp$clipping))
ybar <- as.vector(tapply(biomass,clipping,mean))
error.bars(ybar,se,labels)


xy.error.bars <- function (x,y,xbar,ybar){
  plot(x, y, pch=16, ylim=c(min(y-ybar),max(y+ybar)),
       xlim=c(min(x-xbar),max(x+xbar)))
  arrows(x, y-ybar, x, y+ybar, code=3, angle=90, length=0.1)
  arrows(x-xbar, y, x+xbar, y, code=3, angle=90, length=0.1) }


x <- rnorm(10, 25, 5)
y <- rnorm(10, 100, 20)
xb <- runif(10) * 5
yb <- runif(10) * 20
xy.error.bars(x, y, xb, yb)

# switch
central <- function(y, measure) {
  switch(measure,
         Mean = mean(y),
         Geometric = exp(mean(log(y))),
         Harmonic = 1/mean(1/y),
         Median = median(y),
         stop("Measure not included")) }

central(rnorm(100,10,2),"Harmonic")
central(rnorm(100,10,2),4)

# argumentos opcionais
charplot <- function(x, y, pc = 16, co = "red"){
  plot(y ~ x, pch = pc, col = co)}

charplot(1:10,1:10)
charplot(1:10,1:10,17)


# escrevendo em um arquivo

save(list = ls(all = TRUE), file = "session")
load(file = "session")

savehistory(file = "session18.txt")
loadhistory(file = "session18.txt")
pdf("fig1.pdf")
dev.off()


nbnumbers <- rnbinom(1000, size = 1, mu = 1.2)
write(nbnumbers,"nbnumbers.txt", 1)

xmat <- matrix(rpois(100000,0.75),nrow = 1000)
write.table(xmat, "table.txt", col.names = F, row.names = F)

nbtable <- table(nbnumbers)
nbtable
write.table(nbtable, "table2.txt", col.names = F, row.names = F)


library(psych)

# correlacao
variavel1 <- rnorm(100)
variavel2 <- rnorm(100)
cor.test(variavel1, variavel2, method = "pearson")

## regressao
amfa <- c(0, 10, 20, 30, 40, 50)
desmp <- c(4, 22, 44, 60, 82, 100)
lm.r <- lm(desmp ~ amfa)
summary(lm.r)
plot(lm.r)

## teste t
altura_homens <- rnorm(n = 1000, mean = 1.75, sd = 0.20)
altura_mulheres <- rnorm(n = 1000, mean = 1.65, sd = 0.20)

t.test(altura_homens, altura_mulheres,
       alternative = c("two.sided"), mu = 0, 
       paired = FALSE, var.equal = FALSE, conf.level = 0.95)

# ANOVA
memoria <- c(2, 13, 17, 13, 8, 5, 11, 2, 10, 14, 9, 12, 11, 18, 14)
grupo <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3)
fit <- aov(memoria ~ grupo)
anova(fit)

tapply(memoria, grupo, mean)

# qui-quadrado
produto <- c(1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1)
propaganda <- c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 2, 2)
chisq.test(produto, propaganda)

#####################################################################
#####################################################################
#####################################################################

# Codigos do Psicometria Online Accademy, Douglas Dutra

#############################
# PRIMEIROS PASSOS COM R    #
#############################

# O R funciona, primeiramente, como uma grande calculadora.
# Todas as operações básicas podem ser feitas diretamente no console.

# Exemplos:

114 + 28 # Adição

237 - 30 # Subtração

76 * 22 # Multiplicação

1284 / 13 # Divisão

32 ^ 4 # Potenciação

sqrt(9) # Raiz quadrada



# O R também faz avaliações lógicas:

8 > 2 
#[1] TRUE

8 < 2
#[1] FALSE

8 == 2
#[1] FALSE

8 != 2
#[1] TRUE



################################
# OBJETOS E CLASSES DE OBJETOS #
################################



# Todas as informações existentes no R são armazenadas em objetos.
# Um objeto pode tomar a forma de um único número, de uma variável,
# um banco de dados ou gráfico.

# Objetos são criados com o operador <- 
# (nome do objeto) <- (valor ou variável para ser armazenada)
# Ex:

a <- 35
a

b <- "Casa"
b

# Dica: utilize o atalho de teclado "Alt" + "-" para criar o <- 

##### Formas alternativas de se criar objetos

# -> (direção invertida)
40 -> c
c

# = (similar a outras liguagens, como python)
C = 20 # Alerta: não funciona dentro de funções
C

# Funçãp assign() (Útil para usar em conjunto com outras funções)
assign("d", 8)
d

## DELETANDO OBJETOS

# Todos os objetos são armazenados no Workspace, o espaço virtual do R
# A função ls() lista todos os objetos e funções que foram criados pelo usuário.

ls()

# Para deletar um objeto devemos utilizar a função rm()

rm(C)

# O comando rm(list= ls()) elimina todos os objetos do workspace.


#####


# O operador c() é utilizado para atribuir mais de um valor ao objeto.
# A função c() cria um vetor, que é o conjunto básico de informações.

e <-  c(1, 3, 6, 8, 9) # Note que vírgulas separam os elementos
e

# Podemos realizar operações sobre os vetores

2 * e

e + 10

f <- c("maçã", "banana", "abacaxi")
f

# Todas as informações de um vetor devem ser do mesmo tipo.
# Não podemos ter números e textos no mesmo vetor.
# Ao tentar misturar números e texto, o R converterá os números em texto automaticamente.

g <- c("Pincel", 4)
g

class(g)


#### Classes de objetos


# Existem, fundamentalmente, seis classes de objetos no R:
# 1- Logicals
# 2- numeric
# 3- Interger
# 4- Complex
# 5- character
# 6- Factor


# logical: TRUE, FALSE, NA - avaliações lógicas
class(TRUE)
class(e == e)
is.logical(e == e)
TRUE + TRUE

e <- c(1, 3, NA, 8, 9)
mean(e)
mean(e, na.rm = TRUE)
is.na(e)


# numeric: 2, 4.3, 5, 78 - números inteiros e decimais
class(18)
class(e)
is.numeric(e)

# integer: 1L, 7L, as.integer(4) - Números inteiros
class(45L)
is.integer(45L)

# complex: 1 + 4i, 7 +3i - números imaginários
class(4 + 1i)
is.complex(4 + 1i)

# character: "d", "asf", "troco" - textos
class(f)
is.character(f)

g <- "Meu nome é Douglas"
class(g)
paste("a", "b", "c", sep = "-")

# factor: 1, 2 ("Masculino, "Feminino") - Categorias numéricas


### Fatores

# Os fatores são equivalentes a variáveis categóricas e podem ser ordenados ou não.
# Fatores são armazenados no R como valores numéricos (levels), mas exibidos 
# como texto (labels).

escolaridade <- factor(x = c(1, 3, 4, 2, 1, 4, 3, 3, 2), 
                       levels = c(1:4),
                       labels = c("Fundamental", "Médio", "Superior", "PG"), 
                       ordered = TRUE
)

escolaridade.inv <- factor(x = c(1, 3, 4, 2, 1, 4, 3, 3, 2), 
                          levels = c(4:1),
                          labels = c("PG", "Superior", "Médio", "Fundamental"), 
                          ordered = T)

escolaridade
escolaridade.inv

genero <- factor(x = c(1, 2, 1, 2, 2, 3),
                 levels = c(1, 2),
                 labels = c("Masculino", "Feminino"))
class(genero)
levels(genero)

is.factor(genero)
is.na(genero)

# As funções as.[nome da classe]() convertem objetos em classes diferentes
# Números e fatores podem ser convertidos em characters (texto), 
# mas texto não pode ser convertido em números

as.character(a)
as.numeric(b)

as.character(genero)
as.numeric(genero) # Os factors são ambíguos, podem virar texto ou números

as.factor(b) # Factors também podem aceitar textos ou números
as.factor(a)

h <- is.na(genero) # Logicals podem virar números
as.numeric(h)

# Em resumo: Logicals -> Numeric -> Factor -> Character


################################
#      ESTRUTURA DE DADOS      #
################################

# O vetor é a estrutura de dados mais básica e 
# são os blocos que formarão estruturas mais complexas.
# Existem cinco tipos de estrutura.
# 1 - Vector
# 2 - Matrix
# 3 - Array
# 4 - Data frame
# 5 - List

# 1 - Vetor

i <- seq(from = 4, to = 160, by = 4); i
length(i) # Comprimento do vetor

# [] serve para selecionar elementos dentro do vetor

i[2]
i[2:10]
i[c(1, 5, 9, 15)]
i[i < 80] # Uma expressão lógica pode filtrar vetores
i[-6] # use - para eliminar elementos do vetor
i[-c(3, 12, 20)]
i[a] # Você pode usar outros objetos para selecionar elementos 

# Podemos substituir elementos dentro dos vetores
i[14] <- NA
i[14] <- 56

# Os elementos dos vetores podem ser nomeados com a função names()
names(i)
names(i) <- paste0("x", 1:40) # paste0() cria texto unindo os elementos da função
i
i["x6"]
i[c("x4", "x12")]


# Reciclagem

# A reciclagem é a forma como o R soluciona operações com vetores de tamanhos 
# diferentes. O vetor de menor comprimento é duplicado (reciclado) até atingir o
# tamanho do vetor maior.
# 
# A reciclagem funciona perfeitamente quando o comprimento do maior vetor é múltiplo do 
# comprimento do menor vetor. Quando este não é o caso, o R faz o cálcuolo, mas
# gera um aviso.

i + 1

i + e
length(i)/length(e)

i * c(10, 100, 1000)
length(i)/3


# Funções auxiliares
length(i) # Tamanho, número de elementos
min(i); max(i) # Mínimo e máximo
mean(i) # média
sd(i) # DP
var(i) # variância
summary(i) # Mínimo, máximo, média e quartis
table(genero) # Conta a frequência dos elementos

# 2 - Matrix

# A matrix é um objeto bidimensional (linhas e colunas) que aceita apenas dados da mesma classe
# A função matrix() é utilizada para criar matrizes
?matrix

j <- matrix(data = c(1:9), nrow = 3); j

# byrow = FALSE por default; isso significa que dados são preenchidos por colunas
k <- matrix(j, ncol = 3,  byrow = TRUE); k

matrix(data = c(f, 1:3), # dados textuais e numéricos
       nrow = 3, 
       ncol = 2
)

# A indexação da matrix segue o formato k[linhas, colunas]
# Se houver apenas um elemento nos colchetes, o R retornará os elementos naquela
# posição, seguindo a ordem vertical

k[2, 3]
k[2,]
k[,2]
k[,-2]
k[2]

# As linhas e as colunas da matrix podem ser nomeadas
dimnames(k)
colnames(k) <- paste0("col", c(1:3)); colnames(k)
rownames(k) <- paste0("row", c(1:3)); rownames(k)

# Funções auxiliares

dim(k) # Dimensões da matriz
nrow(k)
ncol(k)


# 3 - Array

# O array é um objeto multidimensional.
# Pode ser considerado uma matriz de várias páginas.

l1 <- array(data = c(1:18), dim = c(3, 3, 2)); l1

l2 <- array(data = c(1:256), dim = c(4, 4, 4, 4)); l2

dimnames(l2) <- list("Linhas" = paste0("row", c(1:4)),
                     "Colunas" = paste0("col", c(1:4)),
                     "Páginas" = paste0("p", c(1:4)),
                     "Volumes" = paste0("vol", c(1:4))
                     )

l2["row2", "col3", "p1", "vol4"]

# 4 - Data frame

# O data frame é bidimensional (como a matrix), mas aceita todos os tipos de dados.
# O data frame, obrigatoriamente, possui nomes de colunas, mas geralmente não se usa nome de linhas

df <- data.frame("country" = c("Brazil", "Germany", "Italy", "Argentina",
                               "France", "Uruguay", "England", "Spain", "Netherlands"),
                 "titles" = c(5, 4, 4, 3, 2, 2, 1, 1, 0),
                 "continent" = factor(c(1, 2, 2, 1, 2, 1, 2, 2, 2),
                                      levels = c(1, 2),
                                      labels = c("South America", "Europe")),
                 "champion_21st_century" = c(T, T, T, T, T, F, F, T, NA)
                 )

# As variaveis devem ter o mesmo comprimento ou NAs serão inseridos
df


# O data frame pode ser indexado da mesma forma que uma matriz
# df[linhas , colunas]

df[3, ]
df[3, 2]
df[4] # Se a vírgula for ocultada o R seleciona a coluna
df[-c(2, 4)]

# Mas há tambem a seleção com o operador $
# $ serve para selecionar apenas uma variável

df$country # retorna um vetor simples
df$titles
df$titles[3]



# Filtrando o data frame com vetors lógicos
h <- c(T, F, T, F, T, F, T, F, T)
df[h,] # Somente as linhas com TRUE são selecionadas

df[df$continent == "Europe",] # Não esqueça a vírgula!!!!

# Reorganizando o banco de acordo com os valores de uma variável
df[order(df$titles, decreasing = FALSE), ]



# Podemos adicionar linhas e colunas ao data frame com as funções
# rbind() e cbind(), respectivamente

rbind(df, df[4,]) #Dupliquei uma linha
cbind(df, "var"= c(1:9))

# Funções auxiliares
str(df) # Exibe o nome e tipo de todas as variáveis
head(mtcars) # retornará apenas os primeiros casos do df
tail(mtcars) # retornará apenas os últimos casos do df


# 5 - Lista

# A lista é a estrutura de dados mais complexa
# Ela pode absorver todas as outras estruturas de dados, inclusive outras listas.


m <- list("numeric" = e,
          "string" = f,
          "matrix"= k,
          "array"= l2,
          "df"= df,
          "list"= list(a, b, g, j)
)
m

# O operador $ pode ser utilizados com listas, para retornar objetos em sua classe original

m$matrix; class(m$matrix)

# Os elementos das listas podem ser indexados com colchetes simples [] ou
# colchetes duplos [[]]

# Os colchetes simples retornam uma lista com os elementos selecionados
m[5]; class(m[5])

# Os colchetes duplos retornam objetos em sua classe original
m[[5]];class(m[[5]])

m[[5]][3,]
m[[5]]$factor

# Também podemos usar a função str() para explorar listas
str(m)

# Listas são muito importantes para armazenar os resultados de funções
result <- t.test(titles ~ continent, data = df, na.rm = T)
result

str(result)


#####################################
#
#  EXERCÍCIOS DE OPERAÇÕES BÁSICAS
#
#####################################

# 1)
# Antes de rodar as linhas abaixo, tente prever qual será o resultado.
# Depois de pensar sobre o comando, verifique a resposta com o R.

x <- c(1, 3, 5, 7, 9)
y <- c(2, 3, 5, 7, 11, 13)

x + 1
y * 2

length(x); length(y)
sum(x > 5) 
sum(x[x > 5])
sum(x[x > 5 | x < 3]) # o operador | significa "ou"
y[3]
y[-3]
y[x] #(O que é NA?)
y[y >= 7]

# 2)
# Crie o vetor z com os três primeiros objetos do vetor x e os três primeiros
# objetos do vetor y.
z <- c(x[1:3], y[1:3])


# 3a)
lucro <- c(2500, 1700, 3650, 2800, 2100, 2200)
# O vetor lucro contém o lucro diário de uma loja
# Nomeie este vetor com os nomes dos dias da semana, começando pela 
# segunda-feira.
names(lucro) <- c("segunda-feira", "terça-feira", "quarta-feira", "quinta-feira",
              "sexta-feira", "sábado")

lucro

# 3b)
# O lucro da terça-feira foi digitado errado. Altere o valor de terça para 2700.
lucro["terça-feira"] <- 2700
lucro

# 4) Ao somar os vetores x e y, qual será o valor do sexto elemento do novo vetor?
# Verifique sua resposta com o R.
w <- x + y # 1 + 13 = 14
w[6]


# 5) 
sexo <- rep(c(1, 2), 14, each = TRUE)
# Transforme o vetor sexo em um fator, onde 1= Feminino e 2= Masculino
sexo <- factor(x = sexo,
               levels = c(1, 2),
               labels = c("Feminino", "Masculino"))

sexo


# 6) Transforme a variável escolaridade em um fator ordenado com a seguinte ordem:
# Fundamental < Médio < Superior < Pós-Graduação

escolaridade <- c("Fundamental","Pós-Graduação", "Superior", "Médio", "Superior",  
                  "Médio", "Fundamental", "Médio", "Fundamental", "Médio")

escolaridade <- factor(escolaridade,
                       levels = c("Fundamental", "Médio", "Superior", "Pós-Graduação"),
                       labels = c("Fundamental", "Médio", "Superior", "Pós-Graduação"),
                       ordered = TRUE
                       )
escolaridade


# 7a)  
df <- iris
# Use a função str() e verifique as classes das variáveis do banco
str(df)

# 7b) Descubra a frequência de cada espécie de flor no banco df
table(df$Species)



# 8a) Crie o vetor Sepal.ratio, dividindo os valores de Sepal.Length por Sepal.Width.
Sepal.ratio <- df$Sepal.Length / df$Sepal.Width

# 8b) Adicione Sepal.ratio ao banco df
df$Sepal.ratio <- Sepal.ratio


# 9) Crie o banco setosa somente com os casos desta espécie
setosa <- df[df$Species == "setosa", ]
setosa
table(setosa$Species)

# 10a) Crie o banco big.ratio somente com os casos onde Sepal.ratio é maior que 2.
big.ratio <- df[df$Sepal.ratio > 2, ]


# 10b) Qual a frquência das espécies no banco big.ratio?
table(big.ratio$Species)



#######################
# 
#  WORKING DIRECTORY
# 
#######################


# O working directory (wd) é a pasta de onde o R, por padrão, importa/exporta os arquivos.

# O painel Files do RStudio exibe o wd. Por este painel é possível abrir, importar,
# copiar, renomear e deletar os arquivos da pasta. Também é possível alterar a 
# pasta utilizada como wd.

# Para acessar um arquivo do wd com uma função, basta escrever o nome do arquivo 
# e sua extensão ("Banco.sav"). 
# Arquivos que estão localizados em outras pastas também podem ser acessados 
# diretamente por código, mas é necessário que o caminho completo do arquivo 
# seja especificado ("C:/User/Documents/Banco.sav").

# Para verificar qual o working directory:
getwd()
# E para alterar o working directory, basta inserir o caminho da pasta na
#  função setwd()

# A função choose.dir() facilita encontrar o caminho para a pasta de intesse.
choose.dir()

# Novo working directory
path <- "C:/Users/User/Desktop/Python para Psic?logos/scripts-R/001 - Introdução ao R/"
setwd(path)



####  IMPORTANTE !!

# O windows utiliza a barra para esquerda nos caminhos das pastas ("\"), mas 
# o R não reconhece este caracter como válido. Por isso é necessário sempre 
# utilizar a barra para direita ("/") ou barras duplas para a esquerda ("\\") 
# ao escrever os caminhos para pastas e arquivos.


#########################
#
# INSTALANDO PACOTES
#
#########################

# Os pacotes só precisam ser instalados uma vez, mas precisam ser carregados a
# cada nova sessão do R.

# Para instalar os pacotes use a função install.packages()

# install.packages("haven")

# Os pacotes podem ser carregados na sessão com a função library

library(haven)


#########################
#
# IMPORTANDO ARQUIVOS
#
#########################


### SPSS

# install.packages("haven")
# O pacote haven lê arquivos do SPSS, SAS e Stata

library(haven)
banco_spss <-read_sav("Banco_teste_t.sav") 

# É possível usar a função file.choose() para encontrar arquivos em outras pastas.

head(banco_spss) # As variáveis categóricas estão em formato numérico

# O haven importa os labels do SPSS, mas mão transforma as variáveis em fatores aautomaticamente.
# É necessários usar a função as_factor() para utilizar os labels do SPSS como fatores.
# A função zap_labels() retira as labels das categorias e dos nomes de variáveis.

banco_spss <- as_factor(banco_spss) # Transforma as variáveis categóricas em fatores
head(banco_spss)


### Excel

# install.packages("openxlsx")
library(openxlsx)

banco_excel <- read.xlsx("Banco_teste_t.xlsx")
# A função read.xlsx() possui outros argumentos, verifique a documentação 
# da função com ?read.xlsx()

head(banco_excel)
# As variáveis categóricas devem ser transformadas em factors manualmente

banco_excel$Sexo <- factor(x = banco_excel$Sexo, levels = c(1, 2), 
                           labels = c("Feminino", "Masculino")
                           )

head(banco_excel)

### .CSV

# Existem dois tipos de arquivos .csv:

# O csv simples é delimitado por vírgulas, ele é lido com a função read.csv()
# O outro tipo é o csv delimitado com ponto e vírgula (;) e deve ser 
# importado com a função read.csv2()
# Sempre verifique a forma de tabulação.

banco_csv <- read.csv("Banco_teste_t.csv") # Função errada para o arquivo
head(banco_csv)

banco_csv <- read.csv2("Banco_teste_t.csv") 
head(banco_csv)

# Tammbém é possível indicar a forma de tabulação correta com o argumento sep=
# Exemplo: read.csv("Banco_teste_t.csv", sep = ";")

# Note que o nome da primeira variável aparece como "ï..Sexo". Vamos corrigir:
names(banco_csv)[1] <- "Sexo"
head(banco_csv)

# As variáveis categóricas devem ser transformadas em factors manualmente

banco_csv$Sexo <- factor(x = banco_csv$Sexo, levels = c(1, 2), 
                         labels = c("Feminino", "Masculino")
)

head(banco_csv)

### .DAT

# Muitas vezes os arquivos ,dat não possuem os nomes das variáveis, então 
# é necessário insesrir os nomes dentro do código.

# A função read.delim() assume que os dados são separados por tabulação (sep = "\t")

banco_dat <- read.delim("Banco_teste_t.dat")
head(banco_dat) # A 1° linha do arquivo foi utilizada como nome das variáveis

banco_dat <- read.delim("Banco_teste_t.dat", header = FALSE, 
                        col.names = c("Sexo", "Idade", "Filhos", "SaudeMental", 
                                      "Satisfação", "Felicidade_t1", "Felicidade_t2")
                        )

head(banco_dat)


# Nota:

# As função read.csv() e read.delim() são aplicações da função geral read.table().
# A função read.table() pode ler arquivos .csv, .dat e outros tipos de texto, sendo
# função de importação mais maleável, mas que necessita de muitos argumentos para
# especificar o tipo de arquivo.


#########################
#
# EXPORTANDO ARQUIVOS
#
#########################

# Toda função de exportação pede duas informações básicas: o objeto que será 
# exportado e o nome ou caminho do novo arquivo. 
# Para criar um arquivo no working directory, basta passar o nome do arquivo.
# Para criar arquivos em outras pastas, é preciso passar o caminho completo para a pasta.

# Argumentos adicionais podem ser
# utilizados para definir outros parâmetros, por exemplo, se os nomes das linhas
# e colunas devem ser exportados para o arquivo.



### SPSS

# Para exportar arquivos do SPSS também podemos usar o pacote haven.
# library(haven)

write_sav(data = banco_spss, path = "Export_spss.sav")
# Note que os factors se tornam variáveis categóricas automaticamente



### Excel

# Também vamos utilizar o pacote openxlsx para exportar arquivos do excel.

write.xlsx(x = banco_excel, file = "Export_excel.xlsx")
# Os factors são exportados como texto

# Nota: Se o argumento x for uma lista, cada elemento da lista vira uma planilha
# diferente no exccel.

### CSV

write.csv2(x = banco_csv, file = "Export_csv2.csv", row.names = FALSE)
# Esta função cria nomes de linhas automaticamente, então pode ser melhor 
# especificar row.names = FALSE


### DAT

# Normalmente, o arquivo .dat deve conter apenas números. Pode ser necessário 
# converter factors para o formato numeric com as.numeric()

# Exemplo
# banco_spss$Sexo <- as.numeric(banco_spss$Sexo)



write.table(x = banco_dat, file = "Export_dat.dat", 
            sep = "\t", # indica que o arquivo é separado por tabulação
            col.names = FALSE, # omite o nome das variáveis
            row.names = FALSE  # omite número das linhas
)
# Esta é a forma de exportar arquivos para o mplus e o factor





#######################
#######################
# 
#  MANIPULANDO DADOS
# 
#######################
#######################

install.packages("janitor")

library(openxlsx)
library(janitor)

path <- "C:/Users/User/Desktop/Python para Psic?logos/scripts-R/001 - Introdução ao R/"
setwd(path)

dados_brutos <- read.xlsx("Banco MRP - dados brutos.xlsx")



########################
# Procurando por erros
########################

# Coisas que sempre precisam ser investigadas:
# 1 -	Número de colunas e linhas
# 2 -	Nome das variáveis
# 3 -	Tipo das variáveis
# 4 -	Missings
# 5 -	Valores não esperados (e. g. erros de digitação)

# Vamos verificar
# 1 -	Número de colunas e linhas
# 2 -	Nome das variáveis
# 3 -	Tipo das variáveis
str(dados_brutos)
# As variáveis sexo, estado, escolaridade, civil e renda familiar precisam ser
# transformadas em factor.
#  A variável idade aparece como character e deve ser transformada em numeric.

# 4 -	Missings
complete.cases(dados_brutos)
# cada valor FALSE corresponde a um caso incompleto. Para resumir:
sum( ! complete.cases(dados_brutos)) # Temos 28 casos incompletos

# 5 -	Valores não esperados
tabyl(dados_brutos, "sexo") # do pacote janitor
tabyl(dados_brutos, "idade")
tabyl(dados_brutos, "estado") # Não vamos repetir o mesmo comando várias vezes

# Vamos automatizar o processo para criar tabelas de frequência




################################################
# Automatizando operações: a família apply
################################################

# A família apply é composta por um conjunto de funções que funcionam de 
# forma similar: elas aplicam a mesma operação em conjunto de variáveis de forma
# simultânea.

# A função apply() aplica uma função sobre todas as linhas e/ou colunas de uma
# matriz/data frame. 
# Ela vai retornar um vetor, matriz ou lista, dependendo da função aplicada.

apply(X = , # Dados. Eles serão convetidos em uma matriz
      MARGIN = , # Se a função dave ser aplicada sobre linhas (1) ou colunas (2)
      FUN = ) # A função que será utilizada. Não é necesário escrever parênteses.

# Exemplo: 
itens1_5 <- dados_brutos[8:12]; head(itens1_5)

apply(X = itens1_5, # Dados
      MARGIN = 2, # Operação sobre as colunas
      FUN = mean, na.rm = TRUE) # Queremos a média e removemos casos NA

# O resultado é a média de cada variável.
# Se mudássemos o argumento MARGIN para 1, teríamos a média 
# de cada um dos 343 participantes em todos os 5 itens

##############################################



# Criando as tabelas de frequêcia:
freq <- apply(X = dados_brutos, MARGIN = 2, tabyl)

# Para facilitar a visualização, vamos arredondar os valores decimais.
# Vamos fazer isso com a função lapply(), que é a versão de apply() para listas.
# lapply() sempre retorna uma lista do mesmo comprimento que o seu input.

freq <- lapply(freq, adorn_rounding, digits = 2) # não há argumento MARGIN
freq
# Precisamos retirar o texto da variável idade
# As variáveis MRP6 e MRP19 possuem valores 55




########################
#
# Corrigindo o banco
#
########################


dados <- dados_brutos

# Retirando o texto de idade
# Vou utilizar uma "cola" para escrever a função:
# https://paulvanderlaken.files.wordpress.com/2017/08/r-regular-expression-cheetsheat.pdf
dados$idade <- gsub(pattern = "[[:alpha:]]", # o que queremos substituir: letras
                    replacement = "", 
                    x = dados$idade)

# verificando a mudança
as.data.frame(table(dados$idade))

# Alterando idade para numeric
dados$idade <- as.numeric(dados$idade)
str(dados$idade); sum(is.na(dados$idade))

# Alterando demográficas para factor
# A função apply não produz factors, então precisamos usar a lapply
dados[c(1, 3:5, 7)] <- lapply(dados[c(1, 3:5, 7)], factor)

# Retirando missings
dados <- dados[complete.cases(dados),]
dim(dados)

# Variáveis MRP6 e MRP19 possuem erros de digitação (55)
# Corrigindo o valor para 5
dados[dados$MRP6 == 55 , "MRP6"] <- 5
dados[dados$MRP19 == 55 , "MRP19"] <- 5

View(dados)



# Vamos verificar novamente o banco
# 1-	Número de colunas e linhas
# 2-	Nome das variáveis
# 3-	Tipo das variáveis
str(dados)

# 4-	Missings
sum( ! complete.cases(dados)) # Todos os casos completos

# 5-	Valores não esperados
tabelas_freq <- apply(dados, 2, janitor::tabyl)
tabelas_freq <- lapply(tabelas_freq, adorn_totals)
tabelas_freq <- lapply(tabelas_freq, adorn_pct_formatting)


########################
#
# Criando novas variáveis
#
########################


# 1- Colapsando níveis de variáveis categóricas e numéricas

# Vamos criar a variável região geográfica
# install.packages("forcats")
library(forcats)

dados$regiao <- fct_collapse(dados$estado, 
                             "Norte" = c("Acre", "Amapá", "Amazonas", "Pará", 
                                         "Rondônia"),
                             "Nordeste" = c("Alagoas", "Bahia", "Ceará", 
                                            "Maranhão", "Paraíba", "Pernambuco", 
                                            "Piauí", "Rio Grande do Norte", "Sergipe"),
                             "Centro-Oeste" = c("Distrito Federal", "Goiás", 
                                                "Mato Grosso", "Mato Grosso do Sul"),
                             "Sudeste" = c("Espírito Santo", "Minas Gerais", 
                                           "Rio de Janeiro", "São Paulo"),
                             "Sul" = c("Paraná", "Rio Grande do Sul", "Santa Catarina")
)

# Determinando se o participante mora sozinho
dados$mora_sozinho <- ifelse(test = dados$pessoas_casa == 1, # Condição a ser testada
                             yes = "Sim", # Novo valor, se a condição for verdadeira
                             no = "Não") # Novo valor, se a condição for falsa

dados$mora_sozinho <- factor(dados$mora_sozinho)

# Criando faixas etárias
dados$fx_etaria <- ifelse(test = dados$idade <= 34, yes = "18 a 34 anos", 
                          no = ifelse(
                            test = dados$idade <= 49, yes = "35 a 49 anos", 
                            no = ifelse(
                              test = dados$idade <= 60, yes = "50 a 59 anos", 
                              no = "Acima de 60 anos"
                            )
                          )
)

dados$fx_etaria <- factor(dados$fx_etaria, ordered = TRUE)

str(dados)

# 2- Computando os escores das escalas

# MANEIRA MAIS SIMPLES:

# Se a escala não possuir itens invertidos, podemos usar as funções
# rowSums() ou rowMeans() para computar o escore de cada fator da escala.
# Por exemplo, para o fator de socialização:

social <- c("MRP2", "MRP7", "MRP12", "MRP17", "MRP22")
escore_socializacao <- rowMeans(dados[social])

# MANEIRA MAIS COMPLETA: PACOTE PSYCH

# O pacote psych é vantajoso, pois também calcula a fidedignidade e realiza a AFE,
# Além disso, ele pode inverter os itens automaticamente e calcular os escores 
# de todos os fatores de uma só vez.

# Primeiro, é necessário criar uma lista
# install.packages("psych")
library(psych)

chave_mrp <- list(extroversao= c("MRP1", "-MRP6", "-MRP11", "MRP16", "MRP21"), # itens negativos
                  socializacao= c("MRP2", "MRP7", "MRP12", "MRP17", "MRP22"),
                  conscienciosidade= c("MRP3", "MRP8", "MRP13", "MRP18", "MRP23"),
                  neuroticismo= c("MRP4", "MRP9", "MRP14", "MRP19", "MRP24"),
                  abertura= c("MRP5", "MRP10", "MRP15", "MRP20", "MRP25")
)

escores_mrp <- scoreItems(keys = chave_mrp, items = dados[8:32], min = 1, max = 5)

dados <- cbind(dados, escores_mrp$scores)

describe(dados[36:40])

