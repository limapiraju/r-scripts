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
