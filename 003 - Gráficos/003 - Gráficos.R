# 003 - Gráficos

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/003 - Gráficos/"
setwd(path)

# graficos com duas variaveis

# plot(x,y) scatterplot of y against x;
# plot(factor, y) box-and-whisker plot of y at each factor level;
# barplot(y) heights from a vector of y values (one bar per factor level).

# 5.2 - Plotting with two continuous explanatory variables: Scatterplots

data1 <- read.table("scatter1.txt", header = TRUE)
attach(data1)
names(data1)

plot(data1$xv, data1$yv, col = "red", xlab = "Explanatory Variable", ylab = "Response Variable")
abline(lm(data1$yv ~ data1$xv))

data2 <- read.table("scatter2.txt", header = TRUE)
names(data2)
points(data2$xv2, data2$yv2, col = "blue", pch = 16)
abline(lm(data2$yv2 ~ data2$xv2))

plot(c(data1$xv,data2$xv2),c(data1$yv, data2$yv2),xlab = "Explanatory Variable",
     ylab = "Response Variable", type = "n")
points(data1$xv,data1$yv, col = "red")
points(data2$xv2, data2$yv2,col = "blue", pch = 16)

range(c(data1$xv, data2$xv2))
range(c(data1$yv, data2$yv2))



plot(c(data1$xv,data2$xv2),c(data1$yv, data2$yv2),xlim = c(0,100), ylim = c(0,70),
     xlab = "Explanatory Variable",
     ylab = "Response Variable", type = "n")
points(data1$xv,data1$yv, col = "red")
points(data2$xv2, data2$yv2,col = "blue", pch = 16)
legend(locator(1),c("treatment","control"),pch = c(1,16), col = c("red","blue"))


plot(0:10,0:10,xlim=c(0,32),ylim=c(0,40),type="n",xaxt="n",yaxt="n",
     xlab="",ylab="")
x <- seq(1,31,2)
s <- -16
f <- -1
for (y in seq(2,40,2.5)) {
  s <- s+16
  f <- f+16
  y2 <- rep(y,16)
  points(x,y2,pch=s:f,cex=0.7)
  text(x,y-1,as.character(s:f),cex=0.6) }

plot(0:9,0:9,pch=16,type="n",xaxt="n",yaxt="n",ylab="col",xlab="bg")
axis(1,at=1:8)
axis(2,at=1:8)
for (i in 1:8) points(1:8,rep(i,8),pch=c(21,22,24),bg=1:8,col=i)


map.places <- read.csv("map.places.csv", header = TRUE)
map.data <- read.csv("bowens.csv", header = TRUE)
nn <- ifelse(map.data$north < 60, map.data$north + 100, map.data$north)
windows(9,7)
plot(c(20, 100), c(60,110), type = "n", xlab= "", ylab = "", xaxt = "n", yaxt = "n")

for (i in 1:length(map.places$wanted)){
  ii <- which(map.data$place == as.character(map.places$wanted[i]))
  text(map.data$east[ii], nn[ii], as.character(map.data$place[ii]), cex = 0.6) }


data <- read.table("sleep.txt", header = TRUE)
plot(data$Days, data$Reaction)
s <- as.numeric(factor(data$Subject))
plot(data$Days,data$Reaction, type = "n")
for (k in 1:max(s)){
  x <- data$Days[s == k]
  y <- data$Reaction[s == k]
  lines(x, y, type = "b", col = "gray")
}

sym <- rep(c(21,22,24),c(7,7,4))
bcol <- c(2:8,2:8,2:5)

for (k in 1:max(s)){
  points(data$Days[s == k], data$Reaction[s == k], pch = sym[k], bg = bcol[k], col = 1)
}


smooth <- read.table("smooth.txt", header = TRUE)
plot(x, y, pch = 16)
sequence <- order(x)
lines(x[sequence], y[sequence])


x <- 0:10
y <- 0:10
plot(x,y)
lines(x, y, col = "red")
lines(x, y, col = "blue", type = "s")
lines(x, y, col = "green", type = "S")


# 5.3 - Adding other shapes to a plot

plot(0:10, 0:10, xlab = "", ylab = "", xaxt = "n", yaxt = "n", type = "n")

z <- seq(-3,3,0.01)
pd <- dnorm(z)
plot(z,pd,type="l", xlab = "z-score", ylab = "Density")
polygon(c(z[z <= -1.96], -1.96), c(pd[z <= -1.96], pd[z == -3]), col = "red")
polygon(c(z[z >= 1.96], 1.96), c(pd[z >= 1.96], pd[z == -3]), col = "red")


# 5.4 - Drawing mathematical functions

curve(x ** 3 - 3 * x, -2, 2)

x <- seq(-2, 2, 0.01)
y <- x ** 3 - 3 * x
plot(x, y, type = "l")

xv <- 0:100
yA <- 482 * xv * exp(-0.045 * xv)
yB <- 518 * xv * exp(-0.055 * xv)
plot(c(xv, xv),c(yA, yB), xlab = "stock", ylab = "recruits", type = "n")
lines(xv, yA,  lty = 2, col = "blue")
lines(xv, yB, lty = 1, col = "red")
info <- read.table("plotfit.txt", header = T)
points(info$x, info$y, pch = 16)

data <- read.table("jaws.txt",header = T)
attach(data)
names(data)
par(mfrow = c(2,2))
# non-parametric smoother, lowess
plot(age, bone, pch = 16, main = "lowess")
lines(lowess(age, bone), col = "red")
# modelling tool, loess (author recommendation)
plot(age, bone, pch = 16, main = "loess")
model <- loess(bone ~ age)
xv <- 0:50
yv <- predict(model, data.frame(age = xv))
lines(xv, yv, col = "red")
# generalized additive model, gam
library(mgcv)
plot(age, bone, pch = 16,main = "gam")
model <- gam(bone ~ s(age))
xv <- 0:50
yv <- predict(model, list(age = xv))
lines(xv, yv, col = "red")
# polinomial regression
plot(age, bone, pch = 16, main = "cubic polynomial")
model <- lm(bone ~ age + I(age ** 2) + I(age ** 3))
xv <- 0:50
yv <- predict(model,list(age = xv))
lines(xv, yv, col = "red")


# 5.5 - Shape and size of the graphics window

data <- read.table("pollute.txt", header = T)
attach(data)
windows(7, 4)
par(mfrow = c(1,2))
plot(Population, Pollution)
plot(Temp, Pollution)


# 5.6 - Plotting with a categorical explanatory variable

weather <- read.table("SilwoodWeather.txt",header = T)
attach(weather)
names(weather)
month <- factor(month)
plot(month,upper)
plot(month, upper, ylab = "Daily Maximum Temperature", xlab = "Month", notch = TRUE)

lima <- read.table("lima_et_al_exp1_bs.txt",header = TRUE) # exp. 1, between-subject format
attach(lima)
names(lima)
condition <- factor(condition)

means <- tapply(performance, condition, mean)
barplot(means, xlab = "Condition", ylab = "Proportion Recalled on Final Test", ylim = c(0, 1), col = "gray")

data <- read.table("silwoodweather.txt", header = TRUE)
attach(data)
month <- factor(month)
season <- heat.colors(12)
temp <- c(11, 10, 8, 5, 3, 1, 2, 3, 5, 8, 10, 11)
plot(month, upper, col = season[temp])


# 5.7 - Plots for single samples

data <- read.table("daphnia.txt", header = TRUE)
attach(data)
names(data)
par(mfrow=c(1, 2))
hist(Growth.rate, seq(0, 8, 0.5), col = "green", main = "")
y <- as.vector(tapply(Growth.rate, list(Daphnia, Detergent), mean))
barplot(y, col = "green", ylab = "Growth rate", xlab = "Treatment")

par(mfrow = c(2,2))
hist(Growth.rate, seq(0, 8, 0.25), col = "green", main = "")
hist(Growth.rate, seq(0, 8, 0.5), col = "green", main = "")
hist(Growth.rate, seq(0, 8, 2), col = "green", main = "")
hist(Growth.rate, c(0, 3, 4, 8), col = "green", main = "")

range(Growth.rate)
edges <- c(0, 3, 4, 8)
bin <- cut(Growth.rate,edges)

values <- rpois(1000, 1.70)
hist(values, breaks=(-0.5:8.5), main = "", xlab = "Random Numbers from a Poisson with Mean 1.7")

y <- rnbinom(158, mu = 1.5, size = 1)
bks <- -0.5:(max(y) + 0.5)
hist(y, bks, main = "")
a <- mean(y)
b <- var(y)
c <- mean(y) ** 2 / (var(y) - mean(y))

xs <- 0:11
ys <- dnbinom(xs, size = c, mu = a)
lines(xs, ys * 158)

data <- read.csv("piedata.csv")
pie(data$amounts,labels=as.character(data$names))


normal.plot <- function(y) {
  s <- sd(y)
  plot(c(0,3),c(min(0,mean(y)-s * 4*
                      qnorm(0.75)),max(y)),xaxt="n",xlab="",type="n",ylab="")
  # for your data's boxes and whiskers, centred at x = 1
  top <- quantile(y,0.75)
  bottom <- quantile(y,0.25)
  w1u <- quantile(y,0.91)
  w2u <- quantile(y,0.98)
  w1d <- quantile(y,0.09)
  w2d <- quantile(y,0.02)
  rect(0.8,bottom,1.2,top)
  lines(c(0.8,1.2),c(mean(y),mean(y)),lty=3)
  lines(c(0.8,1.2),c(median(y),median(y)))
  lines(c(1,1),c(top,w1u))
  lines(c(0.9,1.1),c(w1u,w1u))
  lines(c(1,1),c(w2u,w1u),lty=3)
  lines(c(0.9,1.1),c(w2u,w2u),lty=3)
  nou <- length(y[y>w2u])
  points(rep(1,nou),jitter(y[y>w2u]))
  lines(c(1,1),c(bottom,w1d))
  lines(c(0.9,1.1),c(w1d,w1d))
  lines(c(1,1),c(w2d,w1d),lty=3)
  lines(c(0.9,1.1),c(w2d,w2d),lty=3)
  nod <- length(y[y<w2d])
  points(rep(1,nod),jitter(y[y<w2d]))
  #for the normal box and whiskers, centred at x = 2
  n75 <- mean(y)+ s * qnorm(0.75)
  n25 <- mean(y)- s * qnorm(0.75)
  n91 <- mean(y)+ s * 2* qnorm(0.75)
  n98 <- mean(y)+ s * 3* qnorm(0.75)
  n9 <- mean(y)- s * 2* qnorm(0.75)
  n2 <- mean(y)- s * 3* qnorm(0.75)
  rect(1.8,n25,2.2,n75)
  lines(c(1.8,2.2),c(mean(y),mean(y)),lty=3)
  lines(c(2,2),c(n75,n91))
  lines(c(1.9,2.1),c(n91,n91))
  lines(c(2,2),c(n98,n91),lty=3)
  lines(c(1.9,2.1),c(n98,n98),lty=3)
  lines(c(2,2),c(n25,n9))
  lines(c(1.9,2.1),c(n9,n9))
  lines(c(2,2),c(n9,n2),lty=3)
  lines(c(1.9,2.1),c(n2,n2),lty=3)
  lines(c(1.2,1.8),c(top,n75),lty=3,col="gray")
  lines(c(1.1,1.9),c(w1u,n91),lty=3,col="gray")
  lines(c(1.1,1.9),c(w2u,n98),lty=3,col="gray")
  lines(c(1.2,1.8),c(bottom,n25),lty=3,col="gray")
  lines(c(1.1,1.9),c(w1d,n9),lty=3,col="gray")
  lines(c(1.1,1.9),c(w2d,n2),lty=3,col="gray")
  # label the two boxes
  axis(1,c(1,2),c("data","normal")) }

y <- rnbinom(100,1,0.2)
normal.plot(y)


# 5.8 - Plots with multiple variables

# pairs: for a matrix of scatterplots of every variable against every other;
# coplot: for conditioning plots where y is plotted against x for different values of z;
# xyplot: where a set of panel plots is produced.

ozonedata <- read.table("ozone.data.txt",header = TRUE)
attach(ozonedata)
names(ozonedata)
pairs(ozonedata, panel = panel.smooth)

coplot(ozone ~ wind|temp,panel = panel.smooth)

yields <- read.table("splityield.txt", header = TRUE)
attach(yields)
names(yields)
interaction.plot(fertilizer,irrigation,yield)


# 5.9 - Special plots

bubble.plot <- function(xv,yv,rv,bs=0.1){
  r <- rv/max(rv)
  yscale <- max(yv)-min(yv)
  xscale <- max(xv)-min(xv)
  plot(xv,yv,type="n", xlab=deparse(substitute(xv)),
       ylab=deparse(substitute(yv)))
  for (i in 1:length(xv)) bubble(xv[i],yv[i],r[i],bs,xscale,yscale) }
bubble <- function (x,y,r,bubble.size,xscale,yscale) {
  theta <- seq(0,2*pi,pi/200)
  yv <- r*sin(theta)*bubble.size*yscale
  xv <- r*cos(theta)* bubble.size*xscale
  lines(x+xv,y+yv) }

ddd <- read.table("pgr.txt", header = TRUE)
attach(ddd)
names(ddd)
bubble.plot(hay, pH, FR)


numbers <- read.table("longdata.txt", header = TRUE)
attach(numbers)
names(numbers)
plot(jitter(xlong,amount=1), jitter(ylong, amount = 1), xlab = "input", ylab = "count")
sunflowerplot(xlong,ylong)


# 5.10 - Saving graphics to file

data <- read.table("pollute.txt", header = TRUE)
attach(data)

pdf("pollution.pdf", width = 7, height = 4)
par(mfrow=c(1, 2))
plot(Population, Pollution)
plot(Temp, Pollution)
dev.off()
