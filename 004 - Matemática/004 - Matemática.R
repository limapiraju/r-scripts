# 004 - Matemática

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/004 - Matemática/"
setwd(path)

# 7.1 - Mathematical functions

# exponential and logarithmic functions
x <- seq(0, 10, 0.1)
windows(7,4)
par(mfrow = c(1,2))
y <- exp(x)
plot(y ~ x, type = "l", main = "Exponencial")
y <- log(x)
plot(y ~ x, type = "l", main = "Logaritmica")

# cosine, sine, and tangent
windows(7,7)
par(mfrow = c(2,2))
x <- seq(0,2 * pi, 2 *pi / 100)
y1 <- cos(x)
y2 <- sin(x)
y3 <- tan(x)
plot(y1 ~ x,type = "l", main = "Cosseno")
plot(y2 ~ x,type = "l", main = "Seno")
plot(y3 ~ x,type = "l", ylim = c(-3, 3), main = "Tangente")

# power functions
windows(7,7)
par(mfrow = c(2,2))
x <- seq(0,1, 0.01)
y <- x ** 0.5
plot(x, y, type = "l", main = "0 < b < 1")
y <- x
plot(x, y, type = "l", main = "b = 1")
y <- x ** 2
plot(x, y, type = "l", main = "b > 1")
y <- 1 / x
plot(x, y, type = "l", main = "b < 0")

# polynomials
x <- seq(0, 10, 0.1)
y1 <- 2 + 5 * x - 0.2 * x ** 2
y2 <- 2 + 5 * x - 0.4 * x ** 2
y3 <- 2 + 4 * x - 0.6 * x ** 2 + 0.04 * x ** 3
y4 <- 2 + 4 * x + 2 * x ** 2 - 0.6 * x ** 3 + 0.04 * x ** 4
par(mfrow = c(2, 2))
plot(x, y1, type = "l", ylab = "y", main = "Decelerating")
plot(x, y2, type = "l", ylab = "y", main = "Humped")
plot(x, y3, type = "l", ylab = "y", main = "Inflection")
plot(x, y4, type = "l", ylab = "y", main = "Local Maximum")

par(mfrow = c(2, 2))
y1 <- x / (2 + 5 * x)
y2 <- 1 / (x - 2 + 4 / x)
y3 <- 1 / ( x ** 2 - 2 + 4 / x)
plot(x, y1, type = "l", ylab = "y", main = "Michaelis-Menten")
plot(x, y2, type = "l", ylab = "y", main = "shallow Hump")
plot(x, y3, type = "l", ylab = "y", main = "steep Hump")

# gamma function
par(mfrow = c(1, 1))
t <- seq(0.2, 4, 0.01)
plot(t,gamma(t),type = "l")
abline(h = 1, lty = 2)

# 7.2 - Probability functions

# factorial
par(mfrow = c(1, 1))
x <- 0:6
plot(x, factorial(x), type = "s", main = "Factorial x", log = "y")

# binomial coefficients

choose(8, 3)
plot(0:8, choose(8, 0:8), type = "s", main = "Binomial Coefficients")


# 7.3 - Continuous probability distributions

# probability density function (which has a d prefix)
# cumulative probability (p)
# quantiles of the distribution (q)
# random numbers generated from the distribution (r)

curve(pnorm(x), -3, 3)
arrows(-1, 0, -1, pnorm(-1), col = "red")
arrows(-1, pnorm(-1), -3, pnorm(-1), col = "green")
pnorm(-1)

curve(dnorm(x), -3, 3)

par(mfrow = c(2, 2))
x <- seq(-3, 3, 0.01)
y <- exp(-abs(x))
plot(x, y, type = "l", main = "x")
y <- exp(-abs(x) ** 2)
plot(x, y, type = "l", main = "x ** 2")
y <- exp(-abs(x) ** 3)
plot(x, y, type = "l", main = "x ** 3")
y <- exp(-abs(x) ** 8)
plot(x, y, type = "l", main = "x ** 8")

pnorm(-1.25) # AUC below a given z-score
1 - pnorm(1.875) # AUC above a given z-score
pnorm(1.25) - pnorm(-0.625) # AUC between given two z-scores


par(mfrow = c(2, 2))

x <- seq(-3, 3, 0.01)
z <- seq(-3, -1.25, 0.01)
p <- dnorm(z)
z <- c(z, -1.25, -3)
p <- c(p, min(p), min(p))
plot(x, dnorm(x), type = "l", xaxt = "n", ylab = "Probability Density", xlab = "Height")
axis(1, at = -3:3, labels = c("146","154","162","170","178","186","192"))
polygon(z, p, col = "red")

z <- seq(1.875, 3, 0.01)
p <- dnorm(z)
z <- c(z, 3, 1.875)
p <- c(p, min(p), min(p))
plot(x, dnorm(x), type = "l", xaxt = "n", ylab = "Probability Density", xlab = "Height")
axis(1, at = -3:3, labels = c("146","154","162","170","178","186","192"))
polygon(z, p, col = "red")

z <- seq(-0.635, 1.25, 0.01)
p <- dnorm(z)
z <- c(z, 1.25, -0.635)
p <- c(p, 0, 0)
plot(x, dnorm(x), type = "l", xaxt = "n", ylab = "Probability Density", xlab = "Height")
axis(1, at = -3:3, labels = c("146","154","162","170","178","186","192"))
polygon(z, p, col = "red")

z <- seq(1.96, 3, 0.01)
w <- seq(-1.96, -3, -0.01)
p <- dnorm(z)
z <- c(z, 3, 1.96)
w <- c(w, -3, -1.96)
p <- c(p, min(p), min(p))
plot(x, dnorm(x), type = "l", xaxt = "n", ylab = "Probability Density", xlab = "Height")
axis(1, at = -3:3, labels = c("146","154","162","170","178","186","192"))
polygon(z, p, col = "red")
polygon(w, p, col = "red")


# central limit theorem
par(mfrow = c(1, 1))
hist(runif(10000) * 10, main = "")

means <- numeric(10000)
for (i in 1:10000){
  means[i] <- mean(runif(5) * 10)
}
hist(means, ylim = c(0, 1600), xlab = "Sample Means", main = "")

mean(means)
sd(means)

xv <- seq(0, 10, 0.1)
yv <- dnorm(xv, mean = 5.012042, sd = 1.293902) * 5000
lines(xv, yv)

par(mfrow = c(2, 2))
hist(sample(1:6, replace = TRUE, 10000), breaks = 0.5:6.5, main = "", xlab = "One Die")

a <- sample(1:6, replace = TRUE, 10000)
b <- sample(1:6, replace = TRUE, 10000)
hist(a + b, breaks = 1.5:12.5, main = "", xlab = "Two Dice")

c <- sample(1:6, replace = TRUE, 10000)
hist(a + b + c, breaks = 2.5:18.5, main = "", xlab = "Three Dice")

d <- sample(1:6, replace = TRUE, 10000)
e <- sample(1:6, replace = TRUE, 10000)
hist(a + b + c + d + e, breaks = 4.5:30.5, main = "", xlab = "Five Dice")


par(mfrow = c(2, 2))
curve(dnorm, -3, 3, xlab = "z", ylab = "Probability Density", main = "Density")
curve(pnorm, -3, 3, xlab = "z", ylab = "Probability", main = "Probability")
curve(qnorm, 0, 1, xlab = "p", ylab = "Quantile (z)", main = "Quantiles")
y <- rnorm(1000)
hist(y, xlab = "z", ylab = "Frequency", breaks = -3.5:3.5, main = "Random numbers")

# random number generation
yvals <- rnorm(100, 24, 4)
mean(yvals)
sd(yvals)

ydevs <- rnorm(100, 0, 1)
ydevs <- (ydevs - mean(ydevs)) / sd(ydevs)
mean(ydevs)
sd(ydevs)

yvals <- 24 + ydevs * 4
mean(yvals)
sd(yvals)

1 - pchisq(14.3, 9) # observed X-squared = 14.3, df = 9, returns p-value
qchisq(0.95, 9) # alpha set at .05, df = 9, returns critical X-squared
1 - pf(2.85,8,12) # observed F-statistic, df1, df2, returns p-value
qt(0.975, 10) # two-tailed alpha set at .05, df = 10, returns critical t-statistic

# chi-square distribution
windows(7, 4)
par(mfrow = c(1, 2))
x <- seq(0, 30, .25)
plot(x, pchisq(x, 3, 7.25),type = "l", ylab = "p(x)", xlab = "x")
plot(x, pchisq(x, 5, 10), type = "l", ylab = "p(x)", xlab = "x")

8 * 10.2 / qchisq(.975, 8) # confidence interval on sample variances
8 * 10.2 / qchisq(.025, 8) # confidence interval on sample variances

# Fisher's F distribution
qf(.95, 2, 18) # one-tailed test, df1, df2, return F-statistic
x <- seq(0.05, 4, 0.05)

windows(7, 4)
par(mfrow = c(1, 2))
plot(x, df(x, 2, 18), type = "l", ylab = "f(x)", xlab = "x")
plot(x, df(x, 6, 18), type = "l", ylab = "f(x)", xlab = "x")


windows(7, 7)
par(mfrow = c(1, 1))
df <- seq(1, 30, .1)
plot(df, qf(.95, df, 30), type = "l", ylab = "Critical F")
lines(df, qf(.95, df, 10), lty = 2)


x <- seq(0.01, 3, 0.01)
plot(x, df(x, 1, 10), type = "l", ylim = c(0,1), ylab = "f(x)")
lines(x, df(x, 2, 10), lty = 6, col = "red")
lines(x, df(x, 5, 10), lty = 2, col = "green")
lines(x, df(x, 30, 10), lty = 3, col = "blue")
legend(2, 0.9, c("1","2","5","30"), col = (1:4), lty = c(1,6,2,3), title = "Numerator d.f.")

# Student's t distribution
curve((1 + x ** 2) ** (-0.5), -3, 3, ylab = "t(x)", col = "red")

plot(1:30, qt(0.975, 1:30), ylim = c(0, 12), type = "l",
     ylab = "Student's t value", xlab = "d.f.", col = "red")
abline(h = 2, lty = 2, col = "green")

xvs <- seq(-4, 4, 0.01)
plot(xvs, dnorm(xvs), type = "l", lty = 2,
     ylab = "Probability Density", xlab = "Deviates")
lines(xvs, dt(xvs, df = 5), col = "red")

# the gamma distribution
x <- seq(0.01, 4, .01)
par(mfrow = c(2,2))
y <- dgamma(x, .5, .5)
plot(x, y, type = "l", col = "red", main = "Alpha = 0.5")
y <- dgamma(x, .8, .8)
plot(x, y, type = "l", col = "red", main = "Alpha = 0.8")
y <- dgamma(x, 2, 2)
plot(x, y, type = "l", col = "red", main = "Alpha = 2")
y <- dgamma(x, 10, 10)
plot(x, y, type = "l", col = "red", main = "Alpha = 10")


qgamma(0.95, 2/3, 4/3) # 95% quantile from a gamma distribution with mean 2, and variance 3


rexp(15, 0.1)


par(mfrow = c(2, 2))
x <- seq(0,1, 0.01)
fx <- dbeta(x, 2, 3)
plot(x, fx, type = "l", main = "a = 2, b = 3", col = "red")
fx <- dbeta(x, 0.5, 2)
plot(x, fx, type = "l", main = "a = 0.5, b = 2", col = "red")
fx <- dbeta(x, 2, 0.5)
plot(x, fx, type = "l", main = "a = 2, b = 0.5", col = "red")
fx <- dbeta(x, 0.5, 0.5)
plot(x, fx, type = "l", main = "a = 0.5, b = 0.5", col = "red")


rbeta(10, 2, 3) # beta distributions, parameters 2 and 3

# Cauchy distribution: a long-tailed two-parameter distribution,
# characterized by a location parameter a and a scale parameter b

windows(7, 4)
par(mfrow = c(1, 2))
plot(-200:200, dcauchy(-200:200, 0, 10), type = "l", ylab = "p(x)", xlab = "x",
     col = "red")
plot(-200:200, dcauchy(-200:200, 0, 50), type = "l", ylab = "p(x)", xlab = "x",
     col = "red")


# the lognormal distribution
windows(7, 7)
plot(seq(0, 10, 0.05), dlnorm(seq(0, 10, 0.05)),
     type = "l", xlab = "x", ylab = "LogNormal f(x)", col = "red")


# the logistic distribution
windows(7, 4)
par(mfrow = c(1, 2))
plot(seq(-5, 5, 0.02), dlogis(seq(-5, 5, .02)),
     type = "l", main = "Logistic", col = "red", xlab = "x", ylab = "p(x)")
plot(seq(-5, 5, 0.02), dnorm(seq(-5, 5, .02)),
     type = "l", main = "Normal", col = "red", xlab = "x", ylab = "p(x)")


# the log-logistic distribution
# it's a very flexible four-parameter model for describing growth or decay processes
windows(7, 4)
par(mfrow = c(1, 2))
x <- seq(0.1, 1, 0.01)
y <- -1.4 + 2.1 * (exp(-1.59 * log(x) - 1.53) / (1 + exp(-1.59 * log(x) - 1.53)))
plot(log(x), y, type = "l", main = "c = -1.59", col = "red")
y <- 0.1 + 2.1 * (exp(1.59 * log(x) - 1.53) / (1 + exp(1.59 * log(x) - 1.53)))
plot(log(x), y, type = "l", main = "c = 1.59", col = "red")

# the Weibull distribution
# it's a two-paramter model that has the exponential distribution as a special case
windows(7, 7)
a <- 3
l <- 1
t <- seq(0, 1.8, .05)
ft <- a * l * t ** (a - 1) * exp(-l * t ** a)
plot(t, ft, type = "l", col = "blue", ylab = "f(t) ")
a <- 1
ft <- a * l * t ** (a - 1) * exp(-l * t ** a)
lines(t, ft, type = "l", col = "red")
a <- 2
ft <- a * l * t ** (a - 1) * exp(-l * t ** a)
lines(t, ft, type = "l", col = "green")
legend(1.4, 1.1, c("1","2","3"), title = "Alpha", lty = c(1,1,1), col = c(2,3,4))


# multivariate normal distribution
library(MASS)
xy <- mvrnorm(1000, mu = c(50, 60), matrix(c(4, 3.7, 3.7, 9), 2))
var(xy)

x <- xy[ ,1]
y <- xy[ ,2]
plot(x, y, pch = 16, ylab = "y", xlab = "x", col = "blue")


# If the two samples were independent, then the variance of the sum of the
# two variables would be equal to the sum of the two variances
var(x + y)
var(x) + var(y)


# the uniform distribution
x <- ceiling(runif(10000) * 6)
table(x)
hist(x, breaks = 0.5:6.5, main = "")

x <- runif(1000000)
y <- runif(1000000)
plot(x, y, pch = ".", col = "blue")


# 7.4 - Discrete probability distributions

# The Bernoulli distribution

# The binomial distribution

factorial(60) / (factorial(6) * factorial(60 - 6))
choose(60, 6)

p <- 0.1
n <- 4
x <- 0:n
px <- choose(n, x) * p ** x * (1 - p) ** (n - x)
barplot(px, names = x, xlab = "Outcome", ylab = "Probability", col = "green")

barplot(pbinom(0:4, 4, 0.1),names = 0:4, xlab = "Parasitized Fish",
        ylab = "Probability",col = "red")

qbinom(.025, 4, 0.1) # LL CI 95%
qbinom(.975, 4, 0.1) # LL CI 95%

rbinom(10, 4, 0.1)


# the geometric distribution
fx <- dgeom(0:20, 0.2)
barplot(fx, names = 0:20, xlab = "Outcome", ylab = "Probability", col = "cyan")
table(rgeom(100, 0.1))

# the hypergeometric distribution
ph <- dhyper(0:5, 6, 14, 5) # probability of 0 to 5 balls blue when 5 balls
# are sampled without replacement from 6 blue and 16 red balls
barplot(ph, names = (0:5), col = "red", xlab= "Outcome", ylab = "Probability")
rhyper(20, 6, 14, 5)


# the multinomial distribution
multi <- function(a, b, c) {
  factorial(a + b + c) / (factorial(a) * factorial(b) * factorial(c)) * 0.5 ** a * 0.25 ** b * 0.25 ** c}

barplot(sapply(0:20, function(i) multi(i, 20 - i, 4)), names = 0:20, cex.names = 0.7,
        xlab = "Outcome", ylab = "Probability", col = "yellow")


# the Poisson distribution
# it's a one-parameter distribution with its variance is equal to its mean

count <- rpois(600, 0.9)
table(count)
hist(count, breaks = -0.5:6.5, main = "")

# the negative binomial distribution

negbin <- function(x, u, k)
  (1 + u / k) ** (-k) * (u / (u + k)) ** x * gamma(k + x) / (factorial(x) * gamma(k))
xf <- sapply(0:10, function(i) negbin(i, 0.8, 0.2))
barplot(xf, names = 0:10, xlab = "Count", ylab = "Probability Density", col = "green")
plot(5:100, dnbinom(5:100, 5, 0.1), type = "s", xlab = "x", ylab = "f(x)")

x <- 0:12
freq <- c(131, 55, 21, 14, 6, 6, 2, 0, 0, 0, 0, 2, 1)
barplot(freq, names = x, ylab = "Frequency", xlab = "Spores", col = "purple")



# 7.5 - Matrix algebra

a <- matrix(c(1, 0, 4, 2, -1, 1), nrow = 3) # column-wise
b <- matrix(c(1, -1, 2, 1, 1, 0), nrow = 2) # column-wise

a %*% b
b %*% a

(ym <- diag(1, 3, 3))

A <- matrix(c(1, 2, 4, 2, 1, 1, 3, 1, 2), nrow = 3)
det(A)

library(MASS)
ginv(A)
