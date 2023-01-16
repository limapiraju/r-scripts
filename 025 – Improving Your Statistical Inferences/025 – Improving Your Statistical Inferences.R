# Improving Your Statistical Inferences
# by Daniel Lakens

# Chapter 1 - Using p-values to test a hypothesis


# 1.4 Which p-values can you expect?

# case 1: there is no effect

p <- numeric(100000) # store all simulated *p*-values

for (i in 1:100000) { # for each simulated experiment
  x <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  y <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  p[i] <- t.test(x, y)$p.value # store the *p*-value
}

(sum(p < 0.05) / 100000) # compute power

hist(p, breaks = 20) # plot a histogram

# case 2: there is effect

p <- numeric(100000) # store all simulated *p*-values

for (i in 1:100000) { # for each simulated experiment
  x <- rnorm(n = 71, mean = 100, sd = 15) # Simulate data
  y <- rnorm(n = 71, mean = 105, sd = 15) # Simulate data
  p[i] <- t.test(x, y)$p.value # store the *p*-value
}

(sum(p < 0.05) / 100000) # compute power

hist(p, breaks = 20) # plot a histogram


# one sample t-test comparing means with mu = 100

nsims <- 100000 # number of simulations

m <- 106 # mean sample
n <- 26 # set sample size
sd <- 15 # SD of the simulated data

p <- numeric(nsims) # set up empty vector
bars <- 20

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = m, sd = sd)
  z <- t.test(x, mu = 100) # perform the t-test
  p[i] <- z$p.value # get the p-value
}
power <- round((sum(p < 0.05) / nsims), 2) # power

# Plot figure
hist(p,
     breaks = bars, xlab = "P-values", ylab = "number of p-values\n", 
     axes = FALSE, main = paste("P-value Distribution with", 
                                round(power * 100, digits = 1), "% Power"),
     col = "grey", xlim = c(0, 1), ylim = c(0, nsims))
axis(side = 1, at = seq(0, 1, 0.1), labels = seq(0, 1, 0.1))
axis(side = 2, at = seq(0, nsims, nsims / 4), 
     labels = seq(0, nsims, nsims / 4), las = 2)
abline(h = nsims / bars, col = "red", lty = 3)


# Chapter 2 - Error control

# 2.4 Optional stopping

# case 1: there is no effect

n <- 200 # total number of datapoints (per condition) after initial 10
d <- 0.0 # effect size d

p <- numeric(n) # store p-values
x <- numeric(n) # store x-values
y <- numeric(n) # store y-values

n <- n + 10 # add 10 to number of datapoints

for (i in 10:n) { # for each simulated participants after the first 10
  x[i] <- rnorm(n = 1, mean = 0, sd = 1)
  y[i] <- rnorm(n = 1, mean = d, sd = 1)
  p[i] <- t.test(x[1:i], y[1:i], var.equal = TRUE)$p.value
}

p <- p[10:n] # Remove first 10 empty p-values

# Create the plot
par(bg = "#fffafa")
plot(0, col = "red", lty = 1, lwd = 3, ylim = c(0, 1), xlim = c(10, n), 
     type = "l", xlab = "sample size", ylab = "p-value")
lines(p, lwd = 2)
abline(h = 0.05, col = "darkgrey", lty = 2, lwd = 2) # draw line at p = 0.05

min(p) # Return lowest p-value from all looks
cat("The lowest p-value was observed at sample size", which.min(p) + 10) 
cat("The p-value dropped below 0.05 for the first time at sample size:", 
    ifelse(is.na(which(p < 0.05)[1] + 10), "NEVER", which(p < 0.05)[1] + 10)) 


# case 2: there is an effect

n <- 200 # total number of datapoints (per condition) after initial 10
d <- 0.3 # effect size d

p <- numeric(n) # store p-values
x <- numeric(n) # store x-values
y <- numeric(n) # store y-values

n <- n + 10 # add 10 to number of datapoints

for (i in 10:n) { # for each simulated participants after the first 10
  x[i] <- rnorm(n = 1, mean = 0, sd = 1)
  y[i] <- rnorm(n = 1, mean = d, sd = 1)
  p[i] <- t.test(x[1:i], y[1:i], var.equal = TRUE)$p.value
}

p <- p[10:n] # Remove first 10 empty p-values

# Create the plot
par(bg = "#fffafa")
plot(0, col = "red", lty = 1, lwd = 3, ylim = c(0, 1), xlim = c(10, n), 
     type = "l", xlab = "sample size", ylab = "p-value")
lines(p, lwd = 2)
abline(h = 0.05, col = "darkgrey", lty = 2, lwd = 2) # draw line at p = 0.05

min(p) # Return lowest p-value from all looks
cat("The lowest p-value was observed at sample size", which.min(p) + 10) 
cat("The p-value dropped below 0.05 for the first time at sample size:", 
    ifelse(is.na(which(p < 0.05)[1] + 10), "NEVER", which(p < 0.05)[1] + 10)) 


# inflation of the Type I error rate using the optional stopping procedure

N <- 100 # total datapoints (per condition)
looks <- 5 # set number of looks at the data
nsims <- 50000 # number of simulated studies
alphalevel <- 0.05 # set alphalevel

if(looks > 1){
  look_at_n <- ceiling(seq(N / looks, N, (N - (N / looks)) / (looks - 1)))
}  else {
  look_at_n <- N
}
look_at_n <- look_at_n[look_at_n > 2] # Remove looks at N of 1 or 2
looks<-length(look_at_n) # if looks are removed, update number of looks

matp <- matrix(NA, nrow = nsims, ncol = looks) # Matrix for p-values l tests
p <- numeric(nsims) # Variable to save pvalues

# Loop data generation for each study, then loop to perform a test for each N
for (i in 1:nsims) {
  x <- rnorm(n = N, mean = 0, sd = 1)
  y <- rnorm(n = N, mean = 0, sd = 1)
  for (j in 1:looks) {
    matp[i, j] <- t.test(x[1:look_at_n[j]], y[1:look_at_n[j]], 
                         var.equal = TRUE)$p.value # perform the t-test, store
  }
  cat("Loop", i, "of", nsims, "\n")
}

# Save Type 1 error rate smallest p at all looks
for (i in 1:nsims) {
  p[i] <- ifelse(length(matp[i,which(matp[i,] < alphalevel)]) == 0, 
                 matp[i,looks], matp[i,which(matp[i,] < alphalevel)])
}

hist(p, breaks = 100, col = "grey") # create plot
abline(h = nsims / 100, col = "red", lty = 3)

cat("Type 1 error rates for look 1 to", looks, ":", 
    colSums(matp < alphalevel) / nsims)
cat("Type 1 error rate when only the lowest p-value for all looks is reported:", 
    sum(p < alphalevel) / nsims)


# Chapter 3 - Likelihoods

factorial(10)/(factorial(8)*(factorial(10-8))) * 0.5^8 * (1 - 0.5)^(10-8)
dbinom(x = 2, size = 10, prob = 0.5)

n <- 10 # set total trials
x <- 5 # set successes
H0 <- 0.5 # specify one hypothesis you want to compare
H1 <- 0.4 # specify another hypothesis you want to compare
dbinom(x, n, H0) / dbinom(x, n, H1) # Returns the H0/H1 likelihood ratio
dbinom(x, n, H1) / dbinom(x, n, H0) # Returns the H1/H0 likelihood ratio

theta <- seq(0, 1, len = 100) # create probability variable from 0 to 1
like <- dbinom(x, n, theta)

plot(theta, like, type = "l", xlab = "p", ylab = "Likelihood", lwd = 2)
points(H0, dbinom(x, n, H0))
points(H1, dbinom(x, n, H1))
segments(H0, dbinom(x, n, H0), x / n, dbinom(x, n, H0), lty = 2, lwd = 2)
segments(H1, dbinom(x, n, H1), x / n, dbinom(x, n, H1), lty = 2, lwd = 2)
segments(x / n, dbinom(x, n, H0), x / n, dbinom(x, n, H1), lwd = 2)
title(paste("Likelihood Ratio H0/H1:", round(dbinom(x, n, H0) / dbinom(x, n, H1), digits = 2), " Likelihood Ratio H1/H0:", round(dbinom(x, n, H1) / dbinom(x, n, H0), digits = 2)))

# Chapter 4 - Bayesian statistics

# computing the credible interval and the highest density interval
# install.packages("binom")
library(binom)

n <- 20 # set total trials
x <- 10 # set successes
aprior <- 1 # Set the alpha for the Beta distribution for the prior
bprior <- 1 # Set the beta for the Beta distribution for the prior

binom.bayes(x, n, type = "central", prior.shape1 = aprior, prior.shape2 = bprior)

binom.bayes(x, n, type = "highest", prior.shape1 = aprior, prior.shape2 = bprior)




# Chapter 7 - Confidence intervals
# install.packages("MOTE")
library(MOTE)
MOTE::d.ind.t(m1 = 1.7, m2 = 2.1,
              sd1 = 1.01, sd2 = 0.96,
              n1 = 77, n2 = 78,
              a = .05)$estimate

library(MBESS)
MBESS::smd(Mean.1 = 1.7, Mean.2 = 2.1,
           s.1 = 1.01, s.2 = 0.96,
           n.1 = 77, n.2 = 78)


set.seed(33)
x <- rnorm(n = 20, mean = 0, sd = 2.5) # create sample from normal distribution
y <- rnorm(n = 200, mean = 1.5, sd = 3.5) # create sample from normal distribution

library(effectsize)
effectsize::cohens_d(x, y)

# Hedges g*s, see Delacre et al., 2021
effectsize::cohens_d(x, y, pooled_sd = FALSE)



x <- rnorm(n = 50, mean = 3, sd = 5) # get sample group 1
y <- rnorm(n = 50, mean = 5, sd = 5) # get sample group 2

d <- data.frame(
  labels = c("X", "Y", "Difference"),
  mean = c(mean(x), mean(y), mean(y) - mean(x)),
  lower = c(t.test(x)[[4]][1], t.test(y)[[4]][1], t.test(y, x)[[4]][1]),
  upper = c(t.test(x)[[4]][2], t.test(y)[[4]][2], t.test(y, x)[[4]][2])
)

plot(NA, xlim = c(.5, 3.5), ylim = c(0, max(d$upper[1:2] + 1)), bty = "l", 
     xaxt = "n", xlab = "", ylab = "Mean")
points(d$mean[1:2], pch = 19)
segments(1, d$mean[1], 5, d$mean[1], lty = 2)
segments(2, d$mean[2], 5, d$mean[2], lty = 2)
axis(1, 1:3, d$labels)
segments(1:2, d$lower[1:2], 1:2, d$upper[1:2])
axis(4, seq((d$mean[1] - 3), (d$mean[1] + 5), by = 1), seq(-3, 5, by = 1))
points(3, d$mean[1] + d$mean[3], pch = 19, cex = 1.5)
segments(3, d$mean[1] + d$lower[3], 3, d$mean[1] + d$upper[3], lwd = 2)
mtext("Difference", side = 4, at = d$mean[1], line = 3)
segments(1:1, d$upper[1:1], 1:2, d$upper[1:1], lty = 3)
segments(1:1, d$lower[1:2], 1:2, d$lower[1:2], lty = 3)
text(3, 1, paste("P-value", round(t.test(x, y)$p.value, digits = 3)))





library(ggplot2)

n <- 20 # set sample size
nsims <- 100000 # set number of simulations

x <- rnorm(n = n, mean = 100, sd = 15) # create sample from normal distribution

# 95% Confidence Interval
ciu <- mean(x) + qt(0.975, df = n - 1) * sd(x) * sqrt(1 / n)
cil <- mean(x) - qt(0.975, df = n - 1) * sd(x) * sqrt(1 / n)

# 95% Prediction Interval
piu <- mean(x) + qt(0.975, df = n - 1) * sd(x) * sqrt(1 + 1 / n)
pil <- mean(x) - qt(0.975, df = n - 1) * sd(x) * sqrt(1 + 1 / n)

ggplot(as.data.frame(x), aes(x)) + # plot data
  geom_rect(aes(xmin = pil, xmax = piu, ymin = 0, ymax = Inf),
            fill = "gold") + # draw yellow PI area
  geom_rect(aes(xmin = cil, xmax = ciu, ymin = 0, ymax = Inf),
            fill = "#E69F00") + # draw orange CI area
  geom_histogram(colour = "black", fill = "grey", aes(y = ..density..), bins = 20) +
  xlab("Score") +
  ylab("frequency") +
  theme_bw(base_size = 20) +
  theme(panel.grid.major.x = element_blank(), axis.text.y = element_blank(),
        panel.grid.minor.x = element_blank()) + 
  geom_vline(xintercept = mean(x), linetype = "dashed", size = 1) +
  coord_cartesian(xlim = c(50, 150)) +
  scale_x_continuous(breaks = c(seq(50, 150, 10))) +
  annotate("text", x = mean(x), y = 0.02, label = paste(
    "Mean = ", round(mean(x)), "\n",
    "SD = ", round(sd(x)), sep = ""), size = 6.5)

# Simulate Confidence Intervals
ciu_sim <- numeric(nsims)
cil_sim <- numeric(nsims)
mean_sim <- numeric(nsims)

for (i in 1:nsims) { # for each simulated experiment
  x <- rnorm(n = n, mean = 100, sd = 15) # create sample from normal distribution
  ciu_sim[i] <- mean(x) + qt(0.975, df = n - 1) * sd(x) * sqrt(1 / n)
  cil_sim[i] <- mean(x) - qt(0.975, df = n - 1) * sd(x) * sqrt(1 / n)
  mean_sim[i] <- mean(x) # store means of each sample
}

# Save only those simulations where the true value was inside the 95% CI
ciu_sim <- ciu_sim[ciu_sim < 100]
cil_sim <- cil_sim[cil_sim > 100]

# Calculate how many times the observed mean fell within the 95% CI of the original study
mean_sim <- mean_sim[mean_sim > cil & mean_sim < ciu]

cat((100 * (1 - (length(ciu_sim) / nsims + length(cil_sim) / nsims))),
    "% of the 95% confidence intervals contained the true mean")
cat("The capture percentage for the plotted study, or the % of values within
    the observed confidence interval from", cil, "to", ciu,
    "is:", 100 * length(mean_sim) / nsims, "%")



# install.packages("pwr")
library(pwr)

alpha_level <- 0.05 # set alpha level
n <- 100 # set number of observations
st_dev <- 1 # set true standard deviation
effect <- 0.5 # set effect size (raw mean difference)

# calculate lower and upper critical values c_l and c_u
c_l <- sqrt((n - 1)/qchisq(alpha_level/2, n - 1, lower.tail = FALSE))
c_u <- sqrt((n - 1)/qchisq(alpha_level/2, n - 1, lower.tail = TRUE))

# calculate lower and upper confidence interval for sd
st_dev * c_l
st_dev * c_u

# d based on lower bound of the 95CI around the SD
effect/(st_dev * c_l)
# d based on upper bound of the 95CI around the SD
effect/(st_dev * c_u)

pwr::pwr.t.test(d = effect/(st_dev * c_l), power = 0.9, sig.level = 0.05)
pwr::pwr.t.test(d = effect/(st_dev * c_u), power = 0.9, sig.level = 0.05)

# Power analysis for true standard deviation for comparison
pwr::pwr.t.test(d = effect/st_dev, power = 0.9, sig.level = 0.05)








# Chapter 8 - Sample size justification

n_sims <- 1000000
p <- numeric(10000)   # to store p-values
for (i in 1:n_sims) {  # simulate n_sims tests
  x <- rnorm(n = 20, mean = 0.5, sd = 1)
  p[i] <- t.test(x)$p.value # store p-value
}
sum(p < 0.05) / n_sims # Compute power


# Chapter 9 - Equivalence testing and interval hypothesis
# install.packages("TOSTER")
library(TOSTER)
TOSTER::tsum_TOST(m1 = 4.55, 
                  m2 = 4.87, 
                  sd1 = 1.05, 
                  sd2 = 1.11,
                  n1 = 15, 
                  n2 = 15, 
                  low_eqbound = -0.5, 
                  high_eqbound = 0.5)


# true effect = 0, equivalence test with lower and upper bounds
# equal -.5 and .5, respectively
TOSTER::power_t_TOST(power = 0.9, delta = 0,
                     alpha = 0.05, type = "two.sample",
                     low_eqbound = -0.5, high_eqbound = 0.5)

# true effect = .1, equivalence test with lower and upper bounds
# equal -.5 and .5, respectively
TOSTER::power_t_TOST(power = 0.9, delta = 0.1,
                     alpha = 0.05, type = "two.sample",
                     low_eqbound = -0.5, high_eqbound = 0.5)



# New TOSTER power functions allows power for expected non-zero effect.
TOSTER::power_t_TOST(power = 0.9, delta = 0,
                     alpha = 0.05, type = "two.sample",
                     low_eqbound = -5, high_eqbound = 0.5)



library("pwr")

pwr::pwr.t.test(
  n = 20, 
  sig.level = 0.05, 
  power = 0.33, 
  type = "one.sample",
  alternative = "two.sided"
)


pwr::pwr.t.test(
  n = 30, 
  sig.level = 0.05, 
  power = 1/3, 
  type = "two.sample",
  alternative = "two.sided"
)











# Chapter 10 - Sequential Analysis
# install.packages("rpact")
library(rpact)

design <- getDesignGroupSequential(
  kMax = 3,
  typeOfDesign = "P",
  sided = 2,
  alpha = 0.05,
  beta = 0.2
  )

design # one-sided alpha level

# In clinical trials, researchers mostly test directional predictions,
# and thus, the default setting is to perform a one-sided test
# for two-sided alpha level, run the following code
design$stageLevels * 2



# 10.5 Updating boundaries during a study
design <- getDesignGroupSequential(kMax = 3, 
                                   typeOfDesign = "asP",
                                   sided = 2, 
                                   alpha = 0.05, 
                                   beta = 0.1)
design$stageLevels * 2


# information rates
# when would we look at the data?
design <- getDesignGroupSequential(
  typeOfDesign = "asP", 
  informationRates = c(76/198, 2/3, 1), 
  alpha = 0.05, 
  sided = 2)

esign$stageLevels * 2

design$alphaSpent




design <- getDesignGroupSequential(
  typeOfDesign = "asUser", 
  informationRates = c(72/206, 132/206, 1), 
  alpha = 0.05, 
  sided = 2, 
  userAlphaSpending = c(0.0253, 0.0382, 0.05)
)
design$stageLevels * 2


# sample size for sequential designs
design <- getDesignGroupSequential(
  kMax = 1,
  typeOfDesign = "P",
  sided = 2,
  alpha = 0.05,
  beta = 0.2
)

power_res <- getSampleSizeMeans(
  design = design,
  groups = 2,
  alternative = 0.5, 
  stDev = 1, 
  allocationRatioPlanned = 1,
  normalApproximation = FALSE)

power_res


# increasing final N to maintaing achieved power
seq_design <- getDesignGroupSequential(
  kMax = 2,
  typeOfDesign = "asP",
  sided = 2,
  alpha = 0.05,
  beta = 0.1
)

# Compute the sample size we need
power_res_seq <- getSampleSizeMeans(
  design = seq_design,
  groups = 2,
  alternative = 0.5, 
  stDev = 1, 
  allocationRatioPlanned = 1,
  normalApproximation = FALSE)

power_res_seq

# assuming there is a true effect of d = 0.5, the expected sample size
# on average is the probability of stopping at each look, multiplied by
# the number of observations we collect at each look,
# so 0.6 * 96 + 0.3 * 190 = 133.39.



# Use getPowerMeans and set max N to 188 based on analysis above
sample_res <- getPowerMeans(
  design = seq_design,
  groups = 2,
  alternative = seq(0, 1, 0.01), 
  stDev = 1, 
  allocationRatioPlanned = 1,
  maxNumberOfSubjects = 190, 
  normalApproximation = FALSE)

plot(sample_res, type = 6)





seq_design <- getDesignGroupSequential(
  kMax = 2,
  typeOfDesign = "asOF",
  sided = 2,
  alpha = 0.05,
  beta = 0.1
)

# Compute the sample size we need
power_res_seq <- getSampleSizeMeans(
  design = seq_design,
  groups = 2,
  alternative = 0.5, 
  stDev = 1, 
  allocationRatioPlanned = 1,
  normalApproximation = FALSE)

summary(power_res_seq)


# stopping for futility
design <- getDesignGroupSequential(
  sided = 1,
  alpha = 0.05,
  beta = 0.1,
  typeOfDesign = "asP",
  futilityBounds = c(0, 0),
  bindingFutility = FALSE
)










# Chapter 11 - Meta-analysis

library(ggplot2)
#set.seed(3190) # set seed for reproducibility
n <- 100000 # set sample size
x <- rnorm(n = n, mean = 100, sd = 15) # simulate data

# plot data adding normal distribution and annotations
ggplot(as.data.frame(x), aes(x)) +
  geom_histogram(colour = "black", fill = "grey", aes(y = ..density..), binwidth = 2) +
  stat_function(fun = dnorm, args = c(mean = 100, sd = 15), size = 1, color = "red", lty = 2) +
  xlab("IQ") +
  ylab("number of people") +
  theme_bw(base_size = 20) +
  geom_vline(xintercept = mean(x), colour = "gray20", linetype = "dashed") +
  coord_cartesian(xlim = c(50, 150)) +
  scale_x_continuous(breaks = seq(50, 150, 10)) +
  annotate("text", x = mean(x), y = 0.02, label = paste("Mean = ", round(mean(x)), "\n", "SD = ", round(sd(x)), sep = ""), size = 8) + 
  theme(plot.background = element_rect(fill = "#fffafa")) + 
  theme(panel.background = element_rect(fill = "#fffafa"))


# escalc: can be used to calculate effect sizes, their variances,
# and confidence intervals around effect size estimates
library(metafor)
g <- escalc(measure = "SMD",
            n1i = 50, # sample size in Group 1
            m1i = 5.6, # observed mean in Group 1
            sd1i = 1.2, # observed standard deviation in Group 1
            n2i = 50, # sample size in Group 2
            m2i = 4.9, # observed mean in Group 2
            sd2i = 1.3) # observed standard deviation in Group 2
g

# run a single-study meta-analysis
meta_res <- rma(yi, vi, data = g)
meta_res

# simulating data for a meta-analysis, fixed-effect model
set.seed(94)
nSims <- 12 # number of simulated studies
m1 <- 0.4 # population mean Group 1
sd1 <- 1 # standard deviation Group 1
m2 <- 0 # population mean Group 2
sd2 <- 1 # standard deviation Group 1
metadata <- data.frame(yi = numeric(0), vi = numeric(0)) # create dataframe

for (i in 1:nSims) { # for each simulated study
  n <- sample(30:100, 1) # pick a sample size per group
  x <- rnorm(n = n, mean = m1, sd = sd1) 
  y <- rnorm(n = n, mean = m2, sd = sd2)
  metadata[i,1:2] <- metafor::escalc(n1i = n, n2i = n, m1i = mean(x), 
                                     m2i = mean(y), sd1i = sd(x), sd2i = sd(y), measure = "SMD")
}

# run meta-analysis on simulated data
result <- metafor::rma(yi, vi, data = metadata, method = "FE")
par(bg = "#fffafa")
metafor::forest(result)

result


# simulating meta-analysis for dichotomous data
set.seed(5333)
nSims <- 12 # Number of simulated experiments

pr1 <- 0.7 # Set percentage of successes in Group 1
pr2 <- 0.5 # Set percentage of successes in Group 2

ai <- numeric(nSims) # set up empty vector for successes Group 1
bi <- numeric(nSims) # set up empty vector for failures Group 1
ci <- numeric(nSims) # set up empty vector for successes Group 2
di <- numeric(nSims) # set up empty vector for failures Group 2

for (i in 1:nSims) { # for each simulated experiment
  n <- sample(30:80, 1)
  x <- rbinom(n, 1, pr1) # participants (1 = success, 0 = failure)
  y <- rbinom(n, 1, pr2) # participants (1 = success, 0 = failure)
  ai[i] <- sum(x == 1) # Successes Group 1
  bi[i] <- sum(x == 0) # Failures Group 1
  ci[i] <- sum(y == 1) # Successes Group 2
  di[i] <- sum(y == 0) # Failures Group 2
}

# Combine data into dataframe
metadata <- cbind(ai, bi, ci, di)
# Create escalc object from metadata dataframe 
metadata <- escalc(measure = "OR", 
                   ai = ai, bi = bi, ci = ci, di = di, 
                   data = metadata)
# Perform Meta-analysis
result <- rma(yi, vi, data = metadata)
# Create forest plot. Using ilab and ilab.xpos arguments to add counts
par(mar=c(5, 4, 0, 2))
par(bg = "#fffafa")
forest(result, 
       ilab = cbind(metadata$ai, metadata$bi, metadata$ci, metadata$di), 
       xlim = c(-10, 8), 
       ilab.xpos = c(-7, -6, -5, -4))
text(c(-7, -6, -5, -4), 14.7, c("E+", "E-", "C+", "C-"), font = 2, cex = .8)


# Print result meta-analysis
result



library(metafor)
set.seed(2942)
nSims <- 12 # Number of simulated experiments

pr1 <- 0.7 # Set percentage of successes in Group 1
pr2 <- 0.2 # Set percentage of successes in Group 2

ai <- numeric(nSims) # set up empty vector for successes Group 1
bi <- numeric(nSims) # set up empty vector for failures Group 1
ci <- numeric(nSims) # set up empty vector for successes Group 2
di <- numeric(nSims) # set up empty vector for failures Group 2

for (i in 1:nSims/2) { # for half (/2) of the simulated studies
  n <- sample(30:80, 1)
  x <- rbinom(n, 1, pr1) # produce simulated participants (1 = success, 0 = failure)
  y <- rbinom(n, 1, pr2) # produce simulated participants (1 = success, 0 = failure)
  ai[i] <- sum(x == 1) # Successes Group 1
  bi[i] <- sum(x == 0) # Failures Group 1
  ci[i] <- sum(y == 1) # Successes Group 2
  di[i] <- sum(y == 0) # Failures Group 2
}

pr1 <- 0.9 # Set percentage of successes in Group 1
pr2 <- 0.7 # Set percentage of successes in Group 2

for (i in (nSims/2 + 1):(nSims)) { # for the other half (/2) of each simulated study
  n <- sample(30:80, 1)
  x <- rbinom(n, 1, pr1) # produce simulated participants (1 = success, 0 = failure)
  y <- rbinom(n, 1, pr2) # produce simulated participants (1 = success, 0 = failure)
  ai[i] <- sum(x == 1) # Successes Group 1
  bi[i] <- sum(x == 0) # Failures Group 1
  ci[i] <- sum(y == 1) # Successes Group 2
  di[i] <- sum(y == 0) # Failures Group 2
}

# Combine data into dataframe
metadata <- cbind(ai, bi, ci, di)
# Create escalc object from metadata dataframe 
metadata <- escalc(measure = "OR", 
                   ai = ai, bi = bi, ci = ci, di = di, 
                   data = metadata)
# Perform Meta-analysis
result <- rma(yi, vi, data = metadata)
# Print result meta-analysis
result


# Chapter 12 - Bias detection

# publication bias and the funnel plot
# install.packages("truncnorm")
library(metafor)
library(truncnorm)

nsims <- 100 # number of simulated experiments
pub.bias <- 0.05 # set percentage of significant results in the literature

m1 <- 0 # too large effects will make non-significant results extremely rare
sd1 <- 1
m2 <- 0
sd2 <- 1
metadata.sig <- data.frame(m1 = NA, m2 = NA, sd1 = NA, sd2 = NA, 
                           n1 = NA, n2 = NA, pvalues = NA, pcurve = NA)
metadata.nonsig <- data.frame(m1 = NA, m2 = NA, sd1 = NA, sd2 = NA, 
                              n1 = NA, n2 = NA, pvalues = NA, pcurve = NA)

# simulate significant effects in the expected direction
if(pub.bias > 0){
  for (i in 1:nsims*pub.bias) { # for each simulated experiment
    p <- 1 # reset p to 1
    n <- round(truncnorm::rtruncnorm(1, 20, 1000, 100, 100)) # n based on truncated normal
    while (p > 0.025) { # continue simulating as along as p is not significant
      x <- rnorm(n = n, mean = m1, sd = sd1) 
      y <- rnorm(n = n, mean = m2, sd = sd2) 
      p <- t.test(x, y, alternative = "greater", var.equal = TRUE)$p.value
    }
    metadata.sig[i, 1] <- mean(x)
    metadata.sig[i, 2] <- mean(y)
    metadata.sig[i, 3] <- sd(x)
    metadata.sig[i, 4] <- sd(y)
    metadata.sig[i, 5] <- n
    metadata.sig[i, 6] <- n
    out <- t.test(x, y, var.equal = TRUE)
    metadata.sig[i, 7] <- out$p.value
    metadata.sig[i, 8] <- paste0("t(", out$parameter, ")=", out$statistic)
  }}

# simulate non-significant effects (two-sided)
if(pub.bias < 1){
  for (i in 1:nsims*(1-pub.bias)) { # for each simulated experiment
    p <- 0 # reset p to 1
    n <- round(truncnorm::rtruncnorm(1, 20, 1000, 100, 100))
    while (p < 0.05) { # continue simulating as along as p is significant
      x <- rnorm(n = n, mean = m1, sd = sd1) # produce  simulated participants
      y <- rnorm(n = n, mean = m2, sd = sd2) # produce  simulated participants
      p <- t.test(x, y, var.equal = TRUE)$p.value
    }
    metadata.nonsig[i, 1] <- mean(x)
    metadata.nonsig[i, 2] <- mean(y)
    metadata.nonsig[i, 3] <- sd(x)
    metadata.nonsig[i, 4] <- sd(y)
    metadata.nonsig[i, 5] <- n
    metadata.nonsig[i, 6] <- n
    out <- t.test(x, y, var.equal = TRUE)
    metadata.nonsig[i, 7] <- out$p.value
    metadata.nonsig[i, 8] <- paste0("t(", out$parameter, ")=", out$statistic)
  }}

# Combine significant and non-significant effects
metadata <- rbind(metadata.nonsig, metadata.sig)

# Use escalc to compute effect sizes
metadata <- escalc(n1i = n1, n2i = n2, m1i = m1, m2i = m2, sd1i = sd1, 
                   sd2i = sd2, measure = "SMD", data = metadata[complete.cases(metadata),])
# add se for PET-PEESE analysis
metadata$sei <- sqrt(metadata$vi)

#Perform meta-analysis
result <- metafor::rma(yi, vi, data = metadata)
result

# Print a Funnel Plot
metafor::funnel(result, level = 0.95, refline = 0)
abline(v = result$b[1], lty = "dashed") # vertical line at meta-analytic ES
points(x = result$b[1], y = 0, cex = 1.5, pch = 17) # add point


# Chapter 12 - Bias detection
# install.packages("puniform")
library(puniform)
puniform::puniform(m1i = metadata$m1, m2i = metadata$m2, n1i = metadata$n1, 
                   n2i = metadata$n2, sd1i = metadata$sd1, sd2i = metadata$sd2, side = "right")


# install.packages("zcurve")
library(zcurve)
z_res <- zcurve::zcurve(p = metadata$pvalues, method = "EM", bootstrap = 1000)
summary(z_res, all = TRUE)
plot(z_res, annotation = TRUE, CI = TRUE)






# Chapter 14 - Computational reproducibility

stroop_data <- read.table("https://raw.githubusercontent.com/Lakens/Stroop/master/stroop.txt",
                          sep = "\t", header = TRUE)

write.table(stroop_data, file = "stroop.csv", quote = F, row.names = F)