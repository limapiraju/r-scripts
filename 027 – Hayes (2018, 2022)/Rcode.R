# Limpa a sessao do R
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# to avoid having to specify the location of files when reading data, 
# set your directory to where you have stored the books csv data files
# for example
path = "C:/Users/User/Desktop/hayes2022data/protest/"
setwd(path)


# Otherwise, add file paths to read.table functions in code below


#############
# CHAPTER 2 #
#############

# To what extent are people's beliefs about the role government should play
# in mitigating the potential effects of a global crisis related to their
# emotional reactions to such a crisis?

# govact = support to government actions to mitigate effects of a global crisis
# negemot = negative emotional responses to the climate change
# posemot = positive emotional responses to the climate change
# ideology = political ideology
# sex = biological sex
# age = age in years

# section 2.1
# read the glbwarm data
glbwarm <- read.table("glbwarm.csv", sep = ",", header = TRUE)
head(glbwarm)
plot(y = glbwarm$govact,x = glbwarm$negemot)
cor(glbwarm$govact, glbwarm$negemot)

# section 2.2
summary(model <- lm(govact ~ negemot, data = glbwarm))
anova(model)
confint(model, level = 0.95)
lm(scale(govact) ~ scale(negemot), data = glbwarm)

summary(model <- lm(govact ~ sex, data = glbwarm))
anova(model)
confint(model,level = 0.95)
lm(scale(govact) ~ scale(sex),data = glbwarm)

# section 2.4
model <- lm(govact ~ negemot + posemot + ideology + sex + age,
            data = glbwarm)
summary(model)
confint(model, level = 0.95)
lm(scale(govact) ~ scale(negemot) + scale(posemot) + scale(ideology) + 
     scale(sex) + scale(age), data = glbwarm)

# section 2.6
# selecting a subsample of a population of 815 individuals
# to illustratethe  sampling variance concept
indx <- sample(1:nrow(glbwarm), 50)
summary(lm(govact ~ negemot + posemot + ideology + 
             sex + age, data = glbwarm[indx,]))
# comparing models
model1 <- lm(govact ~ ideology + sex + age, data = glbwarm)
model2 <- lm(govact ~ negemot + posemot + ideology + sex + age,
             data = glbwarm)
anova(model1, model2)

# section 2.7
# multicategorical antecedent variables and dummy coding
d1 <- as.numeric(glbwarm$partyid == 1)
d2 <- as.numeric(glbwarm$partyid == 3)
glbwarm <- data.frame(glbwarm, d1, d2)
summary(lm(govact ~ d1 + d2, data = glbwarm))


#############
# CHAPTER 3 #
#############

# reaction = intention to buy sugar after reading a newspaper article
# pmi = presumed media influence
# cond = frontpage (1) or inside (0)

#section 3.3
#read the pmi data
pmi <- read.table("pmi.csv", sep = ",", header = TRUE)
head(pmi)

summary(lm(pmi ~ cond, data = pmi)) # a coefficient
summary(lm(reaction ~ cond + pmi, data = pmi)) # c' and b coefficients
summary(lm(reaction ~ cond, data = pmi)) # c coefficient
process(data = pmi,
        y = "reaction",
        x = "cond",
        m = "pmi",
        total = 1, # default = 0, do not show total effects
        normal = 1, # test of indirect effect using the normal theory approach
        model = 4, # simple mediation model
        boot = 5000, # number of bootstrap samples
        conf = 95, # confidence level of CIs
        seed = 31216)

# section 3.5
#read the estress data
estress <- read.table("estress.csv", sep = ",", header = TRUE)
head(estress)

# estress = an index of economic stress (X)
# affect = business-related depressed affect (M)
# withdraw = intentions to withdraw from entrepreneurship (Y)
process(data = estress,
        y = "withdraw",
        x = "estress",
        m = "affect",
        total = 1,
        stand = 1, # standardized effects
        model = 4,
        seed = 100770)


#############
# CHAPTER 4 #
#############

# section 4.2
#read the estress data
estress <- read.table("estress.csv", sep = ",", header = TRUE)
head(estress)

# estress = an index of economic stress (X)
# affect = business-related depressed affect (M)
# withdraw = intentions to withdraw from entrepreneurship (Y)
# ese = entrepreneurial self-efficacy (C1)
# sex = sex (0 = female, 1 = male; C2)
# tenure = length time in the business, in years (C3)

cor.test(estress$ese, estress$estress)
cor.test(estress$ese, estress$affect)
cor.test(estress$ese, estress$withdraw)

process(data = estress,
        y = "withdraw",
        x = "estress",
        m = "affect",
        cov = c("ese","sex","tenure"),
        total = 1,
        model = 4,
        seed = 100770)

# how to estimate a model multiple times
# with more than one X
process (data=datafile,y="dv",x="iv1",m="med",cov=c("iv2","iv3"),
         model=4,seed=5235)
process (data=datafile,y="dv",x="iv2",m="med",cov=c("iv1","iv3"),
         model=4,seed=5235)
process (data=datafile,y="dv",x="iv3",m="med",cov=c("iv1","iv2"),
         model=4,seed=5235)

# how to estimate a model multiple times
# with more than one Y
process (data=datafile,y="dv1",x="iv",m="med",
         model=4,seed=5235)
process (data=datafile,y="dv2",x="iv",m="med",
         model=4,seed=5235)
process (data=datafile,y="dv3",x="iv",m="med",
         model=4,seed=5235)




#############
# CHAPTER 5 #
#############

#read the pmi data
pmi<-read.table("pmi.csv", sep = ",", header = TRUE)
head(pmi)

# reaction = intention to buy sugar after reading a newspaper article
# pmi = presumed media influence
# cond = frontpage (1) or inside (0)
# import = perceived importance of potential sugar shortage

summary(lm(import~cond,data=pmi))
summary(lm(pmi~cond,data=pmi))
summary(lm(reaction~cond+import+pmi,data=pmi))
process(data=pmi,y="reaction",x="cond",m=c("import","pmi"),total=1,contrast=1,model=4,modelres=1,seed=31216)

# section 5.4
summary(lm(import ~ cond, data = pmi))
summary(lm(pmi ~ cond + import,data = pmi))
summary(lm(reaction ~ cond + import + pmi, data = pmi))


process(data = pmi,
        y = "reaction",
        x = "cond",
        m = c("import", "pmi"),
        total = 1,
        contrast = 1, # 1 = comparing different indirect effects
        # 2 = comparing strength of indirect effects ignoring signs
        model = 4,
        seed = 31216)



process(data = pmi,
        y = "reaction",
        x = "cond",
        m = c("import", "pmi"), # order matters!
        total = 1,
        contrast = 1, # 1 = comparing different indirect effects
        # 2 = comparing strength of indirect effects ignoring signs
        model = 6, # serial multiple mediator model
        seed = 31216)

#############
# CHAPTER 6 #
#############

# section 6.2
#read the protest data
protest <- read.table("protest.csv", sep = ",", header = TRUE)
head(protest)

# protest = 0 (no protest), 1 (individual protest), and 2 (collective protest)
# liking = how much participants liked Catherine
# respappr = response appropriateness judgment


d1 <- as.numeric(protest$protest == 1)
d2 <- as.numeric(protest$protest == 2)
protest <- data.frame(protest, d1, d2)
summary(lm(liking ~ d1 + d2, data = protest))
summary(lm(respappr ~ d1 + d2, data = protest))
summary(lm(liking ~ d1 + d2 + respappr, data = protest))

model1 <- lm(liking ~ respappr, data = protest)
model2 <- lm(liking ~ respappr + d1 + d2, data = protest)
anova(model1, model2)

process(data = protest,
        y = "liking",
        x = "protest",
        m = "respappr",
        mcx = 1, # multicategorical x = indicator coding (1)
        total = 1,
        model = 4,
        seed = 30217)

# section 6.3
# manually defining the Helmert coding
d1 <- (protest$protest == 0) * (-2/3) + (protest$protest > 0) * (1/3)
d2 <- (protest$protest == 1) * (-1/2) + (protest$protest==2) * (1/2)
protest <- data.frame(protest, d1, d2)

process(data = protest,
        y = "liking",
        x = "protest",
        m = "respappr",
        mcx = 3, # multicategorical x, Helmert coding (3)
        total = 1,
        model = 4,
        seed = 30217)


#############
# CHAPTER 7 #
#############

#section 7.2
#read the disaster data
disaster <- read.table("disaster.csv", sep = ",", header = TRUE)
head(disaster)
framskep <- disaster$frame * disaster$skeptic
disaster <- data.frame(disaster, framskep)
summary(lm(justify ~ frame + skeptic + framskep, data = disaster))
summary(lm(justify ~ frame + skeptic + frame * skeptic, data = disaster))

process(data = disaster,
        y = "justify",
        x = "frame",
        w = "skeptic",
        model = 1,
        jn = 1,
        plot = 1)

# mean-centering
skepticp <- disaster$skeptic - 3.378
framskpp <- disaster$frame * skepticp
summary(lm(disaster$justify ~ disaster$frame + skepticp + framskpp))

#section 7.3
x<-c(0,1,0,1,0,1)
w<-c(1.592,1.592,2.80,2.80,5.20,5.20)
y<-c(2.619,2.377,2.746,2.747,2.998,3.482)
plot(y=y,x=w,pch=15,col="black",xlab="Climate Change Skepticism (W)",
ylab="Negative Justifications (Y)")
legend("topleft",legend=legend.txt,lty=c(3,1),lwd=c(3,2),col=c("blue","red"))
lines(w[x==0],y[x==0],lwd=3,lty=3,col="blue")
lines(w[x==1],y[x==1],lwd=2,lty=1,col="red")

#section 7.4
skepticp<-disaster$skeptic-1.592;framskpp<-disaster$frame*skepticp
summary(lm(disaster$justify~disaster$frame+skepticp+framskpp))
skepticp<-disaster$skeptic-2.8;framskpp<-disaster$frame*skepticp
summary(lm(disaster$justify~disaster$frame+skepticp+framskpp))
skepticp<-disaster$skeptic-5.2;framskpp<-disaster$frame*skepticp
summary(lm(disaster$justify~disaster$frame+skepticp+framskpp))

skeptic<-c(1,1.17,1.42,1.84,2.26,2.68,3.11,3.53,3.94,3.95,4.37,
4.79,5.21,5.63,6.05,6.47,6.89,7.32,7.74,8.16,8.58,9)
effect<-c(-.361,-.327,-.277,-.192,-.107,-.023,.062,.147,.229,.232,.316,
.401,.486,.571,.655,.740,.825,.909,.994,1.079,1.164,1.249)
llci<-c(-.702,-.654,-.584,-.469,-.360,-.256,-.161,-.075,0,.002,.071,
.132,.188,.241,.290,.337,.383,.427,.471,.513,.556,.597)
ulci<-c(-.021,0,.031,.086,.145,.211,.285,.368,.458,.461,.562,.670,.783,
.901,1.021,1.143,1.267,1.392,1.518,1.644,1.771,1.899)
plot(x=skeptic,y=effect,type="l",pch=19,ylim=c(-1,1.5),xlim=c(1,6),
lwd=3,col="black",ylab="Conditional effect of disaster frame",
xlab="Climate Change Skepticism (W)")
points(skeptic,llci,lwd=1.75,lty=2,type="l",col="red")
points(skeptic,ulci,lwd=1.75,lty=2,type="l",col="red")
abline(h=0, untf=FALSE,lty=3,lwd=1)
abline(v=1.171,untf=FALSE,lty=3,lwd=1)
abline(v=3.934,untf=FALSE,lty=3,lwd=1)
text(1.171,-1,"1.171",cex=0.8)
text(3.934,-1,"3.934",cex=0.8)

result<-process(data=disaster,y="justify",x="frame",w="skeptic",model=1,jn=1,save=2)
skeptic<-result[12:33,1]
effect<-result[12:33,2]
llci<-result[12:33,6]
ulci<-result[12:33,7]
plot(x=skeptic,y=effect,type="l",pch=19,ylim=c(-1,1.5),xlim=c(1,6),
lwd=3,col="black",ylab="Conditional effect of disaster frame",
xlab="Climate Change Skepticism (W)")
points(skeptic,llci,lwd=1.75,lty=2,type="l",col="red")
points(skeptic,ulci,lwd=1.75,lty=2,type="l",col="red")
abline(h=0, untf=FALSE,lty=3,lwd=1)
abline(v=1.171,untf=FALSE,lty=3,lwd=1)
abline(v=3.934,untf=FALSE,lty=3,lwd=1)
text(1.171,-1,"1.171",cex=0.8)
text(3.934,-1,"3.934",cex=0.8)

# section 7.6
t.test(justify~frame,var.equal=TRUE,data=subset(disaster,skeptic<3.378))
t.test(justify~frame,var.equal=TRUE,data=subset(disaster,skeptic>=3.378))


#############
# CHAPTER 8 #
#############


#section 8.1
#read the disaster data
disaster <- read.table("disaster.csv", sep=",",header=TRUE)
head(disaster)
summary(lm(justify ~ skeptic + frame + frame * skeptic, data = disaster))

process(data = disaster,
        y = "justify",
        x = "skeptic",
        w = "frame",
        model = 1,
        plot = 1)

framep <- 1 - disaster$frame; frampskp <- framep * disaster$skeptic;
disaster <- data.frame(disaster, framep, frampskp)
summary(lm(justify ~ skeptic + framep + frampskp, data = disaster))

#section 8.2
#read the glbwarm data
glbwarm <- read.table("glbwarm.csv", sep = ",", header = TRUE)
head(glbwarm)

summary(lm(govact ~ negemot + age + negemot * age
           + posemot + ideology + sex, data = glbwarm))

process(data = glbwarm,
        y = "govact",
        x = "negemot",
        w = "age",
        cov = c("posemot","ideology","sex"),
        model = 1,
        jn = 1,
        plot = 1)

process(data = glbwarm,
        y = "govact",
        x = "negemot",
        w = "age",
        cov = c("posemot","ideology","sex"),
        model = 1,
        jn = 1,
        plot = 1,
        wmodval = c(30,50,70))

x <- c(1.67,3.67,5.33,1.67,3.67,5.33,1.67,3.67,5.33)
w <- c(30,30,30,50,50,50,70,70,70)
y <- c(4.038,4.657,5.171,3.772,4.644,5.362,3.506,4.631,5.565)
wmarker <- c(15,15,15,16,16,16,17,17,17)
plot(y=y,x=x,cex=1.2,pch=wmarker,xlab="Negative Emotions (W)",
ylab="Support for Government Action (Y)")
legend.txt<-c("30 years old","50 years old", "70 years old")
legend("topleft", legend = legend.txt,cex=1,lty=c(1,3,6),lwd=c(2,3,2),
pch=c(15,16,17))
lines(x[w==30],y[w==30],lwd=2,col="black")
lines(x[w==50],y[w==50],lwd=3,lty=3,col="black")
lines(x[w==70],y[w==70],lwd=2,lty=6,col="black")

result<-process(data=glbwarm,y="govact",x="negemot",w="age",cov=c("posemot","ideology","sex"),model=1,jn=1,plot=1,save=2)
age<-result[15:36,1]
effect<-result[15:36,2]
llci<-result[15:36,6]
ulci<-result[15:36,7]
plot(age,effect,type="l",pch=19,ylim=c(-.5,1),xlim=c(15,90),lwd=3,
ylab="Conditional effect of negative emotions",xlab="Age (W)")
points(age,llci,lwd=2,lty=2,type="l",col="red")
points(age,ulci,lwd=2,lty=2,type="l",col="red")
abline(h=0,untf=FALSE,lty=3,lwd=1)

#section 8.4
#read the caskets data
caskets<-read.table("caskets.csv", sep=",",header=TRUE)
head(caskets)
kerryc<-caskets$kerry-0.5;policyc<-caskets$policy-0.5
caskets<-data.frame(caskets,kerryc,policyc)
process(data=caskets,y="interest",x="policyc",w="kerryc",plot=1,model=1)


#############
# CHAPTER 9 #
#############

#section 9.1
#read the glbwarm data
glbwarm<-read.table("glbwarm.csv", sep=",",header=TRUE)
head(glbwarm)
negemotc<-glbwarm$negemot-3.558;agec<-glbwarm$age-49.536
glbwarm<-data.frame(glbwarm,negemotc,agec)
summary(lm(govact~negemotc+agec+negemotc*agec,data=glbwarm))

#section 9.2
#read the glbwarm data
glbwarm<-read.table("glbwarm.csv", sep=",",header=TRUE)
head(glbwarm)
zgovact<-scale(glbwarm$govact)
znegemot<-scale(glbwarm$negemot)
zage<-scale(glbwarm$age)
glbwarm<-data.frame(glbwarm,zgovact,znegemot,zage)
summary(lm(zgovact~znegemot+zage+znegemot*zage,data=glbwarm))

#section 9.4
process(data=glbwarm,y="govact",x="negemot",w="sex",z="age",cov=c("posemot","ideology"),model=2,plot=1,zmodval=c(30,50,70))

oldp<-par(mfrow=c(3,1),mar=c(3,4,0,0),oma=c(2,2,2,2),mgp=c(5,0.5,0))
x<-c(1.67,3.67,5.33,1.67,3.67,5.33)
w<-c(0,0,0,1,1,1)
yage30<-c(4.2003,4.6714,5.0624,3.800,4.6801,5.4106)
yage50<-c(3.9943,4.6554,5.2041,3.5941,4.6641,5.5523)
yage70<-c(3.7883,4.6394,5.3458,3.3881,4.6481,5.6940)
legend.txt<-c("Female (W=0)","Male (W=1)")
for (i in 1:3){
if (i==1)
  {y<-yage30
  legend2.txt<-c("Age (Z) = 30")}
if (i==2)
  {y<-yage50
  legend2.txt<-c("Age (Z) = 50")}
if (i==3)
  {y<-yage70
  legend2.txt<-c("Age (Z) = 70")}
plot(y=y,x=x,col="white",ylim=c(3,6),cex=1.5,xlim=c(1,6),tcl=-0.5)
  lines(x[w==0],y[w==0],lwd=2,lty=2)
  lines(x[w==1],y[w==1],lwd=2,lty=1)
  legend("topleft",legend=legend.txt,lwd=2,lty=c(2,1))
  legend("bottomright",legend=legend2.txt)
box}
mtext("Negative Emotions (X)",side=1,outer=TRUE)
mtext("Support for Government Action",side=2,outer=TRUE)
par(oldp)

process(data=glbwarm,y="govact",x="negemot",w="sex",z="age",cov=c("posemot","ideology"),model=3,jn=1,plot=1,zmodval=c(30,50,70))

oldp<-par(mfrow=c(3,1),mar=c(3,4,0,0),oma=c(2,2,2,2),mgp=c(5,0.5,0))
x<-c(1.67,3.67,5.33,1.67,3.67,5.33)
w<-c(0,0,0,1,1,1)
yage30<-c(4.0561,4.6566,5.1551,3.9422,4.6819,5.2959)
yage50<-c(4.0190,4.6562,5.1850,3.6220,4.6654,5.5314)
yage70<-c(3.9820,4.6557,5.2149,3.3017,4.6489,5.7670)
legend.txt<-c("Female (W=0)","Male (W=1)")
for (i in 1:3){
if (i==1)
  {y<-yage30
  legend2.txt<-c("Age (Z) = 30")}
if (i==2)
  {y<-yage50
  legend2.txt<-c("Age (Z) = 50")}
if (i==3)
  {y<-yage70
  legend2.txt<-c("Age (Z) = 70")}
plot(y=y,x=x,col="white",ylim=c(3,6),cex=1.5,xlim=c(1,6),tcl=-0.5)
  lines(x[w==0],y[w==0],lwd=2,lty=2)
  lines(x[w==1],y[w==1],lwd=2,lty=1)
  legend("topleft", legend=legend.txt,lwd=2,lty=c(2,1))
  legend("bottomright",legend=legend2.txt)
box}
mtext("Negative Emotions (X)",side=1,outer=TRUE)
mtext("Support for Government Action",side=2,outer=TRUE)
par(oldp)

result<-process(data=glbwarm,y="govact",x="negemot",w="sex",z="age",cov=c("posemot","ideology"),model=3,jn=1,plot=1,save=2)
age<-result[24:45,1]
effect<-result[24:45,2]
llci<-result[24:45,6]
ulci<-result[24:45,7]
plot(age,effect,type="l",pch=19,ylim=c(-1,1.5),xlim=c(15,90),lwd=3,
ylab="Conditional negative emotions by sex interaction",xlab="Age")
points(age,llci,lwd=2,lty=2,type="l",col="red")
points(age,ulci,lwd=2,lty=2,type="l",col="red")
abline(h=0,untf=FALSE,lty=3,lwd=1)
abline(v=38.114,untf=FALSE,lty=3,lwd=1)
text(38.114,-1,"38.114",cex=0.8)

#section 9.5
process (data=glbwarm,y="govact",x="negemot",w="sex",z="age",cov=c("posemot","ideology"),model=3,zmodval=c(30,50,70),contrast=c(1,30,0,50))


##############
# CHAPTER 10 #
##############

#section 10.2
#read the protest data
protest <- read.table("protest.csv", sep = ",", header = TRUE)
head(protest)
d1 <- as.numeric(protest$protest == 1)
d2 <- as.numeric(protest$protest == 2)
protest <- data.frame(protest, d1, d2)

model1 <-lm(liking ~ d1 + d2 + sexism, data = protest)
summary(model1)

model2 <- lm(liking ~ d1 + d2 + sexism +
               d1 * sexism + d2 * sexism, data = protest)
summary(model2)

anova(model1,model2)

process(data = protest,
        y = "liking",
        x = "protest",
        w = "sexism",
        mcx = 1, # multicategorical x
        model = 1,
        plot = 1)

#section 10.3
x<-c(0,1,2,0,1,2,0,1,2)
w<-c(4.25,4.25,4.25,5.12,5.12,5.12,5.896,5.896,5.896)
y<-c(5.698,5.400,5.513,5.287,5.773,5.779,4.920,6.105,6.016)
plot(y=y,x=w,pch=15,col="white",xlab="Perceived pervasiveness of sex discrimination (W)",ylab="Liking of the attorney (Y)")
  legend.txt<-c("No protest","Individual protest","Collective protest")
  legend("topleft",legend=legend.txt,lty=c(1,3,1),lwd=c(4,4,1))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
lines(w[x==2],y[x==2],lwd=1,lty=1)

#section 10.4
protest<-read.table("protest.csv", sep=",",header=TRUE)
head(protest)
d1<-as.numeric(protest$protest==1)
d2<-as.numeric(protest$protest==2)
sexismp<-protest$sexism-4.25
d1sexp<-d1*sexismp;d2sexp<-d2*sexismp
protest<-data.frame(protest,d1,d2,sexismp,d1sexp,d2sexp)
model1<-lm(liking~sexismp+d1sexp+d2sexp,data=protest)
model2<-lm(liking~d1+d2+sexismp+d1sexp+d2sexp,data=protest)
summary(model2)
anova(model1,model2)

protest<-read.table("protest.csv", sep=",",header=TRUE)
head(protest)
d1<-as.numeric(protest$protest==1);d2<-as.numeric(protest$protest==2)
d1sexism<-d1*protest$sexism;d2sexism<-d2*protest$sexism
protest<-data.frame(protest,d1,d2,d1sexism,d2sexism)
process (data=protest,y="liking",x="d1",w="sexism",cov=c("d2","d2sexism"),jn=1,model=1)
process (data=protest,y="liking",x="d2",w="sexism",cov=c("d1","d1sexism"),jn=1,model=1)

#section 10.5
process(data=protest,y="liking",x="sexism",w="protest",mcw=1,model=1)


##############
# CHAPTER 11 #
##############

#section 11.3
#read the teams data
teams<-read.table("teams.csv", sep=",",header=TRUE)
head(teams)
toneexp<-teams$negtone*teams$negexp
teams<-data.frame(teams,toneexp)
summary(lm(negtone~dysfunc,data=teams))
summary(lm(perform~dysfunc+negtone+negexp+toneexp,data=teams))
process(data=teams,y="perform",x="negtone",w="negexp",cov="dysfunc",model=1,plot=1)

#section 11.4
process(data=teams,y="perform",x="dysfunc",m="negtone",w="negexp",model=14,plot=1,seed=42517)

#section 11.5
x<-c(0,1,0,1,0,1)
w<-c(-0.531,-0.531,-0.060,-0.060,0.600,0.600)
y<-c(0.366,-0.161,0.366,-0.405,0.366,-0.746)
plot(y=y,x=w,pch=15,col="white",
  xlab="Nonverbal negative expressivity",
  ylab="Effect of dysfunctional team behavior")
  legend.txt<-c("Direct effect","Indirect effect")
  legend("bottomleft",legend=legend.txt,lty=c(1,3),lwd=c(4,3))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
abline(0,0,lwd=0.5,lty=2)

#section 11.6
result<-process(data=teams,y="perform",x="dysfunc",m="negtone",w="negexp",model=14,save=3,seed=42517)
boots<-result[[1]];poutput<-result[[2]]
modvalt<-teams$negexp
minmod<-min(modvalt);maxmod<-max(modvalt)
modval<-matrix(seq(minmod,maxmod,((maxmod-minmod)/10000)))
llci<-modval;ulci<-llci;jnfind<-matrix(999999)
effect<-poutput[3,1]*(poutput[7,1]+poutput[9,1]*modval)
for (i in (1:length(modval)))
{bootind<-boots$col2*(boots$col5+boots$col7*modval[i])
 llci[i,]<-matrix(quantile(bootind,.025))
 ulci[i,]<-matrix(quantile(bootind,.975))
}
chk<-sign(llci*ulci)
for (i in (1:(length(modval)-1)))
{
 chk2<-sign(chk[i]*chk[(i+1)])
 if (chk2 < 0)
 {
  vala<-ulci[i];valb<-ulci[i+1]
  chk3<-sign(llci[i]*llci[i+1])
  if (chk3 < 1)
  {
  vala<-llci[i];valb<-llci[i+1]
  }
  wgt1<-(1-(abs(vala)/abs(vala-valb)))
  wgt2<-(1-(abs(valb)/abs(vala-valb)))
  jnfind<-cbind(jnfind,(wgt1*modval[i]+wgt2*modval[i+1]))
 }
}
plot(x=modval,y=effect,type="l",pch=19,lwd=3,
  xlim=c(min(modval),max(modval)),ylim=c(min(llci),max(ulci)),
  ylab="Conditional indirect effect of dysfunctional behavior",
  xlab="Team expressivity (W)")
points(modval,llci,lwd=2,lty=2,type="l",col="black")
points(modval,ulci,lwd=2,lty=2,type="l",col="black")
abline(h=0,untf = FALSE,lty=3,lwd=1,col="red")
if (length(jnfind) > 1)
{
 jnfind<-jnfind[2:length(jnfind)]
 for (i in (1:length(jnfind)))
 {
  abline(v=jnfind[i],untf=FALSE,lty=3,lwd=1)
  text(jnfind[i],min(llci),format(jnfind[i]),cex=0.8)
 }
}
print(jnfind)


##############
# CHAPTER 12 #
##############

#section 12.1	
#read the disaster data
disaster<-read.table("disaster.csv", sep=",",header=TRUE)
head(disaster)
process (data=disaster,y="donate",x="frame",w="skeptic",model=1,plot=1)
process (data=disaster,y="donate",x="frame",m="justify",model=4,total=1,seed=280417)

#section 12.2
framskep<-disaster$frame*disaster$skeptic
disaster<-data.frame(disaster,framskep)
summary(lm(justify~frame+skeptic+framskep,data=disaster))
summary(lm(donate~frame+skeptic+framskep+justify,data=disaster))
process(data=disaster,y="donate",x="frame",w="skeptic",m="justify",model=8,plot=1,seed=280417)

x<-c(0,1,0,1,0,1)
w<-c(1.592,1.592,2.800,2.800,5.200,5.200)
y<-c(0.184,0.224,0.202,-0.001,0.238,-0.446)
plot(y=y,x=w,pch=15,col="white",
  xlab="Climate change skepticism",
  ylab="Effect of frame on willingness to donate")
  legend.txt<-c("Direct effect","Indirect effect")
  legend("bottomleft",legend=legend.txt,lty=c(1,3),lwd=c(4,3))
lines(w[x==0],y[x==0],lwd=4,lty=1)
lines(w[x==1],y[x==1],lwd=4,lty=3)
abline(0,0,lwd=0.5,lty=2)

#section 12.3

result<-process(data=disaster,y="donate",x="frame",m="justify",w="skeptic",model=8,save=3,seed=284017)
boots<-result[[1]];poutput<-result[[2]]
modvalt<-disaster$skeptic
minmod<-min(modvalt);maxmod<-max(modvalt)
modval<-matrix(seq(minmod,maxmod,((maxmod-minmod)/10000)))
llci<-modval;ulci<-llci;jnfind<-matrix(999999)
effect<-(poutput[3,1]+poutput[5,1]*modval)*poutput[13,1]
for (i in (1:length(modval)))
{
 bootind<-(boots$col2+boots$col4*modval[i])*boots$col7
 llci[i,]<-matrix(quantile(bootind,.025))
 ulci[i,]<-matrix(quantile(bootind,.975))
}
chk<-sign(llci*ulci)
for (i in (1:(length(modval)-1)))
{
 chk2<-sign(chk[i]*chk[(i+1)])
 if (chk2 < 0)
 {
  vala<-ulci[i];valb<-ulci[i+1]
  chk3<-sign(llci[i]*llci[i+1])
  if (chk3 < 1)
  {
  vala<-llci[i];valb<-llci[i+1]
  }
  wgt1<-(1-(abs(vala)/abs(vala-valb)))
  wgt2<-(1-(abs(valb)/abs(vala-valb)))
  jnfind<-cbind(jnfind,(wgt1*modval[i]+wgt2*modval[i+1]))
 }
}
plot(x=modval,y=effect,type="l",pch=19,lwd=3,
 xlim=c(min(modval),max(modval)),ylim=c(min(llci),max(ulci)),
 ylab="Conditional indirect effect of disaster frame",
 xlab="Climate change skepticism (W)")
points(modval,llci,lwd=2,lty=2,type="l",col="black")
points(modval,ulci,lwd=2,lty=2,type="l",col="black")
abline(h=0,untf = FALSE,lty=3,lwd=1,col="red")
if (length(jnfind) > 1)
{
 jnfind<-jnfind[2:length(jnfind)]
 for (i in (1:length(jnfind)))
 {
  abline(v=jnfind[i],untf=FALSE,lty=3,lwd=1)
  text(jnfind[i],min(llci),format(jnfind[i]),cex=0.8)
 }
}
print(jnfind)

process(data=disaster,y="donate",x="frame",w="skeptic",m="justify",model=7,plot=1,cov="skeptic",seed=280417)

#section 12.4
disaster<-read.table("disaster.csv", sep=",",header=TRUE)
head(disaster)
framskep<-disaster$frame*disaster$skeptic
disaster<-data.frame(disaster,framskep)
process (data=disaster,y="donate",x="framskep",m="justify",cov=c("frame","skeptic"),model=4,total=1,seed=280417)


##############
# CHAPTER 13 #
##############

#section 13.1	
#read the protest data
protest<-read.table("protest.csv", sep=",",header=TRUE)
head(protest)
d1<-(protest$protest==0)*(-2/3)+(protest$protest > 0)*(1/3)
d2<-(protest$protest==1)*(-1/2)+(protest$protest==2)*(1/2)
d1sexism<-d1*protest$sexism;d2sexism<-d2*protest$sexism
protest<-data.frame(protest,d1,d2,d1sexism,d2sexism)
summary(lm(respappr~d1+d2+sexism+d1sexism+d2sexism,data=protest))
summary(lm(liking~d1+d2+sexism+d1sexism+d2sexism+respappr,data=protest))
process(data=protest,y="liking",x="protest",m="respappr",w="sexism",plot=1,mcx=3,model=8,seed=290417)



##############
# APPENDIX A #
##############

#read the pmi data
pmi<-read.table("pmi.csv", sep=",",header=TRUE)
head(pmi)
result<-process(data=pmi,y="reaction",x="cond",m="pmi",model=4,seed=31216,save=1)
ab<-result$col2*result$col5
hist(ab,breaks=25)
diff<-result$col4-ab
quantile(diff,c(.025,.975))

disaster<-read.table("disaster.csv", sep=",",header=TRUE)
head(disaster)
result<-process(data=disaster,y="justify",x="skeptic",w="frame",model=1,plot=1,save=2)