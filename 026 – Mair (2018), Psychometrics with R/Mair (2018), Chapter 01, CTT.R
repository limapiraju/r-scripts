# Mair (2018), Chapter 01

# Ilustrando o cálculo do alfa de Cronbach

install.packages("MPsychoR")
library("MPsychoR")
library("psych")

# Lendo os dados da própria biblioteca MPsychoR
data("Rmotivation")

# Selecionando apenas os itens de motivação híbrida
ind <- grep("hyb", colnames(Rmotivation))
HybMotivation <- na.omit(Rmotivation[,ind]) ## item selection

# Computando o número de itens do fator
k <- ncol(HybMotivation)

# Calculando a matriz de variância–covariância dos itens do fator motivação híbrida
vcmat <- cov(HybMotivation)

# Calculando o traço da matriz (i.e., a soma dos variâncias, os elementos da diagonal principal)
sigma2_Xi <- tr(vcmat)

# Calculando a variância total (i.e., a soma de todos os elementos da matriz)
sigma2_X <- sum(vcmat)

# Aplicando a fórmula do alfa de Cronbach (Eq. 1.5 do livro)
cronalpha <- k / (k - 1) * (1 - sigma2_Xi / sigma2_X)
round(cronalpha, 4)

# Aplicando a fórmula do erro padrão de mensuração (Eq. 1.4 do livro)
sem <- sqrt(sigma2_X) * sqrt(1 - cronalpha)
sem

# Agora, calculando o alfa usando o pacote psych (Revelle, 2017)
alpha.hyb <- psych::alpha(HybMotivation)
round(alpha.hyb$total[1], 4) ## Cronbach's alpha

# Greatest lower bound (GLB)
glb(HybMotivation)


# ômega H de McDonald: fornece a proporção de variância nos escores observados explicada
# por um único fator geral (isso é diferente de unidimensionalidade; refere-se a um fator
# dominante; seu limite superior não é 1; Revelle & Zinbarg, 2009)

# ômega t de McDonald: fornece a proporção de variância nos escores observados explicada
# por um único todos os fatores, independentemente de um fator geral
# (seu limite superior é 1; Revelle & Zinbarg, 2009)

# ômega H: Omega Hierarchical
# ômega t: ômega Total
omega(HybMotivation)



# Generalizability Theory (G-Theory)

# Calculando o alfa de Cronbach a partir das somas dos quadrados da ANOVA

library("reshape2")
Hyb1 <- data.frame(HybMotivation,
                   person = 1:nrow(HybMotivation))
Hyblong <- melt(Hyb1, id.vars = c("person"),
                variable.name = "item")
Hyblong$person <- as.factor(Hyblong$person)
summary(aov(value ~ person + item, data = Hyblong))

# Aproximação do alfa: [MS(pessoa) - MS(erro)] / MS(pessoa)
alfa <- (0.85 - 0.15) / 0.85
alfa

# A mesma lógica da ANOVA é aplicada aos ICCs; ICC(3, k) é o que nos interessa, a seguir
icchyb <- ICC(HybMotivation)
icchyb

# Calculando componentes de variância
library("lme4")
VarCorr(lmer(value ~ (1|person) + (1|item), data = Hyblong))

# Os componentes de variância acima são usados para calcular o coeficiente de generalizabilidade
# De forma alternativa, usamos a biblioteca gtheory (Moore, 2016), que chama internamente
# a biblioteca lme4 para calcular o coeficiente
install.packages("gtheory")
library("gtheory")
gfit <- gstudy(data = Hyblong,
               formula = value ~ (1|person) + (1|item))
dfit <- dstudy(gfit, colname.objects = "person",
               colname.scores = "value", data = Hyblong)
round(dfit$generalizability, 3)


# Múltiplas fontes de erro

# Preparando os dados
data("Lakes")
phydat <- subset(Lakes, subtest == "physical")
phydat$item <- droplevels(phydat$item)
head(phydat)

# Ajustando uma ANOVA de efeitos aleatórios para obter vários componentes de variância
formula <- score ~ (1|personID) + (1|raterID) + (1|item) +
  (1|personID:raterID) + (1|personID:item) + (1|raterID:item)
gfit <- gstudy(formula = formula, data = phydat)
gfit