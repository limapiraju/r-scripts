# 021 - Meta-analise

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/021 - Meta-análise/"
setwd(path)

# importa biblioteca
library(readxl)
library(meta)

# meta-analise com diferenca simples entre medias

# le dados
Freitas2020 <- read_excel("Freitas2020.xlsx")
View(Freitas2020)

# cria um objeto da meta-analise de modelo de efeito fixo
ma.cinesiofobia_f <- metacont(n.e, m.e, sd.e,         # dados do grupo experimental
                            n.c, m.c, sd.c,           # dados do grupo controle
                            paste(autor, ano),        # autor e ano
                            data = Freitas2020,       # banco de dados
                            random = FALSE            # modelo de efeito fixo
                            )

ma.cinesiofobia_f
  
# cria forest plot
forest(ma.cinesiofobia_f,
       col.square = "pink",
       col.diamond = "red",
       digits.se = 2,
       label.e = "Pilates",
       label.c = "Control",
       label.left = "Favor Pilates",
       label.right = "Favor Control",
       test.overall = TRUE)


# cria um objeto da meta-analise de modelo de efeitos aleatorios
ma.cinesiofobia_r <- metacont(n.e, m.e, sd.e,           # dados do grupo experimental
                              n.c, m.c, sd.c,           # dados do grupo controle
                              paste(autor, ano),        # autor e ano
                              data = Freitas2020,       # banco de dados
                              fixed = FALSE,            # modelo de efeitos aleatorios
                              method.tau = "DL"
                              )

ma.cinesiofobia_r

# cria forest plot
forest(ma.cinesiofobia_r,
       col.square = "pink",
       col.diamond = "red",
       digits.se = 2,
       label.e = "Pilates",
       label.c = "Control",
       label.left = "Favor Pilates",
       label.right = "Favor Control",
       test.overall = TRUE)


# meta-analise com g de Hedges

# le dados
Orrow2012 <- read_excel("Orrow2012.xlsx")
View(Orrow2012)


# a promocao de atividade fisica (atencao primaria) promove
# os niveis de atividade fisica em adultos sedentarios?
# cria um objeto da meta-analise de modelo de efeitos aleatorios
atividade_fisica <- metacont(n_e, m_e, sd_e,    # dados do grupo experimental
                             n_c, m_c, sd_c,    # dados do grupo controle
                             paste(Study, year), # autor e ano
                             data = Orrow2012,  # banco de dados
                             fixed = FALSE,     # modelo de efeitos aleatorios
                             method.tau = "DL",
                             sm = "SMD")

atividade_fisica

# cria forest plot
forest(atividade_fisica,
       col.square = "blue",
       col.diamond = "black",
       digits.se = 2,
       label.e = "Physical Activity",
       label.c = "Control",
       label.left = "Favor Control",
       label.right = "Favor Physical Activity",
       test.overall = TRUE)


# meta-analise com risco relativo

# a vacina BCG, comparada a um placebo, em pessoas com risco
# para tuberculose diminui esse risco?


# le dados
Colditz1994 <- read_excel("Colditz1994.xlsx")
View(Colditz1994)


# cria um objeto da meta-analise de modelo de efeitos aleatorios
vacina_bcg <- metabin(event_e, n_e,       # dados do grupo experimental
                      event_c, n_c,       # dados do grupo controle
                      paste(Study, year), # autor e ano
                      data = Colditz1994, # banco de dados
                      fixed = FALSE,      # modelo de efeitos aleatorios
                      method = "MH")      # metodo Mantel-Haenszel

summary(vacina_bcg)

# cria forest plot
forest(vacina_bcg,
       col.square = "darkgreen",
       col.diamond = "darkblue",
       digits.se = 2,
       label.e = "BCG",
       label.c = "Placebo",
       label.left = "Favor BCG",
       label.right = "Favor Placebo",
       test.overall = TRUE)


# o uso de videogames com jogos interativos eh melhor que a fisioterapia
# convecional para melhorar o controle postural em pacientes pos-AVC?

# le dados
Ferreira2018 <- read_excel("Ferreira2018.xlsx")
View(Ferreira2018)

# cria um objeto da meta-analise de modelo de efeitos aleatorios
videogames <- metacont(n_exp, mean_exp, sd_exp,    # dados do grupo experimental
                       n_ctrl, mean_ctrl, sd_ctrl, # dados do grupo controle
                       paste(Author, year),        # autor e ano
                       data = Ferreira2018,        # banco de dados
                       fixed = FALSE,              # modelo de efeitos aleatorios
                       method.tau = "DL",          # metodo dos momentos
                       sm = "SMD")                 # g de Hedges

summary(videogames)

# cria forest plot
forest(videogames,
       col.square = "blue",
       col.diamond = "black",
       digits.se = 2,
       xlim = c(-3, 3),
       label.e = "Video Game",
       label.c = "Physiotheraphy",
       label.left = "Favor Video Game",
       label.right = "Favor Physiotheraphy",
       test.overall = TRUE)

# educacao para motoristas, comparada ao grupo controle, aumenta 
# a probabilidade de pilotos da corrida maluca de vencer corridas?

# le dados
Vigarista2020 <- read_excel("Vigarista2020.xlsx")
View(Vigarista2020)

# cria um objeto da meta-analise de modelo de efeitos aleatorios
corrida_maluca <- metabin(ev_exp, total_exp,    # dados do grupo experimental
                          ev_ctr, total_ctr,    # dados do grupo controle
                          Study,                # autor e ano
                          data = Vigarista2020, # banco de dados
                          fixed = FALSE,        # modelo de efeitos aleatorios
                          sm = "OR")            # odds ratio

summary(corrida_maluca)

# cria forest plot
forest(corrida_maluca,
       col.square = "blue",
       col.diamond = "black",
       digits.se = 2,
       #xlim = c(-3, 3),
       label.e = "Education",
       label.c = "Control",
       label.left = "Favor Control",
       label.right = "Favor Education",
       test.overall = TRUE)


# analise de subgrupos
corrida_maluca2 <- metabin(ev_exp, total_exp,    # dados do grupo experimental
                           ev_ctr, total_ctr,    # dados do grupo controle
                           Study,                # autor e ano
                           data = Vigarista2020, # banco de dados
                           fixed = FALSE,        # modelo de efeitos aleatorios
                           sm = "OR",            # odds ratio
                           subgroup = Subgroup)


summary(corrida_maluca2)

# cria forest plot
forest(corrida_maluca2,
       col.square = "blue",
       col.diamond = "black",
       digits.se = 2,
       #xlim = c(-3, 3),
       label.e = "Education",
       label.c = "Control",
       label.left = "Favor Control",
       label.right = "Favor Education",
       test.overall = TRUE)


# vies de publicacao

# o treinamento intervalar de alta intensidade eh mais efetivo que o 
# exercicio continuo em melhorar a capacidade cardiorrespiratoria
# em pacientes com doenca arterial coronariana

# le dados
GomesNeto2017 <- read_excel("GomesNeto2017.xlsx")
View(GomesNeto2017)

# cria um objeto da meta-analise de modelo de efeitos aleatorios
treinamento <- metacont(n_e, m_e, sd_e,    # dados do grupo experimental
                        n_c, m_c, sd_c,    # dados do grupo controle
                        paste(Autor, Ano), # autor e ano
                        data = GomesNeto2017,
                        fixed = FALSE     # modelo de efeitos aleatorios
                        )

summary(treinamento)


# cria forest plot
forest(treinamento,
       label.right = "Favor HIIT",
       label.left = "Favor Continuos",
       test.overall = TRUE,
       sortvar = TE)


# criando um funnel plot
funnel(treinamento,
       studlab = TRUE)

# teste de Egger
metabias(treinamento)

# teste de Begg e Mazumdar
metabias(treinamento, method.bias = "Begg")

# metodo trim-and-fill
trim <- trimfill(treinamento)

forest(trim)

funnel(trim)

