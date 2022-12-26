# 012 - TRI Parametrica e Nao-Parametrica

### Limpa tudo da sessao
rm(list = ls()) # Limpa o Work Directory
dev.off()     # Limpa os graficos
cat("\014")   # Limpa o console

# definindo o diretorio de trabalho
path <- "C:/Users/User/Desktop/Python para Psicólogos/scripts-R/013 - TRI Thurstoniana/"
setwd(path)


# instalar e carregar os pacotes
install.packages(c("lavaan", "thurstonianIRT", "devtools"))
library(devtools)
remotes::install_github("felipevalentini/RecodeFCit")
library(thurstonianIRT)
library(RecodeFCit)


# carregue o banco no formato menos e mais caracteristico
banco <- RecodeFCit::dataErr
head(banco)

?RecodeFCit::dataErr

# verifique os empates (ties - marcacoes iguais para - e + caracteristico)
?checkTie()
ProbTies <- checkTie(banco, Cd= c("a","b","c")) #c(1,2,3) formato da digitacao dos blocos de itens (voce deve alterar conforme o seu banco)

# verifique os erros de digitacao (typos)
?checkTypo()
ProbDigit <- checkTypo(banco, Cd = c('a','b','c'))

# recodificar os erros de digitacao e empates como missing
?recodeErrors
banco2 <- recodeErrors(banco, Cd=c("a","b","c"))

# recodificar o banco de ranqueamento para comparacoes binarias
?recodeData
bancoMplus <- recodeData(banco2, Cd=c("a","b","c"))


# Se voce for utilizar o banco no Mplus, podes deixar com missing.
# No entanto, para o lavaan voce precisara excluir ou estimar os missings.
# Neste exemplo, irei excluir os casos com missings
bancoLavaan <- bancoMplus[-c(1,2),]
bancoLavaan <- as.data.frame(sapply(bancoLavaan, as.numeric)) #converter todos itens para numerico, se necess?rio



blocks <-
  set_block(c("i1", "i2", "i3"), traits = c("t1", "t2", "t3"),
            signs = c(-1, 1, 1)) +
  set_block(c("i4", "i5", "i6"), traits = c("t1", "t2", "t4"),
            signs = c(1, -1, 1)) +
  set_block(c("i7", "i8", "i9"), traits = c("t1", "t2", "t5"),
            signs = c(1, 1, -1)) +
  set_block(c("i10", "i11", "i12"), traits = c("t1", "t3", "t4"),
            signs = c(1, 1, -1)) +
  set_block(c("i13", "i14", "i15"), traits = c("t1", "t3", "t5"),
            signs = c(1, -1, 1)) +
  set_block(c("i16", "i17", "i18"), traits = c("t1", "t4", "t5"),
            signs = c(-1, 1, 1)) +
  set_block(c("i19", "i20", "i21"), traits = c("t2", "t3", "t4"),
            signs = c(1, -1, 1)) +
  set_block(c("i22", "i23", "i24"), traits = c("t2", "t3", "t5"),
            signs = c(-1, 1, 1)) +
  set_block(c("i25", "i26", "i27"), traits = c("t2", "t4", "t5"),
            signs = c(1, -1, 1)) +
  set_block(c("i28", "i29", "i30"), traits = c("t3", "t4", "t5"),
            signs = c(1, 1, -1))



# gerar o codigo para o banco ser compreendido 'thurstonianIRT'
#simplificado
triplets_long <- make_TIRT_data(
  data = bancoLavaan, 
  blocks = blocks,
  format = "pairwise")


# rodar o modelo no Lavaan
?fit_TIRT_lavaan
fit <- fit_TIRT_lavaan(triplets_long, estimator = "ULSMV")
print(fit)
summary(fit)

#salvar escore fatorial
bancoEF<-predict(fit)


# rodar o modelo no Mplus 
# ? necess?rio ter o Mplus instalado no computador (fora do R)
install.packages("MplusAutomation")
library(MplusAutomation)
fit2 <- fit_TIRT_mplus(triplets_long)
summary(fit2)
summaries(fit2)
# recuperar indicadores de ajuste
fit2[fit[results[summaries]]]
fit2[["fit"]][["results"]][["summaries"]]
fit2[["fit"]][["results"]][["summaries"]] ["CFI"]
fit2[["fit"]][["results"]][["summaries"]] ["TLI"]
fit2[["fit"]][["results"]][["summaries"]] ["RMSEA_Estimate"]
fit2[["fit"]][["results"]][["summaries"]] ["RMSEA_90CI_LB"]
fit2[["fit"]][["results"]][["summaries"]] ["RMSEA_90CI_UB"]





### caso queira rodar o modelo no MPLUS ou lavan (fora do pacote ThusrtonianIRT)
# voce precisar? do pacote dplyr para rodar essa parte:
install.packages("dplyr")
library(dplyr)

### gerar os codigos para rodar o modelo no lavaan e mplus
# Mplus
TRIT_cod_Mplus<-make_mplus_code(triplets_long) %>% lapply(cat)


#Lavaan
TRIT_cod_lavaan<-make_lavaan_code(triplets_long)


