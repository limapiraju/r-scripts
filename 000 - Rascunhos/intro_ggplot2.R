####################################
#       VISUALIZAÇÃO DE DADOS
#           COM GGPLOT2
####################################

# - ggplot2: segue o grammar-of-graphics framework, que resulta nos
# seguintes benefícios: 
# 1. Estruturas e regras consistentes para qualquer visualização;
# 2. Construção de visualização por camadas;
# 3. Visualizações não restritas ao "tipo" de gráfico
# (e.g., dispersão);
# 
# - camadas essenciais dos gráficos:
# 1. Dados: onde acessar informações;
# 2. Mapeamento estético: atribuir variáveis aos elementos visuais;
# 3. Representação geométrica: como representar as informações;
# - e.g., ggplot(data = xxx, mapping = aes()) + geom_point()
# - alguns aesthetics, aes(), incluem x = o que vai no eixo x;
# y = o que vai no eixo y; size = ???, color = ???, shape = ???;

setwd("C:/Users/limap/OneDrive/Área de Trabalho/Psicometria Online Academy/Blog/Scripts/")


library(tidyverse)
library(haven)

df <- read_sav("Dados RegLog.sav") %>% 
  as_factor()


# Dispersão

df %>% 
  ggplot(mapping = aes(x = NEURO, y = FELIC)) +
  geom_point()

# Plotagem alternativa
# mapping passado direto ao geom_point()
# neste caso, aplica-se somente a essa camada
df %>% 
  ggplot() +
  geom_point(mapping = aes(x = NEURO, y = FELIC))

# Adicionando uma camada: linha de regressão
df %>% 
  ggplot(mapping = aes(x = NEURO, y = FELIC)) +
  geom_point() +
  geom_smooth(method = "lm") 

# no código abaixo, os nomes mapping, x e y foram omitidos
# pois o banco de dados foi passado antes do %>%
df %>% 
  ggplot(aes(NEURO, FELIC)) +
  geom_point() +
  geom_smooth(method = "lm") 

## Alterando cor, formato e tamanho

# Se a aparência for associada a uma variável, 
# o comando deve ser passado DENTRO da função aes().
df %>% 
  ggplot() +
  geom_point(mapping = aes(x = NEURO,
                           y = FELIC, 
                           color = Sexo, 
                           shape = Filhos,
                           size = Idade
                           )
             )

# Se a aparência receber algum outro valor, 
# o argumento deve ser passado FORA da função aes().

df %>% 
  ggplot(mapping = aes(x = NEURO, y = FELIC)) +
  geom_point(color = "blue", 
             shape = 15,
             size = 2
             )

df %>% 
  ggplot(mapping = aes(x= NEURO, y = FELIC)) +
  geom_point(color = "#D745D2",
             shape = "diamond",
             size = 5
             )

# Links úteis
# Cores do R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# Código RGB de qualquer cor: https://www.rapidtables.com/web/color/RGB_Color.html
# Lista de formatos: http://www.sthda.com/english/wiki/r-plot-pch-symbols-the-different-point-shapes-available-in-r

#### Alterando legendas e background 

disper <- df %>% 
  ggplot(aes(x = NEURO,
             y = FELIC,
             color = Sexo)) +
  geom_point(size = 2.25)
disper

# A função labs altera o nome dos eixos, legendas e
# permite adicionar título, subtítulo e notas.
disper +
  labs(x = "Neuroticismo",
       y = "Felicidade", 
       color = "Gênero",
       title = "Gráfico 1", 
       subtitle = "Relação de Neuroticismo com Felicidade",
       caption = "n = 590"
       ) +
  theme_classic()

# O tema se refere a todos os aspectos visuais que não dependem
# dos dados. As cores, tamanhos e textos dos eixos e background 
# fazem parte do tema.

disper + theme_bw()
disper + theme_classic()
disper + theme_dark()
disper + theme_linedraw()
disper + theme_minimal()


disper +
  labs(x = "Neuroticismo",
       y = "Felicidade", 
       color = "Gênero", 
       title = "Gráfico 1", 
       subtitle = "Relação de Neuroticismo com Felicidade",
       caption = "n= 590"
  ) +
  theme_classic() 


## A função theme() permite alterar os outros aspectos visuas do gráfico
?theme()
# element_line()
# element_rect()
# element_text()

# brincando com as funções
disper +
  labs(x = "Neuroticismo",
       y = "Felicidade",
       color = "Gênero",
       title = "Gráfico 1",
  ) +
  theme_classic() +
  theme(plot.title = element_text(face= "bold", size = 15),
        axis.title = element_text(size = 13),
        legend.position = "top", 
        axis.line = element_line(linewidth = 1.5,
                                 color = "lightgrey"),
        panel.backgroun = element_rect(fill = "green",
                                       linewidth = 3,
                                       color = "red"),
        plot.background = element_rect(fill = "blue",
                                       linewidth = 5,
                                       color = "black"),
        legend.background = element_rect(fill = "orange",
                                         linewidth = 4,
                                         color = "magenta")
  )




disper +
  labs(x = "Neuroticismo",
       y = "Felicidade",
       color = "Gênero",
       title = "Gráfico 1",
  ) +
  theme_classic() +
  theme(plot.title = element_text(face= "bold", size = 15),
        axis.title = element_text(size = 13),
        legend.position = "top", 
        axis.line = element_line(linewidth = 1.5,
                                 color = "lightgrey")
                )



disper +
  labs(x= "Neuroticismo", y= "Felicidade",
       color="Gênero",
       title = "Meu primeiro ggplot *_*",
  ) +
  theme_classic() +
  theme(plot.title = element_text(face= "bold", size = 15),
        axis.title = element_text(size = 13),
        legend.position = "top",
        axis.line = element_line(linewidth = 0.5, color = "black"),
        legend.text = element_text(family = "sans", size = 10)
        
  )

### Salvando o gráfico

# Por default, a função salva o último gráfico reproduzido
ggsave(# plot =   # default é o último gráfico gerado
  filename = "Gráfico dispersão.png", 
  # device = ,  # formato do arquivo
  # width = 12,
  # height = 8,
  units = "px",
  dpi = 300
  )




####################################
#       VISUALIZAÇÃO DE DADOS
#           COM GGPLOT2
####################################

library(tidyverse)
library(haven)

df <- read_sav("Dados RegLog.sav") %>% 
  as_factor()


# Para estabelecer um tema para todos os gráficos 
# vamos utilizar a função theme_set()
theme_set(theme_classic() + 
            theme(legend.position = "top"))


## Gráficos univariados

# Histograma

df %>% 
  ggplot(aes(EXT)) +
  geom_histogram(binwidth = 1, # Largura de cada bin
                 # bins = 10   # Número de bins
                 color = "black",
                 fill = "grey85"
  )

# Comparando diferentes subgrupos
# por cor, usando mapping = aes()
df %>% 
  ggplot(aes(EXT)) +
  geom_histogram(binwidth = 1,  
                 mapping = aes(fill = Filhos),
                 color = "white",
                 position = "identity", # default: stack
                 alpha = 0.5  # Transparência
  ) + 
  scale_fill_manual(values = c("Não" = "red",
                               "Sim" = "blue"
                               )
  )


# por gráfico (facet_wrap)
df %>% 
  ggplot(aes(EXT, fill= Filhos)) +
  geom_histogram(binwidth = 1, 
                 color= "white",
                 alpha = .9) +
  facet_wrap(~ Filhos, # segue o formato fórmula
              labeller = label_both
  ) + 
  scale_fill_manual(values = c("Não" = "red",
                               "Sim" = "blue"
                               )
                    )


### Histogramas para diversas variáveis

df %>% 
  pivot_longer(cols = c(EXT:CRISE)) %>% 
  ggplot(aes(value)) +
  geom_histogram( binwidth = 1,
                  color = "white",
                  fill = "gray15") +
  facet_wrap(~ name)

# Como alterar o nome do strip.text
# 1. Crie um vetor com os nomes que serão plotados
# 2. Nomeie este vetor com os nomes das variáveis no banco
# 3. Na função facet_wrap, utilize o argumento e função labeller
# para associar a variável ao novo vetor

big5 <- c("Extroversão", "Socialização", "Conscienciosidade", 
          "Neuroticismo", "Abertura à Experiência", "Crise")
names(big5) <- names(df[5:10])


df %>% 
  pivot_longer(cols = c(EXT:CRISE)) %>% 
  ggplot(aes(value, fill = name)) + # fill = name: cada faceta terá uma cor
  geom_histogram( binwidth = 1,
                  color = "white") +
  facet_wrap(~ name,
             labeller = labeller(name = big5))  + # define rótulos
  theme(strip.background = element_rect(size = 1.2,
                                        fill = "lightblue"), # tamanho do retângulo de título de painel
        strip.text = element_text(color= "black", size = 11), # tamanho do texto do título de painel
        axis.title.x = element_blank(), # remove título do eixo x
        legend.position = "none" # remove legenda
  )



# Densidade
df %>% 
  ggplot(aes(EXT))+
  geom_density()

df %>% 
  ggplot(aes(x = EXT,
             color = Filhos,
             fill = Filhos))+
  geom_density(size = 1, 
               alpha = 0.5
  )

# Unindo o histograma com gráfico de densidade

df %>% 
  ggplot(aes(EXT)) +
  geom_histogram(binwidth = 1,
                 color = "white",
                 fill = "gray60") +
  geom_density(aes(y= ..count..), # ..count.. escala densidade na unidade de contagem
               size = 0.8
  )







# Boxplot (Gráfico de caixa e bigodes)

df %>% 
  ggplot(aes(y = ABEXP)) +
  geom_boxplot() +
  xlim(-1, 1) 

# Como interpretar o boxplot
# - A linha no meio da caixa representa a mediana
# - As bordas da caixa representam o primeiro e terceiro quartil
# - A distância interquartil (tamanho da caixa) é uma medida de dispersão
# - As linhas fora da caixa assumem um tamanho máximo de 1.5 vezes 
#   a distância interquartil. Ou até os valores mínimo e máximo.
# - Observações além de +-1.5 da distância interquartil são 
#   consideradas outliers e representadas por pontos.


# O boxplot é muito útil para comparar grupos

df %>% 
  ggplot(aes(x = Filhos, y = ABEXP)) +
  geom_boxplot()


# O boxplot pode ser combinado com outros elementos gráficos.
# Pontos
df %>% 
  ggplot(aes(x = Filhos, y = ABEXP)) +
  geom_jitter(width = 0.35,
              height = 0, 
              shape = 21,
              alpha = 0.5,
              size = 2) +
  geom_boxplot(color = "brown",
               fill = "gray80",
               notch = TRUE,
               outlier.shape = NA)

# Barras de erro
df %>% 
  ggplot(aes(x = Filhos, y = ABEXP)) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot()

# Histograma
df %>% 
  ggplot(aes(ABEXP)) +
  geom_histogram(binwidth = 1, color= "black", fill= "gray90") +
  geom_boxplot(width= 8, size= 0.9)


# Reunindo o que já vimos:
df %>% 
  pivot_longer(cols = c("EXT": "ABEXP")) %>% 
  ggplot(aes(value)) +
  geom_histogram(binwidth = 1, color= "black", fill= "gray90") +
  geom_boxplot(width= 8, size= 0.9) +
  geom_density(aes(y= ..count..), size= 0.5) +
  facet_wrap(~name, labeller = labeller(name = big5))




# Post blog R
options(digits = 4)
# Define semente para reprodutibilidade do código
set.seed(42)
# Gera 1.000 observações de uma distribuição com M = 100 e DP = 15
dados <- data.frame(int = rnorm(n = 1000, mean = 100, sd = 15))

# caso 1
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int))                               # coluna do data.frame usado no gráfico



# caso 2
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int)) +                              # coluna do data.frame usado no gráfico
  geom_histogram()


# caso 3
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int)) +                              # coluna do data.frame usado no gráfico
  geom_histogram(binwidth = 3,                  # tamanho de cada intervalo
                 color = "black",               # cor das linhas das barras
                 fill = "lightblue"             # cor do preenchimento das barras
  ) + labs(x = "Escore de Inteligência",        # Título do eixo x
           y = "Frequência",                    # Título do eixo y
  ) +
  theme_classic()                               # Tema



# caso 4
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int)) +                              # coluna do data.frame usado no gráfico
  geom_histogram(binwidth = 3,                  # tamanho de cada intervalo
                 color = "black",               # cor das linhas das barras
                 fill = "lightblue"             # cor do preenchimento das barras
  ) + labs(x = "Escore de Inteligência",        # Título do eixo x
        y = "Frequência",                       # Título do eixo y
  ) +
  theme_classic() +                             # Tema
  theme(
    # Formatação do título do eixo x
    axis.title.x = element_text(size = 14,      # Tamanho da fonte
                                face = "bold"), # Negrito
    # Formatação do título do eixo y
    axis.title.y = element_text(size = 14,      # Tamanho da fonte
                                face = "bold")  # Negrito
  ) + 
  # Número de quebras nos intervalos do eixo x
  scale_x_continuous(n.breaks = 10)


set.seed(42)
y <-  c(rgamma(n = 50, shape = 2, rate = 1),
        rnorm(n = 50, mean = 1, sd = 1))
x <- as.factor(c(rep("aeroporto", 50), rep("controle", 50)))

df <- data.frame(x, y)
tapply(df$y, df$x, mean)
tapply(df$y, df$x, sd)


ggplot(data = df,
       aes(x = x, y = y)) +
  geom_jitter(width = 0.35,
              height = 0, 
              shape = 21,
              alpha = 0.5,
              size = 2) +
  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(color = "darkblue",
               fill = "lightblue")




