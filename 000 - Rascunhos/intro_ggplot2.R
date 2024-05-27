# Como Plotar Gráficos com o ggplot2?

# install.packages("ggplot2")

# trecho 1
library(ggplot2)
options(digits = 4)

# Define semente para reprodutibilidade
set.seed(42)
# Gera dados
dados <- data.frame(int = rnorm(n = 1000,
                                mean = 100,
                                sd = 15))

ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int))                                # coluna do data.frame usado no gráfico



# trecho 2
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int)) +                              # coluna do data.frame usado no gráfico
  geom_histogram()


# trecho 3
ggplot(data = dados,                            # nome do data.frame usado na plotagem
       aes(int)) +                              # coluna do data.frame usado no gráfico
  geom_histogram(binwidth = 3,                  # tamanho de cada intervalo
                 color = "black",               # cor das linhas das barras
                 fill = "lightblue"             # cor do preenchimento das barras
  ) + labs(x = "Escore de Inteligência",        # Título do eixo x
           y = "Frequência",                    # Título do eixo y
  ) +
  theme_classic()                               # Tema



# trecho 4
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




# trecho 5
set.seed(42)     

N <- 2000
x <- rnorm(n = N)
y <- c(x[1: (N / 2)] + rnorm(N / 2), 3 + x[(N / 2 + 1):N] + rnorm(N / 2))
grupo <- rep(c("Idosos", "Jovens Adultos"), each = N / 2)
data <- data.frame(x = x,
                   y = y, 
                   grupo = grupo)


ggplot(data, aes(x = x, y = y)) +                 # Scatterplot in ggplot2
  geom_point()

  
# trecho 6
ggplot(data, aes(x = x, y = y, color = grupo)) +
  geom_point(size = 2) +
  scale_color_manual(values = c("Jovens Adultos" = "slateblue",
                                "Idosos" = "violet")) +
  labs(color = "Grupo",
       x = "Variável Preditora",
       y = "Variável de Resultado") +
  theme_classic() +
  theme(
    # Formatação do título do eixo x
    axis.title.x = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
    # Formatação do título do eixo y
    axis.title.y = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
    
    legend.position = "bottom", 
    legend.title = element_blank())





# trecho 7
set.seed(42)
y <-  c(rgamma(n = 50, shape = 2, rate = 1),
        rnorm(n = 50, mean = 1, sd = 1))
x <- as.factor(c(rep("aeroporto", 50), rep("controle", 50)))

df <- data.frame(x, y)
colnames(df) <- c("grupo", "estresse")


# trecho 8
ggplot(data = df,
       aes(x = grupo, y = estresse)) +

  geom_errorbar(stat = "boxplot", width = 0.2) +
  geom_boxplot(color = "darkred",
               fill = "tomato") +
  stat_summary(fun = mean, shape = 15) + 
  labs(x = "Grupo",                             # Título do eixo x
       y = "Estresse",                          # Título do eixo y
  ) +
  theme_classic() +                             # Tema
  theme(
    # Formatação do título do eixo x
    axis.title.x = element_text(size = 14,      # Tamanho da fonte
                                face = "bold"), # Negrito
    # Formatação do título do eixo y
    axis.title.y = element_text(size = 14,      # Tamanho da fonte
                                face = "bold")  # Negrito

  ) 

# trecho 9
tapply(df$estresse, df$grupo, mean)
tapply(df$estresse, df$grupo, sd)
tapply(df$estresse, df$grupo, median)
t.test(df$estresse ~ df$grupo, var.equal = FALSE)


# trecho 10
df <- data.frame(
  tempo = rep(c("Pré-teste", "Pós-teste", "Follow-up"), 2),
  grupo = rep(c("Experimental", "Controle"), each = 3),
  desempenho = c(45, 60, 54, 46, 48, 47),
  MOE = c(3, 2, 2, 3, 3, 2))
df$tempo <- factor(df$tempo, levels = c("Pré-teste",
                                        "Pós-teste",
                                        "Follow-up"))

# trecho 11
ggplot(df, aes(x = tempo,
               y = desempenho,
               fill = grupo)) +
  geom_bar(stat = "identity",
           position = position_dodge(),
           width = 0.7,
           color = "black",        # Mudança de cor das bordas das barras
           size = 1) + 
  geom_errorbar(aes(ymin = desempenho - MOE,
                    ymax = desempenho + MOE),
                width = 0.2,
                position = position_dodge(0.7)) +
  labs(x = "Tempo",y = "Desempenho (%)") +
  scale_y_continuous(limits = c(0, 65)) +
  theme_classic() +
  theme(
    # Formatação do título do eixo x
    axis.title.x = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
    # Formatação do título do eixo y
    axis.title.y = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
  
    legend.position = "bottom", 
        legend.title = element_blank())


# trecho 12
ggplot(df, aes(x = tempo, y = desempenho, group = grupo)) +
  geom_point(aes(shape = grupo, color = grupo), size = 3) +  # Adiciona os pontos
  geom_line(aes(linetype = grupo, color = grupo), size = 1) +  # Adiciona as linhas
  geom_errorbar(aes(ymin = desempenho - MOE, ymax = desempenho + MOE),
                width = 0.2, color = "gray20") +
  scale_shape_manual(values = c(16, 15)) +  # Define os símbolos (círculo cheio para Group1 e quadrado cheio para Group2)
  scale_size_manual(values = c(8, 8)) + 
  labs(x = "Tempo", y = "Desempenho (%)") +  # Define os rótulos dos eixos
  scale_y_continuous(limits = c(40, 65)) +  # Define os limites do eixo y
  theme_minimal() +  # Define um tema minimalista
  theme(
    # Formatação do título do eixo x
    axis.title.x = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
    # Formatação do título do eixo y
    axis.title.y = element_text(size = 12,      # Tamanho da fonte
                                face = "bold"), # Negrito
    
    legend.position = "bottom", 
    legend.title = element_blank())



