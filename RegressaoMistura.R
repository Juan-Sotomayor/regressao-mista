#Criando banco de dados sintético

library(dplyr)

set.seed(42)
n <- 100
data <- data.frame(
  HorasEstudo = runif(n, 1, 10),
  Programa = sample(c("Programa A", "Programa B"), n, replace = TRUE)
)

#Gerando NotaProva com Paradoxo de Simpson

data <- data %>% 
  mutate(
    NotaProva = ifelse(
      Programa == "Programa A",
      90 - 3 * HorasEstudo + rnorm(n, 0, 5), #Relação neg no 'Programa A'
      50 + 2 * HorasEstudo + rnorm(n, 0, 5)  #Relação pos no 'Programa B'
    )
  )
# A subpopulação é latente, ou seja, não é observada

#Benchmark - Modelagem inicial

fit <- lm(NotaProva ~ HorasEstudo, data=data)

plot(data$HorasEstudo, data$NotaProva,
     main = "Regressão Linear Simples",
     xlab = "Horas de Estudo",
     ylab = "Nota da Prova",
     pch = 19, col = "darkgray")

abline(fit, col = "blue", lwd = 2)

summary(fit)

#Modelo ruim com métricas péssimas no diagnóstico
#Quase nenhuma associação entre Horas de Estudo e Nota da Prova

# Modelo Mistura de Regressão

library(flexmix)

# Testando para numero de componentes candidatas. (De 1 a 5)
fittedModel1_1_5_c <- stepFlexmix(NotaProva ~ HorasEstudo, k = c(1,2,3,4,5),
                                  nrep = 10, data = data)
fittedModel1_1_5_c
plot(fittedModel1_1_5_c)

# Melhor modelo: k = 2 grupos, pelo BIC e ICL
best_model <- getModel(fittedModel1_1_5_c, which=2)
summary(best_model)
summary(refit(best_model))

parameters(best_model)

#Parametros bem próximos do original

plot(best_model)

#Visualização, qual observação pertence a qual grupo

posterior(best_model)
clusters(best_model)

# Adicionar os clusters designados ao df
data_cluster <- data
data_cluster$cluster <- factor(clusters(best_model))
data_cluster <- data_cluster %>%
  mutate(cluster = recode(clusters(best_model),
                          `1` = "Programa A",
                          `2` = "Programa B"))

# Como o modelo "classificou"

library(ggplot2)

ggplot(data_cluster, aes(x = HorasEstudo, y = NotaProva, color = cluster)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Regressões Lineares por Programa",
    x = "Horas de Estudo",
    y = "Nota da Prova",
    color = "Programa"
  ) +
  theme_minimal()

ggplot(data_cluster, aes(x = HorasEstudo, y = NotaProva, color = Programa)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Regressões Lineares por Programa Original",
    x = "Horas de Estudo",
    y = "Nota da Prova",
    color = "Programa"
  ) +
  theme_minimal()

# Taxa de acertos
mean(data_cluster$cluster == data_cluster$Programa) * 100
# 87%

# Essa visualização pode induzir ao erro do propósito do modelo.
# A intenção dele é a regressão, e não classificação.
# Essa parte ainda será elaborada futuramente