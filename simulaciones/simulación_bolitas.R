# Simulación del experimento a llevar a cabo con las bolitas ####
library(tidyverse)

## Creamos las bolsas ####
N <- 120
MU <- 12
SD <- 4

set.seed(42)
bolsa <- tibble(color = c(rep("blanca", N*.6), rep("negra", N*.4)),
                peso = rnorm(n = N, mean = MU, sd = SD))

bolsa %>%
  ggplot(aes(x = peso)) +
  geom_histogram(aes(y = after_stat(density)), position="identity", alpha = .4, binwidth = 1) +
  stat_function(fun = dnorm, n = 101, args = list(mean = MU, sd = SD), color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = MU, color = "darkblue", linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

## Tomo las muestras ####
# Simulado a mano
# n_grupos <- 35
# 
# muestras <- tibble(grupo = numeric(),
#                    n = numeric(),
#                    p = numeric(),
#                    peso = numeric(),
#                    sd = numeric())
# 
# set.seed(42)
# for (i in 1:n_grupos) {
#   for (j in c(3, 6, 12)) {
#     sample <- sample_n(bolsa, j, replace = F)
#     sample_stat <- tibble(grupo = i,
#                           n = j,
#                           p = mean(sample$color == "blanca"),
#                           peso = mean(sample$peso),
#                           sd = sd(sample$peso))
#     sample_stat
#     muestras <- muestras %>%
#       bind_rows(sample_stat)
#   }
# }
# head(muestras)

## Las proporciones de color ####
muestras %>%
  group_by(n) %>%
  summarise(p = mean(p))

muestras %>%
  ggplot(aes(x = factor(n), y = p)) +
  geom_violin(fill = "gray80", alpha = .2) +
  geom_jitter(width = .2) +
  #geom_line(data = densidades, aes(x = x, y = densidad), color = "black", size = 1) +
  geom_hline(yintercept = .6, color = "darkred", size = 1) + 
  labs(x = 'n', y = 'Proporción de blancas') +
  scale_y_continuous(breaks = seq(0, 1, .1)) +
  theme_minimal() +
  theme(legend.position = "none")

n_elegido <- 12

CIs_p <- muestras %>% 
  filter(n == n_elegido) %>%
  ungroup() %>%
  summarise(p_hat = mean(p),
            p_low = p_hat - qnorm(.975)/sqrt(n_elegido) * sqrt(p_hat * (1-p_hat)),
            p_high = p_hat + qnorm(.975)/sqrt(n_elegido) * sqrt(p_hat * (1-p_hat)),)

muestras %>% 
  ungroup() %>%
  filter(n == n_elegido) %>%
  mutate(is_in_CI = between(p, CIs_p$p_low, CIs_p$p_high)) %>%
  summarise(cobertura = mean(is_in_CI))

## Los pesos ####
muestras %>%
  group_by(n) %>%
  summarise(m_peso = mean(peso),
            sd_peso = sd(peso))

densidades <- expand_grid(x = seq(6, 18, .1), n = c(3, 6, 12)) %>%
  mutate(densidad = dnorm(x = x, mean = MU, sd = SD/sqrt(n)))

muestras %>%
  ggplot(aes(x = peso)) +
  geom_histogram(aes(y = after_stat(density)), fill = "darkred", position="identity", alpha = .6, binwidth = .5) +
  geom_line(data = densidades, aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = MU, color = "black", size = 1) +
  labs(x = 'Peso (g)', y = NULL) +
  facet_grid(n~., labeller = label_both) +
  theme_minimal() +
  theme(legend.position = "none")

n_elegido <- 12

CIs_mu <- muestras %>% 
  filter(n == n_elegido) %>%
  ungroup() %>%
  summarise(mu_hat = mean(peso),
            mu_low = mu_hat - qnorm(.975)/sqrt(n_elegido) * SD,
            mu_high = mu_hat + qnorm(.975)/sqrt(n_elegido) * SD)

muestras %>% 
  ungroup() %>%
  filter(n == n_elegido) %>%
  mutate(is_in_CI = between(peso, CIs_mu$mu_low, CIs_mu$mu_high)) %>%
  summarise(cobertura = mean(is_in_CI))


# H0: mu = 10
## Y si H0 fuera verdadera? ####
densidades <- expand_grid(x = seq(4, 16, .1), n = c(3, 6, 12)) %>%
  mutate(densidad = dnorm(x = x, mean = 10, sd = SD/sqrt(n)))

muestras %>%
  ggplot(aes(x = peso-2)) +
  geom_histogram(aes(y = after_stat(density)), fill = "darkred", position="identity", alpha = .6, binwidth = .5) +
  geom_line(data = densidades, aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = 10, color = "black", size = 1) +
  facet_grid(n~.) +
  theme_minimal() +
  theme(legend.position = "none")

## Tomemos una muestra de tamano n y calculemos la p de haberla tomado dado que H0 es verdadera ####
### n = 3 ####
set.seed(21)
muestra_3 <- sample(bolsa$peso, 3, replace = F) 

mean_3 <- mean(muestra_3)

muestras_3 <- muestras %>%
  filter(n == 3) 

muestras_3 %>%
  ggplot(aes(x = peso-2)) +
  geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  geom_line(data = densidades %>% filter(n == 3), aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = 10, color = "black", size = 1) +
  geom_vline(xintercept = mean_3, color = "darkred", size = 1) +
  theme_minimal() +
  theme(legend.position = "none")

sum(mean_3 < muestras_3$peso-2) / nrow(muestras_3)
t.test(muestra_3-10)

### n = 6 ####
set.seed(21)
muestra_6 <- sample(bolsa$peso, 6, replace = F) 

mean_6 <- mean(muestra_6)

muestras_6 <- muestras %>%
  filter(n == 6) 

muestras_6 %>%
  ggplot(aes(x = peso-2)) +
  geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  geom_line(data = densidades %>% filter(n == 6), aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = 10, color = "black", size = 1) +
  geom_vline(xintercept = mean_6, color = "darkred", size = 1) +
  labs(x = 'Peso-2 (g)', y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

sum(mean_6 < muestras_6$peso-2) / nrow(muestras_6)
t.test(muestra_6-10)

### n = 12 ####
set.seed(10)
muestra_12 <- sample(bolsa$peso, 12, replace = F) 

mean_12 <- mean(muestra_12)

muestras_12 <- muestras %>%
  filter(n == 12) 

muestras_12 %>%
  ggplot(aes(x = peso-2)) +
  #geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  geom_line(data = densidades %>% filter(n == 12), aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = 10, color = "black", size = 1) +
  geom_vline(xintercept = mean_12, color = "darkred", size = 1) +
  scale_x_continuous(limits = c(4,16)) +
  labs(x = 'Peso-2 (g)', y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# Distribución normalizada
muestras_12 %>%
  ggplot(aes(x = peso-2)) +
  #geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd =1), color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_vline(xintercept = (mean_12-10)/4*sqrt(12), color = "darkred", size = 1) +
  scale_x_continuous(limits = c(-4,4)) +
  labs(x = '(mean(Peso)-10)/(4*sqrt(12))', y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# Distribución t n-1
muestras_12 %>%
  ggplot(aes(x = peso-2)) +
  #geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  stat_function(fun = dt, n = 101, args = list(df=11), color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_vline(xintercept = (mean_12-10)/sd(muestra_12)*sqrt(12), color = "darkred", size = 1) +
  scale_x_continuous(limits = c(-4,4)) +
  labs(x = '(mean(Peso)-10)/(sd(peso)*sqrt(12))', y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

# Bidireccional
muestras_12 %>%
  ggplot(aes(x = peso-2)) +
  #geom_histogram(aes(y = after_stat(density)), fill = "black", position="identity", alpha = .6, binwidth = .5) +
  stat_function(fun = dt, n = 101, args = list(df=11), color = "darkblue", linewidth = 1) +
  geom_vline(xintercept = 0, color = "black", size = 1) +
  geom_vline(xintercept = c(1,-1) * (mean_12-10)/sd(muestra_12)*sqrt(12), color = "darkred", size = 1) +
  scale_x_continuous(limits = c(-4,4)) +
  labs(x = '(mean(Peso)-10)/(sd(peso)*sqrt(12))', y = NULL) +
  theme_minimal() +
  theme(legend.position = "none")


# La probabilidad de observar a la muestra dado que H0 es verdaera
sum(mean_12 < muestras_12$peso-2) / nrow(muestras_12)

# La distribución nula en este caso tiene mu = 10 y sd = 4/sqrt(12) = 1.154701
1 - pnorm(q = mean_12, mean = 10, sd = 4/sqrt(12))

mean_12_norm <- (mean(muestra_12) - 10)/(4/sqrt(12))
1 - pnorm(q = mean_12_norm)

t.test(muestra_12, mu = 10, alternative = "greater")
mean_12_norm <- (mean(muestra_12) - 10)/(sd(muestra_12)/sqrt(12))
1 - pt(q = mean_12_norm, df = 11)

t.test(muestra_12, mu = 10)
mean_12_norm <- (mean(muestra_12) - 10)/(sd(muestra_12)/sqrt(12))
2*(1 - pt(q = mean_12_norm, df = 11))
