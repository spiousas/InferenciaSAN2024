pacman::p_load(tidyverse)

# Generamos una muestra 
set.seed(123)
x <- rnorm(20, mean = 1, sd = 3)

# Su histograma con una función base de R
hist(x)

# Y si nos ponemos chetardos con ggplot2
tibble(x) %>%
  ggplot(aes(x = x, y = ..density..)) +
  geom_histogram(fill = "steelblue", color = "white", binwidth = 2) +
  theme_bw()

# La media y la sd muestral     
mean(x)
sd(x)

# La distribución de las medias después de tomar 104 muestras
means <- c()
n <- 20
mu <- 1
sigma <- 3

set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(n, mean = mu, sd = sigma)
  means <- c(means, mean(x_i))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()

# La distribución de las medias normalizadas cuando H0 es verdadera
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(20, mean = 0, sd = 3)
  Zs <- c(Zs, (mean(x_i))/sqrt(3^2/20))
}

Z_means <- tibble(Zs)
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# Pongo la muestra x en la distribución cuando H0 es verdadera
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# Y también grafico los valores críticos
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * 1.96, linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  theme_bw()

# El p valor con sigma conocido
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

# La distribución t, cuando el sigma no es conocido
# primero con n=6
norm_mean <- c()
n <- 20
mu <- 1
sigma <- 3
set.seed(123)
for (i in 1:10000) {
  x_i <- rnorm(n, mean = mu, sd = sigma)
  norm_mean <- c(norm_mean, (mean(x_i)-mu)/sqrt(sd(x)^2/n))
}

# La distribución comparada con la normal (son casi iguales para este n)
norm_means <- tibble(norm_mean)
norm_means %>% ggplot() +
  geom_histogram(aes(x = norm_mean, y = ..density..), fill = "steelblue", alpha = .4) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = n-1), 
                color = "red", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

# Veamos para n = 6 y n = 20 comparada con la normal
# normal -> negro
# t con n=6 -> rojo
# t con n=20 -> verde
norm_means %>% ggplot() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 5), 
                color = "red", linewidth = 1) +
  stat_function(fun = dt, args = list(df = 19), 
                color = "darkgreen", linewidth = 1) +
  scale_x_continuous(limits = c(-5,5)) +
  theme_bw()

# La distribución de los p-valores cuando H0 es verdadera
ps <- c()
set.seed(123)
for (i in 1:1E4) {
  x <- rnorm(20, mean = 0, sd = 3)
  p <- 2*pnorm(-abs(mean(x)/sqrt(3^2/20)))
  ps <- c(ps, p)
}

p_values <- tibble(ps)
p_values %>% ggplot(aes(x = ps)) +
  geom_histogram(fill = "steelblue", color = "white", 
                 binwidth = .05, boundary = .05) +
  theme_bw()


# La distribución de los p-valores cuando H0 es falsa
ps <- c()
set.seed(123)
for (i in 1:1E4) {
  x <- rnorm(20, mean = 1, sd = 3)
  p <- 2*pnorm(-abs(mean(x)/sqrt(3^2/20)))
  ps <- c(ps, p)
}

p_values <- tibble(ps)
p_values %>% ggplot(aes(x = ps)) +
  geom_histogram(fill = "steelblue", color = "white", 
                 binwidth = .05, boundary = .05) +
  theme_bw()

# El p valor con t y normal
set.seed(123)
x <- rnorm(20, mean = 1, sd = 3) # La misma muestra que al principio

# Sigma conocido
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

# Sigma desconocido
2*pt(-abs(mean(x)/sqrt(sd(x)^2/20)), df = 19)


# Una muestra que es un falso negativo
set.seed(12)
x <- rnorm(20, mean = 1, sd = 3)
mean(x)
2*pnorm(-abs(mean(x)/sqrt(3^2/20)))

Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue", alpha = .4) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = c(1, -1) * mean(x)/sqrt(9/20), linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(1, -1) * qnorm(0.05/2), linetype = "dashed", color = "blue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  stat_function(fun = dnorm, args = list(mean = 1/sqrt(9/20), sd = 1), 
                color = "red", linewidth = 1) +
  theme_bw()

# La potencia
1 + pnorm(qnorm(.05/2)-1/(sqrt(3^2/20))) -
  pnorm(qnorm(1-.05/2)-1/(sqrt(3^2/20)))

# Creamos una función para calcular la potencia
potencia <- function(sigma, mu, n, alpha) {
  pnorm(qnorm(alpha/2)-mu/(sqrt(sigma^2/n))) + 1 -
    pnorm(qnorm(1-alpha/2)-mu/(sqrt(sigma^2/n)))
}

# La potencia en función de mu
pot <- potencia(3,seq(-3,3,.1),20,0.05)
pot_tbl <- tibble(mu = seq(-3,3,.1)) %>%
  mutate(potencia = potencia(3,mu,20,0.05))

pot_tbl %>% ggplot(aes(x = mu,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

# La potencia en función de alpha
pot_tbl <- tibble(alpha = seq(0, 0.05, 0.001)) %>%
  mutate(potencia = potencia(3,1,20,alpha))

pot_tbl %>% ggplot(aes(x = alpha,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()

# La potencia en función de n
pot_tbl <- tibble(n = seq(5, 200)) %>%
  mutate(potencia = potencia(3,1,n,.05))

pot_tbl %>% ggplot(aes(x = n,
                       y = potencia)) +
  geom_line(linewidth = 1) +
  theme_bw()


# Effect size
set.seed(123)
u <- rnorm(20000, mean = .1, sd = 3)

t.test(u)

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(20000, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 20000") +
  theme_bw()

means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rnorm(100, mean = .1, sd = 3)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "n = 100") +
  theme_bw()

# Qué pasa cuando no hay normalidad (distribución de las medias de una exponencial)

# Empecemos con n=100
# Las Xs
set.seed(123)
n <- 100
Xs <- tibble(x = rexp(n,rate = 1))

Xs %>% ggplot() +
  geom_histogram(aes(x = x, y = ..density..), fill = "steelblue") +
  stat_function(fun = dexp, args = list(rate =1), 
                color = "black", linewidth = 1) +
  labs(x = "V") +
  theme_bw()

mean(x)
sd(x)

# Las medias para n=100
means <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(n,rate = 1)
  means <- c(means, mean(x))
}

x_means <- tibble(means)
x_means %>% ggplot(aes(x = means, y = ..density..)) +
  geom_histogram(fill = "steelblue") +
  theme_bw()

# Las medias para n=1000
n = 1000
Zs <- c()
set.seed(123)
for (i in 1:10000) {
  x <- rexp(n, rate = 1)
  Zs <- c(Zs, (mean(x) - 1)/sqrt(sd(x)^2/n))
}

Z_means <- tibble(Zs)
Z_means %>% ggplot() +
  geom_histogram(aes(x = Zs, y = ..density..), fill = "steelblue") +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), 
                color = "black", linewidth = 1) +
  labs(title = paste("n = ", n), x = "Medias de v") +
  scale_x_continuous(limits = c(-4, 4)) +
  theme_bw()
