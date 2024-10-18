# Cargar las librerías necesarias
library(tidyverse)
library(modelsummary)

# Fijar la semilla para la reproducibilidad
set.seed(123)

# Parámetros
n <- 120  # tamaño de la muestra
beta0 <- 2  # intercepto
beta1 <- 1.5  # pendiente
set.seed(123)
data <- tibble(x = rnorm(n))  # variable independiente

# Generar datos con residuos normales
set.seed(123)
hist(rnorm(n, mean = 0, sd = 1))

set.seed(123)
data <- data %>%
  mutate(y_norm = beta0 + beta1 * x + rnorm(n, mean = 0, sd = 1))

data %>% 
  ggplot(aes(x = x,
             y = y_norm)) +
  geom_smooth(method = lm, se = F) +
  geom_point() +
  theme_minimal()
  
# Generar datos con residuos no normales (distribución con colas pesadas)
set.seed(123)
hist(runif(n, min = -2, max = 2))

set.seed(123)
data <- data %>%
  mutate(y_unif = beta0 + beta1 * x + runif(n, min = -2, max = 2)) # distribución t con df = 2 (colas pesadas)

data %>% 
  ggplot(aes(x = x,
             y = y_unif)) +
  geom_smooth(method = lm, se = F) +
  geom_point() +
  theme_minimal()

# Generar datos con residuos sesgados (ej., distribución sesgada a la derecha)
set.seed(123)
hist(rlnorm(n, meanlog = 0, sdlog = 0.5) - 1)

set.seed(123)
data <- data %>%
  mutate(y_skewed = beta0 + beta1 * x + rlnorm(n, meanlog = 0, sdlog = 0.5) - 1) # distribución t con df = 2 (colas pesadas)

data %>% 
  ggplot(aes(x = x,
             y = y_skewed)) +
  geom_smooth(method = lm, se = F) +
  geom_point() +
  theme_minimal()

# Ajustar modelos lineales
lm_norm <- lm(y_norm ~ x, data)
lm_unif <- lm(y_unif ~ x, data)
lm_skewed <- lm(y_skewed ~ x, data)

# Resumen de los modelosVos 
summary(lm_norm)  # Residuos normales
summary(lm_unif)  # Residuos uniformes
summary(lm_skewed)  # Residuos sesgados

modelsummary(list(Normal = lm_norm, 
                  Uniforme = lm_unif, 
                  Sesgado = lm_skewed),
             coef_rename = c('x' = 'beta1'))

# Gráfico de los residuos para inspección visual
par(mfrow = c(1, 1))
qqnorm(resid(lm_norm), main = "Residuos Normales")
qqline(resid(lm_norm))

qqnorm(resid(lm_heavy_tailed), main = "Residuos Colas Pesadas")
qqline(resid(lm_heavy_tailed))

qqnorm(resid(lm_skewed), main = "Residuos Sesgados")
qqline(resid(lm_skewed))

# Simulación de Error Tipo I
simulate_type_1_error <- function(n_simulations, n) {
  p_values <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    # Generar variable independiente
    x_sim <- rnorm(n)
    
    # Simular datos sin efecto real (hipótesis nula verdadera)
    y_null <- rnorm(n)
    
    # Ajustar modelo lineal
    lm_null <- lm(y_null ~ x_sim)
    
    # Guardar el valor p para la pendiente
    p_values[i] <- summary(lm_null)$coefficients[2, 4]
  }
  
  # Calcular la proporción de resultados significativos (p < 0.05)
  type_1_error_rate <- mean(p_values < 0.05)
  
  return(type_1_error_rate)
}

# Simular la tasa de error Tipo I bajo condiciones normales y no normales
set.seed(123)
type_1_error_normal <- simulate_type_1_error(n_simulations = 1000, n = 100)
type_1_error_heavy_tailed <- simulate_type_1_error(n_simulations = 1000, n = 100)

cat("Tasa de Error Tipo I (Residuos Normales):", type_1_error_normal, "\n")
cat("Tasa de Error Tipo I (Residuos Colas Pesadas):", type_1_error_heavy_tailed, "\n")
