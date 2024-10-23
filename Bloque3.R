pacman::p_load(tidyverse, modelsummary, palmerpenguins, sjPlot, car, parameters, performance)

# Modelo de posición con errores normales ####
data_t <- tibble(t = rnorm(50, 500, 20)) 
data_t %>% ggplot(aes(x = t)) + geom_histogram() 

mean(data_t$t)
sd(data_t$t)

# Mejorando el histograma
data_t %>% ggplot(aes(x = t)) + 
  geom_histogram(fill = "steelblue") +
  theme_minimal() 

# Más
data_t %>% ggplot(aes(x = t, y = ..density..)) + 
  geom_histogram(fill = "steelblue", alpha = .5)+
  geom_density(linewidth = 2) +
  xlim(c(420, 580)) +
  geom_vline(xintercept = mean(data_t$t),
             linetype = "dashed", linewidth = 1) +
  labs(title = "Histograma cheto", x = "t", y = NULL) +
  theme_minimal()

# Muchas medias
mean_T <- c()
for (i in 1:100) {
  mean_T <- c(mean_T, mean(rnorm(50, 500, 20)))
}

# Muchas medias en un tibble
data_total <- tibble(sample = numeric(), 
                     t =numeric())

for (i in 1:100) {
  data_t <- tibble(sample = i, t = rnorm(50, 500, 20))
  data_total <- data_total %>% bind_rows(data_t)
}

data_medias <- data_total %>% 
  group_by(sample) %>%
  summarise(mT = mean(t))

mean(data_medias$mT)
sd(data_medias$mT)
sqrt(400/50)

# Para distinto cantidad de muestras de n=50
data_n_fijo <- tibble(n = numeric(),
                     mT = numeric(), 
                     sdT =numeric())

set.seed(1234)
sample <- mean(rnorm(50, 500, 20))
for (i in 2:150) {
    sample <- c(sample, mean(rnorm(50, 500, 20)))
    data_t <- tibble(n = i, mT = mean(sample), sdT = sd(sample))
    data_n_fijo <- data_n_fijo %>% bind_rows(data_t)
}

data_n_fijo %>%
  ggplot(aes(x = n,
             y = mT)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_hline(yintercept = 500, color = "darkorange", linetype = "dashed") +
  labs(x = "Cantidad de muestras de n=50", y = "Media de las medias") +
  theme_minimal()
    
data_n_fijo %>%
  ggplot(aes(x = n,
             y = sdT)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_hline(yintercept = 20/sqrt(50), color = "darkorange", linetype = "dashed") +
  labs(x = "Cantidad de muestras de n=50", y = "SD de las medias") +
  theme_minimal()

# Para distintos ns con 100 muestras para cada n
data_ns <- tibble(n =numeric(),
                  mT = numeric())

set.seed(1234)
for (n in c(10, 50, 100)) {
  for (i in 1:100) {
    data_t <- tibble(n = n, mT = mean(rnorm(n, 500, 20)))
    data_ns <- data_ns %>% bind_rows(data_t)
  }  
}

data_ns %>%
  ggplot(aes(x = mT,
             y = after_stat(density))) +
  geom_histogram(fill = "steelblue") + 
  geom_vline(xintercept = 500, color = "darkorange", linetype = "dashed") +
  labs(x = "Altura (cm)", y = "f(x)") +
  facet_grid(~n, labeller = label_both) +
  theme_minimal()

# Pongamos las densidades encima
densidades <- expand_grid(x = seq(480, 520, .5), n = c(10, 50, 100)) %>%
  mutate(densidad = dnorm(x = x, mean = 500, sd = sqrt(400/n)))
    
data_ns %>%
  ggplot(aes(x = mT,
             y = after_stat(density))) +
  geom_histogram(fill = "steelblue") + 
  geom_line(data = densidades, aes(x = x, y = densidad), color = "black", size = 1) +
  geom_vline(xintercept = 500, color = "darkorange", linetype = "dashed") +
  labs(x = "Altura (cm)", y = "f(x)") +
  facet_grid(~n, labeller = label_both) +
  theme_minimal()

# Modelo lineal ####
# Modelo lineal con los pinguinos 
count(penguins, species)

penguins_adelie <- penguins %>%
  filter(species == "Adelie")

head(penguins_adelie)

penguins_adelie <- penguins %>%
  filter(species == "Adelie") %>%
  drop_na()

# Ancho versus largo de pico
penguins_adelie %>% ggplot(aes(x = bill_depth_mm,
                    y = bill_length_mm)) +
  geom_point() +
  theme_minimal()

# Agreguemos una línea de tendencia
penguins_adelie %>% ggplot(aes(x = bill_depth_mm,
                    y = bill_length_mm)) +
  geom_point() +
  geom_smooth() +
  theme_minimal()

# Agreguemos una línea de tendencia LINEAL
penguins_adelie %>% ggplot(aes(x = bill_depth_mm,
                    y = bill_length_mm)) +
  geom_point() +
  geom_smooth(method = lm) +
  theme_minimal()

# El modelo lineal
model <- lm(data = penguins_adelie, bill_length_mm ~ bill_depth_mm)  
summary(model)

modelsummary(list("Modelo inicial" = model))

# Sigamos con modelos con los pinguinos
m1 <- lm(data = penguins_adelie, body_mass_g ~ flipper_length_mm)  
summary(m1)

# Y con pipe
m1 <- penguins_adelie %>%
  lm(body_mass_g ~ flipper_length_mm, .)  

# Ahora agreguemos una variable categórica
penguins_adelie_gentoo <- penguins %>%
  drop_na() %>%
  filter(species %in% c("Adelie", "Gentoo"))

m2 <- lm(data = penguins_adelie_gentoo, body_mass_g ~ species)  

summary(m2)

# Helados y temperatura (residuos normales)
set.seed(1414)
helados <- tibble(
  Temperatura = rnorm(500, 20, 5),
  Helados = 1 + 1*Temperatura + rnorm(500)
)

mnorm <- lm(Helados ~ Temperatura, data = helados)
summary(mnorm)

helados  %>% ggplot(aes(x = Temperatura, y = Helados)) +
  geom_point(alpha = .2, color = "steelblue") +
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_minimal()

check_model(mnorm)

# Helados y temperatura (residuos uniformes)
set.seed(1414)
helados <- tibble(
  Temperatura = rnorm(500, 20, 5),
  Helados = 1 + 1*Temperatura + runif(500, -sqrt(3), sqrt(3))
)

munif <- lm(Helados ~ Temperatura, data = helados)
summary(munif)

helados  %>% ggplot(aes(x = Temperatura, y = Helados)) +
  geom_point(alpha = .2, color = "steelblue") +
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_minimal()

# Helados y temperatura (residuos exponenciales)
set.seed(1414)
helados <- tibble(
  Temperatura = rnorm(500, 20, 5),
  Helados = 1 + 1*Temperatura + rexp(500)
)

mexp <- lm(Helados ~ Temperatura, data = helados)
summary(mexp)

helados  %>% ggplot(aes(x = Temperatura, y = Helados)) +
  geom_point(alpha = .2, color = "steelblue") +
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_minimal()

check_model(mexp)

# Helados y temperatura (variabilidad no constante)
set.seed(1414)
helados <- tibble(
  Temperatura = rnorm(500, 20, 5),
  Helados = 1 + 1*Temperatura + rnorm(500) * Temperatura/5
)

mnorm <- lm(Helados ~ Temperatura, data = helados)
summary(mnorm)

helados  %>% ggplot(aes(x = Temperatura, y = Helados)) +
  geom_point(alpha = .2, color = "steelblue") +
  geom_smooth(color = "red", method = "lm", se = F) + 
  theme_minimal()

check_model(mnorm)

# Chequeando los supuestos
check_model(mnorm)
# Agreguemos covariables
m3 <- penguins_adelie %>%
  lm(body_mass_g ~ bill_length_mm + flipper_length_mm, .) 
summary(m3)

# Agreguemos una covariable discreta
m4 <- penguins_adelie_gentoo %>%
  lm(body_mass_g ~ bill_depth_mm + species, .) 
summary(m4)

penguins_adelie_gentoo %>% ggplot(aes(x = bill_depth_mm, 
                                      y = body_mass_g)) +
  geom_point(alpha = .3, color = "steelblue", size = 2) +
  geom_smooth(color = "darkorange", method = "lm", se = F) + 
  theme_minimal()

penguins_adelie_gentoo %>% ggplot(aes(x = bill_depth_mm, 
                                      y = body_mass_g,
                                      color = species)) +
  geom_point(alpha = .3,  size = 2) +
  geom_smooth(method = "lm", se = F) + 
  theme_minimal() +
  theme(legend.position = "top")

# Agreguemos una covariable discreta con más de dos niveles
m5 <- penguins %>%
  drop_na() %>%
  lm(body_mass_g ~ bill_depth_mm + species, .) 
summary(m5)

penguins %>%
  drop_na() %>% 
  ggplot(aes(x = bill_depth_mm, y = body_mass_g)) +
  geom_point(alpha = .3, color = "steelblue", size = 2) +
  geom_smooth(color = "darkorange", method = "lm", se = F) + 
  theme_minimal()

penguins %>%
  drop_na() %>%  
  ggplot(aes(x = bill_depth_mm, 
             y = body_mass_g,
             color = species)) +
  geom_point(alpha = .3,  size = 2) +
  geom_smooth(method = "lm", se = F) + 
  theme_minimal() +
  theme(legend.position = "top")

# Salidas de los modelos
model_parameters(m5)

modelsummary(m5)

  modelsummary(m5, gof_omit = ".*",
             statistic = c("conf.int",
                           "p = {p.value}"))

# El efecto conjunto
# Suma de cuadrados para el modelo restringido
m5_res <- penguins %>% drop_na() %>%
  lm(body_mass_g ~ bill_depth_mm, .) 

models <- list(Unrestricted = m5, Restricted = m5_res)
modelsummary(models, gof_omit = ".*",
             statistic = c("conf.int",
                           "p = {p.value}"))

SSR_res <- sum(resid(m5_res)^2)
SSR_res

# Suma de cuadrados para el modelo no restringido (sin la variable de interés como predictora)
SSR_unres <- sum(resid(m5)^2)
SSR_unres

# Calculo el estadístico F
q <- 2
n <- 333
k <- 3
F <- ((SSR_res-SSR_unres)/q)/(SSR_unres/(n-k-1))
F

# Con R
Anova(m5) # Anova con mayúscula del paquete car 

# La ditribución F usando el paquete sjPlot
set_theme(base = theme_light())
dist_f(deg.f1 = 2, deg.f2 = 329) 

dist_f(f = F, deg.f1 = 2, deg.f2 = 329) 

# Todo más simple
Anova(m5, type = 3)

# Regresión Logística (si queda tiempo) ####
pacman::p_load(ISLR2)

# Leemos los datos ####
Advertising <- read_csv(here("data/Advertising.csv")) |>
  select(-"...1")
# ventas en miles de unidades y gasto en miles de USD

# El modelo simple
model <- lm(data = Advertising, sales ~ TV)
summary(model)
check_model(model)

# El modelo full ####
model_full <- lm(data = Advertising, sales ~ .)
summary(model_full)
check_model(model_full)
confint(model_full)
parameters::model_parameters(model_full)

## Predicción con el modelo simple
pred <- model_full$coefficients[1] + 120 * model_full$coefficients[2] + 
  30 * model_full$coefficients[3] + 30 * model_full$coefficients[4]
predict(model_full, newdata = tibble(TV = 120, radio = 30, newspaper =30))

# Logística 
Default |> ggplot(aes(x = balance,
                      y = default)) +
  geom_point(alpha = .2) +
  theme_bw()

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,
             y = default_num)) +
  geom_point(alpha = .2) +
  geom_smooth() +
  theme_bw()

linear_model <- lm(data = Default |> mutate(default_num = if_else(default == "Yes", 1, 0)), 
                   default_num ~ balance)
linear_model

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,y = default_num)) +
  # geom_abline(intercept = linear_model$coefficients[1], slope = linear_model$coefficients[2],
  #             color = "steelblue", linewidth = 2) +
  geom_point(alpha = .2) +
  theme_bw()

log_reg <- glm(data = Default |> mutate(default_num = if_else(default == "Yes", 1, 0)), 
               default_num ~ balance, family = "binomial")
log_reg


fit <- tibble(
  balance   = seq(0, 3000, 1),
  default_num = exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance)/
    (1+exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance))
)

Default |> mutate(default_num = if_else(default == "Yes", 1, 0)) |>
  ggplot(aes(x = balance,y = default_num)) +
  # geom_abline(intercept = linear_model$coefficients[1], slope = linear_model$coefficients[2],
  #             color = "steelblue", linewidth = 2) +
  geom_point(alpha = .2) +
  geom_line(data = fit, aes(x = balance, y = default_num),
            color = "darkorange", linewidth = 2) +
  theme_bw()

balance_new = 1600

exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance_new)/ (1+exp(log_reg$coefficients[1]+log_reg$coefficients[2]*balance_new))
predict(log_reg, newdata = tibble(balance = 1600))

exp(-1.853064)/(1+exp(-1.853064 ))
predict(log_reg, newdata = tibble(balance = 1600), type = "response")

