pacman::p_load(tidyverse, here, skimr, modelsummary, taylor, GGally, corrplot)

# Introducción a R ####
# Carguemos los datos
data <- read_csv("data/hn_survey_26k.csv")

# Funciones de r base para "mirar" los datos
summary(data)
str(data)

# Usando el paquete skimr
skim(data)

# Usando el paquete modelsummary
datasummary_skim(data)

# Filtramos el rango de edad de 18 a 25
filter(data, age == "18_25")

# Y usando pipe %>%
data %>% filter(age == "18_25")

# Seleccionamos una columnas
cols <- c("purp", "OD_dist", "travtime")
data %>% 
  filter(age == "18_25") %>% 
  select(cols)

# Para que no joda 
data %>% 
  filter(age == "18_25") %>% 
  select(all_of(cols))

# Creamos la columna travspeed
data %>% 
  filter(age == "18_25") %>% 
  select(all_of(cols)) %>%
  mutate(travspeed = OD_dist / travtime)

# Volvamos a mirar el dataset
data %>% 
  filter(age == "18_25") %>% 
  select(all_of(cols)) %>%
  mutate(travspeed = OD_dist / travtime) %>%
  datasummary_skim()

# Velocidad promedio por propósito de viaje
data %>% 
  filter(age == "18_25") %>% 
  select(all_of(cols)) %>%
  mutate(travspeed = OD_dist / travtime) %>%
  group_by(purp) %>%
  summarise(mean_speed = mean(travspeed))

data %>% 
  filter(age == "18_25") %>% 
  select(cols) %>%
  filter(travtime == 0)

# Velocidad promedio por propósito de viaje (arreglado)
data_summ <- data %>% 
  filter(age == "18_25" & travtime != 0) %>% 
  select(all_of(cols)) %>%
  mutate(travspeed = OD_dist / travtime) %>%
  group_by(purp) %>%
  summarise(mean_speed = mean(travspeed))

data_summ

# Velocidad promedio por propósito de viaje (ordenado)
data %>% 
  filter(age == "18_25" & travtime != 0) %>% 
  select(all_of(cols)) %>%
  mutate(travspeed = OD_dist / travtime) %>%
  group_by(purp) %>%
  summarise(mean_speed = mean(travspeed)) %>%
  arrange(desc(mean_speed))

# O
data_summ %>%
  arrange(desc(mean_speed))

# Grafiquemos
data_summ %>%
  ggplot(aes(x = purp,
             y = mean_speed)) +
    geom_col()

# Grafiquemos cheto
data_summ %>%
  ggplot(aes(x = fct_reorder(purp, mean_speed), #(reordenar las columnas)
             y = mean_speed)) +
  geom_col(fill = "#338A8B") + #(color)
  geom_text(aes(label = paste0(round(mean_speed, 3), " km/min")),
            color = "white",
            hjust = 1.1) +
  coord_flip() + #(invertir los ejes)
  labs(title = "Velocidad promedio por tipo de viaje",
       x = NULL,
       y = NULL) + #(los labels)
  theme_minimal()#(el tema simple)

# Refrescando proba ####
# Muestreamos una normal 0,1
hist(rnorm(1000))

# Muestreamos una normal 0,1 con tidyverse
sample <- tibble(x = rnorm(1000))
sample %>%
  ggplot(aes(x = x)) +
  geom_histogram() +
  theme_bw()
    
# Muestreamos las alturas
set.seed(123)
muestra_10 <- rnorm(n = 10, 175,49)
mean(muestra_10)
sd(muestra_10)

set.seed(123)
muestra_50 <- rnorm(n = 50, 175,49)
mean(muestra_50)
sd(muestra_50)

set.seed(123)
muestra_100 <- rnorm(n = 100, 175,49)
mean(muestra_100)
sd(muestra_100)

# Para hacer el histograma canchero
muestras <- tibble(x = c(muestra_10, muestra_50, muestra_100),
                   n = c(rep("10", 10), rep("50", 50), rep("100", 100)))

muestras %>% ggplot(aes(x = x)) +
  geom_histogram(aes(y = after_stat(density)), fill = "gray80") + 
  stat_function(fun = dnorm, n = 101, 
                args = list(mean = 175, sd = 49), 
                color = "steelblue", 
                linewidth = 1) +
  labs(x = "Altura (cm)", y = "f(x)") +
  facet_wrap(~n, labeller = label_both) +
  theme_minimal()

# Modelo de posición con errores normales
data_t <- tibble(t = rnorm(50, 500, 20)) 
data_t %>% ggplot(aes(x = t)) + geom_histogram() 

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

# Varianza y covarianza ####
set.seed(123)
x <- rnorm(100) # (100 muestras de una N(0,1))
y <- rnorm(100)

var(x)
var(y)

cov(x,y)

# Covarianza no nula
a <- x	
b <- x + 0.2 * y

cov(a,b)

H <- tibble(a, b)
cov(H)

# Matrix de covarianza
H <- tibble(x, y)
cov(H)

H <- tibble(a, b)
cov(H)

H <- tibble(a, b, x, y)
cov(H)

# Correlación
c <- rnorm(100, 0, 10)	
d <- rnorm(100, 0, 5)

G <- tibble(c, d)
cov(G)
cor(G)

e <- c
f <- e + 0.5 * d

K <- tibble(e, f)
cov(K)
cor(K)

# Simpson
data <- tibble(x = c(rnorm(50), 5 + rnorm(50)), 
               y = c(rnorm(50), 5 + rnorm(50)))
data %>% ggplot(aes(x = x, y = y)) +
  geom_point()

cor(data)

# Outlier
data <- tibble(x = c(rnorm(100), 10), 
               y = c(rnorm(100), 10))
data %>% ggplot(aes(x = x, y = y)) +
  geom_point()

cor(data)

# Matrices de corelación
data <- taylor_album_songs %>%
  select(c("danceability", "duration_ms", "tempo")) %>%
  drop_na()

summary(data)
cor(data)

# Usando modelsummary
datasummary_skim(data)
datasummary_correlation(data)

# Con GGAlly
ggpairs(data)

# Con corrplot
M <- cor(data)
corrplot(M, method="color")

# Con todas las variables
data <- taylor_album_songs %>% 
  select_if(is.numeric) %>%
  drop_na()

M <- cor(data)
corrplot(M, method="color")

# Clusterizando
corrplot(M, method="color", 
         order = "hclust")

# Más cositas
corrplot(M, method="ellipse", 
         type = "lower")
