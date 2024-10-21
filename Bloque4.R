pacman::p_load(tidyverse, modelsummary, palmerpenguins, sjPlot, car, parameters, 
               performance, modelr, lme4, sjPlot, patchwork, stargazer)

# Testeemos cositas ####
# Comparamos las medias de dos grupos
penguins_adelie_gentoo <- penguins %>%
  drop_na() %>%
  filter(species %in% c("Adelie", "Gentoo"))

penguins_adelie_gentoo %>%
  ggplot(aes(x = species,
             y = body_mass_g)) +
  stat_summary(geom = "bar", 
               fill = "steelblue", 
               color = "steelblue", 
               alpha = .3) + 
  geom_jitter(color = "darkorange", 
              alpha = .5, 
              width = .2) +
  labs(x = "Especie", y = "Masa en gramos") +
  theme_light()
    
peso_gentoo <- penguins_adelie_gentoo %>% 
  filter(species == "Gentoo") %>%
  pull(body_mass_g)

peso_adelie <- penguins_adelie_gentoo %>% 
  filter(species == "Adelie") %>%
  pull(body_mass_g)

t.test(peso_gentoo, peso_adelie, var.equal = T)

model_t_test <- lm(body_mass_g ~ species,
                   data = penguins_adelie_gentoo)
summary(model_t_test)
model_parameters(model_t_test)

# Comparamos las medias de 3 grupos
penguins %>%
  drop_na() %>%
  ggplot(aes(x = species,
             y = body_mass_g)) +
  stat_summary(geom = "bar", 
               fill = "steelblue", 
               color = "steelblue", 
               alpha = .3) + 
  geom_jitter(color = "darkorange", 
              alpha = .5, 
              width = .2) +
  labs(x = "Especie", y = "Masa en gramos") +
  theme_light()

anova_peng <- aov(body_mass_g ~ species, 
                  data = penguins %>% drop_na())
summary(anova_peng)

lm_3_medias <- penguins %>%
  drop_na() %>%
  lm(body_mass_g ~ species, .) 
summary(lm_3_medias)
Anova(lm_3_medias)

# Interaccion entre especie y sexo
# Comparamos las medias de dos grupos
penguins_adelie_chinstrap <- penguins %>%
  drop_na() %>%
  filter(species %in% c("Adelie", "Chinstrap"))

penguins_adelie_chinstrap %>%
  ggplot(aes(x = species,
             y = body_mass_g,
             fill = sex,
             color = sex)) +
  stat_summary(geom = "bar", 
               alpha = .3, 
               position = position_dodge()) + 
  geom_point(pch = 21,
             alpha = .5,
             position = position_jitterdodge()) +
  labs(x = "Especie", y = "Masa en gramos") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_light() +
  theme(legend.position = "top")

anova_interac <- aov(body_mass_g ~ species * sex, 
                     data = penguins_adelie_chinstrap)
summary(anova_interac)

lm_interac <- lm(body_mass_g ~ species * sex, 
                  data = penguins_adelie_chinstrap)

Anova(lm_interac)
model_parameters(lm_interac)

data_new <- expand_grid(species = c("Chinstrap", "Adelie"),
                        sex = c("male", "female"))
data_new

predict(lm_interac, newdata = data_new)

data_new %>% 
 add_predictions(lm_interac)

# Interaccion con una variable continua
penguins_adelie_gentoo %>%
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             fill = species,
             color = species)) +
  geom_point(pch = 21,
             alpha = .5,
             position = position_jitterdodge()) +
  geom_smooth(method = lm,
              se = F) +
  labs(x = "Ancho del pico en mm", y = "Masa en gramos") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_light() +
  theme(legend.position = "top")

lm_interac_cont <- lm(body_mass_g ~ species * bill_depth_mm, 
                      data = penguins_adelie_gentoo)

Anova(lm_interac_cont)
model_parameters(lm_interac_cont)

# Centremos la variable
penguins_adelie_gentoo_centered <- penguins_adelie_gentoo %>%
  mutate(bill_depth_mm = bill_depth_mm - mean(bill_depth_mm))

mean(penguins_adelie_gentoo_centered$bill_depth_mm)

penguins_adelie_gentoo_centered %>%
  ggplot(aes(x = bill_depth_mm,
             y = body_mass_g,
             fill = species,
             color = species)) +
  geom_vline(xintercept = 0,
             color = "gray50") +
  geom_point(pch = 21,
             alpha = .5,
             position = position_jitterdodge()) +
  geom_smooth(method = lm,
              se = F) +
  labs(x = "Especie", y = "Masa en gramos") +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  theme_light() +
  theme(legend.position = "top")

lm_interac_centered <- lm(body_mass_g ~ species * bill_depth_mm, 
                      data = penguins_adelie_gentoo_centered)

Anova(lm_interac_centered)
model_parameters(lm_interac_centered)

modelsummary(list("Sin centrar" = lm_interac_cont, "Centrado" = lm_interac_centered), gof_omit = ".*",
             statistic = c("p = {p.value}"))

# Modelos lineales mixtos ####
# El ejemplo de los dragones
load("data/dragons.RData")
head(dragons)

dragons %>%
  ggplot(aes(x = testScore)) +
  geom_histogram(aes(y = after_stat(density)), position="identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Score en el test de inteligencia", y = "Densidad") +
  theme(legend.position = "none")

# Probemos con un modelo lineal
basic.lm <- lm(testScore ~ bodyLength, data = dragons)
model_parameters(basic.lm)

dragons %>%
  ggplot(aes(x = bodyLength, y = testScore)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", color = "darkorange") +
  labs(y = "Score en el test de inteligencia", x = "Longitud del cuerpo") +
  theme_classic()

datasummary_skim(dragons)

check_model(basic.lm)

# La inteligencia por montaña
dragons %>%
  ggplot(aes(x = mountainRange, y = testScore)) +
  geom_boxplot(color = "steelblue") +
  geom_jitter(color = "steelblue", alpha = .2, width = .2) +
  geom_smooth(method = "lm", color = "darkorange") +
  labs(x = "Score en el test de inteligencia", y = "Longitud del cuerpo") +
  theme_classic()

# y en función de la longitud del cuerpo
dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = mountainRange)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  theme_classic() +
  theme(legend.position = "top")

dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = mountainRange)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm", se = F) +
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~mountainRange) +
  theme_classic() +
  theme(legend.position = "none")

# Un modelo con montaña como factor
mountain.lm <- lm(testScore ~ bodyLength + mountainRange, data = dragons)
model_parameters(mountain.lm)

# Un modelo de efectos mixtos
mixed.lmer <- lmer(testScore ~ bodyLength + (1|mountainRange), data = dragons)
summary(mixed.lmer)

# Los efectos aleatorios
plot_model(mixed.lmer, type = "re", show.values = TRUE) +
  theme_bw() 
  
check_model(mixed.lmer)

# Las predicciones
dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = mountainRange)) +
  geom_point(alpha = .5) +
  geom_line(data = cbind(dragons, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~mountainRange) +
  theme_classic() +
  theme(legend.position = "none")
  
# Teniendo en cuenta los sites
dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = site)) +
  geom_point(alpha = .5) +
  geom_smooth(method = lm, se = F) +
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = "Site") +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~mountainRange) +
  theme_classic() +
  theme(legend.position = "top")

mixed.crossed <- lmer(testScore ~ bodyLength + (1|mountainRange) + (1|site), 
                     data = dragons)
summary(mixed.crossed)

# Veamos los efectos aleatorios
ranef(mixed.crossed)
plot_ranefs <- plot_model(mixed.crossed, type = "re", show.values = TRUE, facet.grid=FALSE) 
plot_ranefs[[1]] + theme_bw() + plot_ranefs[[2]] + theme_bw()

# Hamgamos bien el nesting
mixed.nested <- lmer(testScore ~ bodyLength + (1|mountainRange) + (1|site:mountainRange), 
                     data = dragons)
summary(mixed.nested)

# Veamos los efectos aleatorios
ranef(mixed.nested)
plot_ranefs <- plot_model(mixed.nested, type = "re", show.values = TRUE, facet.grid=FALSE) 
plot_ranefs[[1]] + theme_bw() + plot_ranefs[[2]] + theme_bw()

# Las predicciones
dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = site)) +
  geom_point(alpha = .5) +
  geom_line(data = cbind(dragons, pred = predict(mixed.nested)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~mountainRange) +
  theme_classic() +
  theme(legend.position = "none")

# Pendiente aleatoria
mixed.slope <- lmer(testScore ~ bodyLength + (bodyLength|mountainRange/site),
                    data = dragons)
summary(mixed.slope)

plot_ranefs <- plot_model(mixed.slope, type = "re", show.values = FALSE, facet.grid=FALSE) 
plot_ranefs[[1]] + theme_bw() + plot_ranefs[[2]] + theme_bw()


# Las predicciones
dragons %>%
  ggplot(aes(x = bodyLength, 
             y = testScore, 
             color = site)) +
  geom_point(alpha = .5) +
  geom_line(data = cbind(dragons, pred = predict(mixed.slope)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
  labs(y = "Score en el test de inteligencia", 
       x = "Longitud del cuerpo",
       color = NULL) +
  scale_color_brewer(palette = "Dark2") +
  facet_wrap(~mountainRange) +
  theme_classic() +
  theme(legend.position = "none")
