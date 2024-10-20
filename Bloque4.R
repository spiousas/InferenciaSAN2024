pacman::p_load(tidyverse, modelsummary, palmerpenguins, sjPlot, car, parameters, performance)

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
