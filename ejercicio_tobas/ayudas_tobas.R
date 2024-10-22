# Practica de tidyverse usando los datos del paper de Science Advances

# Objetivos: 1) estudiar la duración de los eventos de sueño registrados
# 2) Filtrar "outliers" en los datos (pensar en qué categorías/cuándo filtrar)
# 3) Hacer una tabla resumen de los datos demográficos de los grupos (n, género y edad)
# 3) Graficar los datos promediados por sujeto para cada grupo y hacer una tabla
# con los valores de promedio y error estándar
# 4) estudiar la relación entre edad y género con la duración de sueño

pacman::p_load(dplyr, readr, ggplot2, osfr)
# 1) descargar los datos de OSF ####
# obtengo info del repo de OSF:
toba_repo <- osf_retrieve_node("https://osf.io/nxtvk/")

toba_files <- osf_ls_files(toba_paper) # lista de archivos
osf_download(toba_files[c(4:6),], path = "./data/toba_data") # descargo los deseados

# 2) cargo los datos ####
# abrir el archivo de metadata para conocer el contenido de los archivos
# eventos de sueño
sleep <- read_csv("./data/toba_data/Sleep_data_Toba_Qom.csv")

head(sleep)

# metadata demográfica
demog <- read_csv("./data/toba_data/Demographics_Toba_Qom.csv") %>%
  mutate(ID = as.character(ID),
         Group = factor(Group, levels = c("Rural no light", 
                                          "Rural limited light",
                                          "Urban")))

head(demog)

# creo un tibble unificado
toba_sleep <- sleep %>%
  right_join(demog) %>% # uno los datos demográficos
  select(ID, Gender, Age, Group, BoutNumber, Duration)  %>% # selecciono columnas útiles
  na.omit()

head(toba_sleep)
rm(toba_repo, toba_files, sleep, demog)

# Tabla resumen demográfico
knitr::kable(toba_sleep %>% group_by(ID, Gender, Age, Group) %>%
       summarise() %>%
       mutate(female = if_else(Gender == "F", 1,0)) %>%
       group_by(Group) %>%
       summarise(n = n(),
            Mean_Age = round(mean(Age),1),
            Female_perc = round(sum(female)/n*100)))

# 3) opero con los datos ####

# resumo la base de datos agrupando por sujetos
toba_summ <- toba_sleep %>%
  group_by(ID, Gender, Age, Group) %>%
  summarise(n = n(),
            sem = sd(Duration)/sqrt(n),
            Duration = mean(Duration))

# 4) Gráficos ####
# Boxplot con marcas de los CI 95% y datos individuales
ggplot(toba_summ, aes(Group, Duration)) + 
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = Gender), size = 2, alpha = 0.5, width = .1) +
  scale_y_continuous(limits = c(359, 570),
                     breaks = seq(360, 540, 60),
                     labels = seq(6,9,1)) +
  labs(y = "Sleep duration (h)") +
  # scale_x_discrete(limits = c(unique(toba_summ$Group)[1],
  #                             unique(toba_summ$Group)[2],
  #                             unique(toba_summ$Group)[3]))+
  theme_bw()

# Tabla de promedios
knitr::kable(toba_summ %>% group_by(Group) %>%
               summarise(Duration_mean = round(mean(Duration)/60, 1),
                         Duration_sem = round(sd(Duration)/60*sqrt(n()),1)))

# Gráfico de puntos duración vs. edad, por grupos y género
ggplot(toba_summ %>% filter(Age <40), aes(Age, Duration, color = Gender))+
  geom_point(aes(shape = Group)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(390, 570),
                     breaks = seq(360, 540, 60),
                     labels = seq(6,9,1)) +
  labs(y = "Sleep duration (h)") +
  theme(legend.position = "top")+
  theme_bw()

# Modelo lineal con efectos mixtos ####
library(lme4)
library(car)
lm_crudo <- lm(Duration ~ Age + Gender + Group, 
                  data = toba_sleep)
summary(lm_crudo)
confint(lm_crudo)
Anova(lm_crudo)
emmeans::emmeans(lm_crudo, pairwise~Gender,
                 pbkrtest.limit = 3900,
                 lmerTest.limit = 3900)


lm_mixto <- lmer(Duration ~ Age + Gender + (1|Group) + (1|ID), 
              data = toba_sleep)
summary(lm_group)
confint(lm_group)
Anova(lm_group)
emmeans::emmeans(lm_group, pairwise~Gender,
                 pbkrtest.limit = 3900,
                 lmerTest.limit = 3900)
