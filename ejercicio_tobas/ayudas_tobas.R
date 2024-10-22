# Practica de tidyverse usando los datos del paper de Science Advances

# Objetivos: 1) estudiar la duración de los eventos de sueño registrados
# 2) Filtrar "outliers" en los datos (pensar en qué categorías/cuándo filtrar)
# 3) Hacer una tabla resumen de los datos demográficos de los grupos (n, género y edad)
# 3) Graficar los datos promediados por sujeto para cada grupo y hacer una tabla
# con los valores de promedio y error estándar
# 4) estudiar la relación entre edad y género con la duración de sueño

pacman::p_load(dplyr, readr, ggplot2, osfr, lubridate)
# 1) Descargar y cargar los datos de OSF ####
# obtengo info del repo de OSF:
toba_repo <- osf_retrieve_node("https://osf.io/nxtvk/")

# lista de archivos
toba_files <- osf_ls_files(toba_paper) 

# descargo los deseados
osf_download(toba_files[c(4:6),], path = "./data/toba_data") 

# Cargo los datos
# ¡abrir el archivo de metadata para conocer el contenido de los archivos!
# eventos de sueño
sleep <- read_csv("./data/toba_data/Sleep_data_Toba_Qom.csv")
head(sleep)

# metadata demográfica
demog <- read_csv("./data/toba_data/Demographics_Toba_Qom.csv") %>%
  mutate(ID = as.character(ID),  # para que matchee con el ID de sleep
         Group = factor(Group, levels = c("Rural no light",  # para ordenar los niveles
                                          "Rural limited light",
                                          "Urban")))
head(demog)

# creo un tibble unificado
toba_sleep <- sleep %>%
  right_join(demog) %>% # uno los datos demográficos
  na.omit()
head(toba_sleep)

rm(toba_repo, toba_files, sleep, demog)

# 2) opero con los datos ####
# Tabla resumen demográfico
knitr::kable(toba_sleep %>% group_by(ID, Gender, Age, Group) %>%
       summarise() %>%
       mutate(female = if_else(Gender == "F", 1,0)) %>%
       group_by(Group) %>%
       summarise(n = n(),
            Mean_Age = round(mean(Age),1),
            Female_perc = round(sum(female)/n*100)))


# resumo la base de datos agrupando por sujetos
toba_summ <- toba_sleep %>%
  group_by(ID, Gender, Age, Group) %>%
  summarise(n = n(),
            sem = sd(Duration)/sqrt(n),
            Duration = mean(Duration))

# 3) Gráficos ####
# Boxplot con marcas de los CI 95% y datos individuales
ggplot(toba_summ, aes(Group, Duration)) + 
  geom_boxplot(outliers = F) +
  geom_jitter(aes(color = Gender), size = 2, alpha = 0.5, width = .1) +
  scale_y_continuous(limits = c(359, 570),
                     breaks = seq(360, 540, 60),
                     labels = seq(6,9,1)) +
  labs(y = "Sleep duration (h)") +
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

# 4) Modelo lineal con efectos mixtos ####
library(lme4)
library(car)

lm_mixto <- lmer(Duration ~ Age + Gender + Group + (1|ID), 
              data = toba_sleep)
summary(lm_group)
confint(lm_group) # 95% CI de los efectos
Anova(lm_group)

# Contrastes entre grupos
emmeans::emmeans(lm_group, pairwise~Gender,
                 pbkrtest.limit = 3900,
                 lmerTest.limit = 3900)

# 5) Cambios en el sueño a lo largo del fotoperíodo ####
# Genero una columna con el día del año en términos continuos
library(lubridate)
toba_sleep <- toba_sleep %>%
  mutate(year_date = yday(as.Date(Start_Date, format = "%m/%d/%Y")))

# A ver qué pasa con la duración del sueño
ggplot(data = toba_sleep, aes(year_date, Duration, color = Group)) +
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", se = F) + 
  theme_bw()

modelo_fotoperiodo <- lmer(Duration ~ Group + year_date + Gender + Age +
                             (1|ID), data = toba_sleep)
summary(modelo_fotoperiodo)
confint(modelo_fotoperiodo)
Anova(modelo_fotoperiodo)

# Y con el horario de dormirse?
toba_sleep <- toba_sleep %>%
  mutate(start_time = as.POSIXct(paste(Start_Date, Start_Time),
                                 format = "%m/%d/%Y %H:%M"),
         start_hour = as.numeric(difftime(start_time,
                               round_date(start_time, unit = "day"),
                               unit = "mins")))

ggplot(data = toba_sleep, aes(year_date, start_hour, color = Group)) +
  geom_point(alpha = 0.4)+
  geom_smooth(method = "lm", se = F) + 
  theme_bw()


modelo_fotoperiodo_2 <- lmer(start_hour ~ Group + year_date + Gender + Age +
                             (1|ID), data = toba_sleep)
summary(modelo_fotoperiodo_2)
confint(modelo_fotoperiodo_2)
Anova(modelo_fotoperiodo_2)
         