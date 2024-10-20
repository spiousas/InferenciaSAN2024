# Practica de tidyverse usando los datos del paper de Science Advances

# Objetivos: 1) estudiar la duración de los eventos de sueño registrados
# 2) Filtrar "outliers" en los datos
# 3) Determinar una cantidad mínima de datos para incluir o no a un participante
# 4) Graficar los datos promediados por sujeto para cada grupo
# 5) estudiar la relación entre edad y género con la duración de sueño

# 1) descargar los datos de OSF ####
install.packages("osfr")
library(tidyverse)
library(osfr)

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
  mutate(ID = as.character(ID))

head(demog)

# creo un tibble unificado
toba_sleep <- sleep %>%
  right_join(demog) %>% # uno los datos demográficos
  select(ID, Gender, Age, Group, BoutNumber, Duration)  %>% # selecciono columnas útiles
  na.omit()

head(toba_sleep)
rm(toba_repo, toba_files, sleep, demog)

# 3) opero con los datos ####
# Hay outliers?
library(Routliers)
sleep_outs <- outliers_mad(toba_sleep$Duration, threshold = 2.5) # recomendación de Leys et al.
sleep_outs

# Puedo filtrar todos los datos fuera de los límites
toba_sleep_filtered <- toba_sleep %>%
  filter(Duration >= sleep_outs[3] &
           Duration <= sleep_outs[4])

# resumo la base de datos agrupando por sujetos
toba_summ <- toba_sleep_filtered %>%
  group_by(ID, Gender, Age, Group) %>%
  summarise(Duration = mean(Duration),
            n = n())
head(toba_summ)

# Hay outliers de edad?
age_outs <- outliers_mad(toba_summ$Age, threshold = 2.5) # recomendación de Leys et al.

# Los filtro
toba_summ <- toba_summ %>%
  filter(Age <= age_outs[4])

# histograma de cantidad de registros
ggplot(data = toba_summ) + geom_histogram(aes(n))

# hay relación entre cantidad de registros y duración?
ggplot(data = toba_summ) + geom_point(aes(n, Duration))

# decidimos utilizar todos los datos

# 4) Gráficos ####
# Boxplot con marcas de los CI 95% y datos individuales
ggplot(toba_summ, aes(Group, Duration)) + 
  geom_boxplot(outliers = F, notch = T) +
  geom_jitter(aes(color = Gender), width = .1) +
  scale_y_continuous(limits = c(359, 570),
                     breaks = seq(360, 540, 60),
                     labels = seq(6,9,1)) +
  labs(y = "Sleep duration (h)") +
  scale_x_discrete(limits = c(unique(toba_summ$Group)[1],
                              unique(toba_summ$Group)[2],
                              unique(toba_summ$Group)[3]))+
  theme_bw()

# Gráfico de puntos duración vs. edad, por grupos y género
ggplot(toba_summ, aes(Age, Duration, color = Gender))+
  geom_point(aes(shape = Group)) +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(359, 570),
                     breaks = seq(360, 540, 60),
                     labels = seq(6,9,1)) +
  labs(y = "Sleep duration (h)") +
  theme(legend.position = "top")+
  theme_bw()
