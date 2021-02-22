rm(list=ls())

#Paquetes
library(readr)
library(tidyverse)
library(BAMMtools)
library(fastDummies)
library(FactoMineR)
library(factoextra)

#Abrir la base de victimas en carpetas de investigacion de la ADIP
victimas <- read_csv("Espacios pulicos/PD identification/Input/victimas/victimas-en-carpetas-de-investigacion-pgj.csv",
                     locale = locale(encoding = "UTF-8"))

#Seleccionar las victimas que son mujeres y que sufrieron delitos de genero, robos siendo pasajeras y siendo transeuntes
data <- victimas %>% 
  filter(sexo== "Femenino",
         str_detect(delito, "VIOLACION|SEXUAL|FEMINICIDIO|ESTUPRO|SECUESTRO|
                    PRIVACION DE LA LIBERTAD PERSONAL|ROBO A PASAJERO|ROBO A TRANSEUNTE"),
         !delito== "VIOLACION DE CORRESPONDENCIA") %>% 
  select(id, delito, edad, fechahecho, horahecho)

#Crear dia de la semana y hora en que sufrieron los delitos
data <- data %>% 
  mutate(fechahecho = as.Date(fechahecho, "%d/%m/%Y"),
         diasemana = weekdays(as.Date(fechahecho)),
         diasemana = factor(diasemana, levels = c("lunes", "martes", "miércoles", 
                                                  "jueves", "viernes", "sábado", 
                                                  "domingo")),
         hora = as.POSIXlt(horahecho)$hour) %>% 
  na.omit()

#Histogramas y graficas de barras 
#Numero de victimas por hora en que ocurrio el delito y por tipo de delito
ggplot(data, aes(x = hora)) +
  geom_histogram(binwidth = 1, fill= "#69b3a2", colour = "#bee3db") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 50),
        axis.title.x = element_text(size = 50)) +
  facet_wrap(~delito, scales = "free_y")

ggsave("Espacios pulicos/PD identification/Output/horadelitos.jpg", width = 35, height = 18)

#Numero de victimas por edad y por tipo de delito
ggplot(data, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill= "#69b3a2", colour = "#bee3db") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 50),
        axis.title.x = element_text(size = 50)) +
  facet_wrap(~delito, scales = "free_y")

ggsave("Espacios pulicos/PD identification/Output/edaddelitos.jpg", width = 35, height = 18)

#Numero de victimas por dia de la semana en que ocurrio el delito y tipo de delito
tempo <- data %>% 
  group_by(diasemana, delito) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  ungroup()

ggplot(tempo, aes(x= diasemana, y = n)) + 
  geom_bar(stat="identity", fill= "#69b3a2") +
  theme_minimal()+
  theme(axis.title.y = element_text(size = 50),
        axis.title.x = element_text(size = 50)) +
  facet_wrap(delito ~ ., scales = "free_y")

ggsave("Espacios pulicos/PD identification/Output/diasemana_delitos.jpg", width = 35, height = 18)

#Clasificacion de las horas y la edad con natural breaks
horasjenks <- getJenksBreaks(data$hora, 4)
edadjenks <- getJenksBreaks(data$edad, 5)

data <- data %>% 
  mutate(horas_grupos = ifelse(hora >=0 & hora <= 8, "0a8",
                               ifelse(hora >=9 & hora <= 15, "9a15", 
                                      "16a23")),
         horas_grupos = factor(horas_grupos, levels = c("0a8", "9a15", "16a23")),
         edad_grupos = ifelse(edad >=0 & edad <= 18, "0a18",
                              ifelse(edad >=19 & edad <=31, "19a31",
                                     ifelse(edad>= 32 & edad<= 46, "32a46", 
                                            "47ymas"))),
         edad_grupos = factor(edad_grupos, levels = c("0a18", "19a31", "32a46", 
                                                      "47ymas")),
         diasemana_grupos = ifelse(diasemana == "sábado", "findesemana", 
                                   ifelse(diasemana == "domingo", "findesemana", 
                                          "entresemana")),
         diasemana_grupos = factor(diasemana_grupos, levels = c("entresemana", 
                                                                "findesemana")))

tempo <- data %>% 
  group_by(horas_grupos, delito) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  ungroup()

ggplot(tempo, aes(x= horas_grupos, y = n)) + 
  geom_bar(stat="identity", fill= "#69b3a2") +
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50))
ggsave("Espacios pulicos/PD identification/Output/horas_jenks.jpg", width = 35, height = 18)

tempo <- data %>% 
  group_by(edad_grupos, delito) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  ungroup()

ggplot(tempo, aes(x= edad_grupos, y = n)) + 
  geom_bar(stat="identity", fill= "#69b3a2") +
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50))
ggsave("Espacios pulicos/PD identification/Output/edad_jenks.jpg", width = 35, height = 18)

tempo <- data %>% 
  group_by(diasemana_grupos, delito) %>% 
  summarise(n = n(),.groups = 'drop') %>% 
  ungroup()

ggplot(tempo, aes(x= diasemana_grupos, y = n)) + 
  geom_bar(stat="identity", fill= "#69b3a2") +
  theme_minimal()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50))
ggsave("Espacios pulicos/PD identification/Output/diasemana.jpg", width = 35, height = 18)

#Hierarchical Clustering on Principal Components (HCPC)
#PCA
tempo <- data %>% 
  select(delito, diasemana_grupos, horas_grupos, edad_grupos) %>% 
  dummy_cols() %>% 
  select(-delito, -diasemana_grupos, -horas_grupos, -edad_grupos)

pca <- PCA(tempo, ncp= Inf)
pca$eig
pca <- PCA(tempo, ncp=48)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 45))

#PCA2
data <- data %>% 
  mutate(diasemana_num = (diasemana),
         diasemana_num = case_when(
           diasemana_num == "lunes" ~ 1,
           diasemana_num == "martes"~ 2, 
           diasemana_num == "miércoles"~ 3,
           diasemana_num == "jueves"~ 4,
           diasemana_num == "viernes" ~ 5,
           diasemana_num == "sábado"~ 6,
           diasemana_num == "domingo"~ 7))

tempo <-  data %>% 
  select(delito, diasemana_num, hora, edad) %>% 
  dummy_cols(select_columns = c("delito")) %>% 
  select(-delito)

pca <- PCA(tempo, ncp= Inf)
pca$eig
pca <- PCA(tempo, ncp=46)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 30))

#PCA solo para delitos de genero
data_genero <- data %>% 
  filter(str_detect(delito, "VIOLACION|SEXUAL|FEMINICIDIO|ESTUPRO|SECUESTRO|PRIVACION DE LA LIBERTAD PERSONAL"))

tempo <- data_genero %>% 
  select(delito, diasemana_grupos, horas_grupos, edad_grupos) %>% 
  dummy_cols() %>% 
  select(-delito, -diasemana_grupos, -horas_grupos, -edad_grupos)

pca <- PCA(tempo, ncp= Inf, graph = FALSE)
pca$eig
pca <- PCA(tempo, ncp=21, graph = FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 45))

#HCPC solo para delitos de genero
hcpc <- HCPC(pca, graph = FALSE)
hcpc$desc.var$quanti
hcpc$desc.ind$para

cluster <- hcpc$data.clust$clust
data_genero <- cbind(data_genero, cluster)

resultados <- data_genero %>%
  group_by(cluster) %>%
  do(resumen = summary(.))
resultados$resumen

#PCA2 solo para delitos de genero
tempo <-  data_genero %>% 
  select(delito, diasemana_num, hora, edad) %>% 
  dummy_cols(select_columns = c("delito")) %>% 
  select(-delito)

pca <- PCA(tempo, ncp= Inf, graph = FALSE)
pca$eig
pca <- PCA(tempo, ncp=19, graph = FALSE)
fviz_screeplot(pca, addlabels = TRUE, ylim = c(0, 45))

hcpc <- HCPC(pca, graph = FALSE)
hcpc$desc.var$quanti
hcpc$desc.ind$para

#Dendograma (tarda mucho en correr)
fviz_dend(hcpc, 
          cex = 0.7,                     
          palette = "Set3",               
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "Set3",           
          labels_track_height = 0.8) 

cluster2 <- hcpc$data.clust$clust
data_genero <- cbind(data_genero, cluster2)

resultados_c2 <- data_genero %>%
  group_by(cluster2) %>%
  do(resumen = summary(.))

resultados_c2$resumen
