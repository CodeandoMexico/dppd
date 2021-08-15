rm(list=ls())

#Paquetes
library(readr)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(MASS)
library(caret)
library(ggpubr)
library(performance)
library(car)
library(lmtest)
library(sp)
library(rgeos)
library(rgdal)

#### Identificar desviaciones positivas con regresiones lineales multivariadas ####

#### Delitos de severidad 3 ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
colSums(is.na(base))# Hay 17 AGEBS que no tienen información sobre la población (el aeropuerto, panteones, la central de abastos, etc.) 
base <- na.omit(base)# Eliminamos los valores perdidos

#### 1) Todos los clusters juntos #### 

# Correr la regresión lineal para la variable dependiente g3_log, y en las independientes incluir la variable cluster.
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. Tampoco se incluye bienestar socioeconómico cuando se incluye la variable cluster porque están altamente correlacionadas.  
model1 <- lm(g3_log ~ POBTOT+
               area_agebkm+
               P_0A14+
               P_15A24+
               P_25A59+
               REL_H_M+
               PEA+
               HOGFEM+
               PRES2015+
               edomex+
               total_viajes+
               areaverde_por+
               comercio_km2+
               serv_financieros_km2+
               serv_educativos_km2+
               serv_salud_km2+
               serv_entretenimiento_km2+
               serv_personales_km2+
               viasprimarias_km2+
               viassecundarias_km2+
               distanciamintransporte_km+
               paradastrolebus_km2+
               paradasrtp_km2+
               paradassitis_km2+ 
               sinpavimento_por+
               ambulantaje_por+
               cluster, data=base)

# Resumen modelo 1
summary(model1)

# Comprobación de supuestos 
shapiro.test(model1$residuals)# No se cumple el supuesto de normalidad
bptest(model1) # No se cumple el supuesto de homocedasticidad
vif(model1)  # No hay multicolinealidad alta

# RMSE modelo 1
RMSE_f <- function(error) { sqrt(mean(error^2)) }
RMSE <- RMSE_f(model1$residuals)
RMSE 

# Residuos modelo 1 
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, base$g3_log, predict(model1), model1$residuals)

# Obtener los PDs por clusters para el modelo 1

# Separar los residuos del cluster 1 
list <- split(res_mod, res_mod$`base$cluster`)
list$`1`
c1 <- list$"1"

# Descriptivos de los residuos modelo 1 cluster 1
hist(c1$`model1$residuals`)
summary(c1$`model1$residuals`)

# Obtener el rango intercuartil de los residuos del modelo 1 cluster 1
Q1<- quantile(c1$`model1$residuals`, 0.25)
iq.data <- IQR (c1$`model1$residuals`)
b<-Q1-(iq.data*1.5)
b

# Identificar los PDS del modelo 1 cluster 1
PD_model1_c1 = c1[which(c1$`model1$residuals` <b), ]
PD_model1_c1

# Otra técnica para identificar los PDS del modelo 1 cluster 1
Bp <- boxplot(c1$`model1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
c1[c1$`model1$residuals` %in% PDs,]

# PDS para el modelo 1 cluster 2
list$`2`
c2 <- list$"2"

hist(c2$`model1$residuals`)
summary(c2$`model1$residuals`)

Q1<- quantile(c2$`model1$residuals`, 0.25)
iq.data <- IQR (c2$`model1$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_model1_c2 = c2[which(c2$`model1$residuals` <b), ]
PD_model1_c2

Bp <- boxplot(c2$`model1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
c1[c1$`model1$residuals` %in% PDs,]

# PDS para el modelo 1 cluster 3
list$`3`
c3 <- list$"3"

hist(c3$`model1$residuals`)
summary(c3$`model1$residuals`)

Q1<- quantile(c3$`model1$residuals`, 0.25)
iq.data <- IQR (c3$`model1$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_model1_c3 = c3[which(c3$`model1$residuals` <b), ]
PD_model1_c3

Bp <- boxplot(c3$`model1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
c3[c3$`model1$residuals` %in% PDs,]


####  Modelos por cluster ####

#### 2) Cluster 1 ####

# Dividir la base en clusters
list1 <- split(base, base$cluster)
list1
list1$"1"
clus_1<-list1$"1"

# Correr el modelo cluster 1
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c1 <- lm(g3_log ~ POBTOT+
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 bienestar_socioeco+
                 PRES2015+
                 edomex+
                 total_viajes+
                 areaverde_por+
                 comercio_km2+
                 serv_financieros_km2+
                 serv_educativos_km2+
                 serv_salud_km2+
                 serv_entretenimiento_km2+
                 serv_preparacionalimentos_km2+
                 viasprimarias_km2+
                 viassecundarias_km2+
                 distanciamintransporte_km+
                 paradastrolebus_km2+
                 paradasrtp_km2+
                 paradassitis_km2+ 
                 sinpavimento_por+
                 ambulantaje_por, data=clus_1)

# Resumen modelo cluster 1
summary(model_c1)

# Comprobación de supuestos 
shapiro.test(model_c1$residuals)# No se cumple el supuesto de normalidad
bptest(model_c1) # No se cumple el supuesto de homocedasticidad
vif(model_c1)  # No hay multicolinealidad alta

# RMSE modelo cluster 1
RMSE <- RMSE_f(model_c1$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c1)
arrange(varimp, -Overall)

# Residuos modelo cluster 1
res_mod_c1 <- cbind.data.frame(clus_1$CVEGEO, clus_1$cluster, model_c1$residuals)

hist(res_mod_c1$`model_c1$residuals`)
summary(res_mod_c1$`model_c1$residuals`)

# Identificar los PDs para el modelo cluster1
Q1<- quantile(res_mod_c1$`model_c1$residuals`, 0.25)
iq.data <- IQR (res_mod_c1$`model_c1$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_g3_c1 = res_mod_c1[which(res_mod_c1$`model_c1$residuals` <b), ]
PD_g3_c1

Bp <- boxplot(res_mod_c1$`model_c1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c1[res_mod_c1$`model_c1$residuals` %in% PDs,]

# Renombrar variables
PD_g3_c1 <- PD_g3_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `model_c1$residuals`)

#### 3) Cluster 2 ####

# Dividir la base en clusters
list1$"2"
clus_2<-list1$"2"

# Correr el modelo cluster 2
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c2 <- lm(g3_log ~ POBTOT+
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 bienestar_socioeco+
                 PRES2015+
                 edomex+
                 total_viajes+
                 areaverde_por+
                 comercio_km2+
                 serv_financieros_km2+
                 serv_educativos_km2+
                 serv_salud_km2+
                 serv_entretenimiento_km2+
                 serv_preparacionalimentos_km2+
                 viasprimarias_km2+
                 viassecundarias_km2+
                 distanciamintransporte_km+
                 paradastrolebus_km2+
                 paradasrtp_km2+
                 paradassitis_km2+ 
                 sinpavimento_por+
                 ambulantaje_por, data=clus_2)

# Resumen del modelo cluster 2
summary(model_c2)

# Comprobación de supuestos 
shapiro.test(model_c2$residuals)# No se cumple el supuesto de normalidad
bptest(model_c2) # No se cumple el supuesto de homocedasticidad
vif(model_c2)  # No hay multicolinealidad alta

# RMSE modelo cluster 2
RMSE <- RMSE_f(model_c2$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c2)
arrange(varimp, -Overall)

# Residuos modelo cluster 2
res_mod_c2 <- cbind.data.frame(clus_2$CVEGEO, clus_2$cluster, model_c2$residuals)

hist(res_mod_c2$`model_c2$residuals`)
summary(res_mod_c2$`model_c2$residuals`)

# Identificar los PDs para el modelo cluster 2
Q1<- quantile(res_mod_c2$`model_c2$residuals`, 0.25)
iq.data <- IQR (res_mod_c2$`model_c2$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_g3_c2 = res_mod_c2[which(res_mod_c2$`model_c2$residuals` <b), ]
PD_g3_c2

Bp <- boxplot(res_mod_c2$`model_c2$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c2[res_mod_c2$`model_c2$residuals` %in% PDs,]

# Renombrar variables
PD_g3_c2 <- PD_g3_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `model_c2$residuals`)

#### 4) Cluster 3 ####

# Dividir la base en clusters
clus_3<-list1$"3"

# Correr el modelo cluster 3 sin la variable de paradassitis_km2 porque causa multicolinealidad perfecta (no hay paradas SITIS en estas AGEBS).
# Tampoco incluimos en el modelo la población de 0 a 14 años, población residente en la CDMX en 2015, comercio, servicios de entretenimiento y servicios educativos para elminar la multicolienalidad alta.

model_c3 <- lm(g3_log ~ POBTOT+
                 area_agebkm+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 edomex+
                 total_viajes+
                 areaverde_por+
                 serv_financieros_km2+
                 serv_salud_km2+
                 serv_preparacionalimentos_km2+
                 viasprimarias_km2+
                 viassecundarias_km2+
                 distanciamintransporte_km+
                 paradastrolebus_km2+
                 paradasrtp_km2+
                 sinalumbrado_por+ 
                 sinpavimento_por+
                 ambulantaje_por, data=clus_3)

# Resumen modelo cluster 3
summary(model_c3)

# Comprobación de supuestos 
shapiro.test(model_c3$residuals)# No se cumple el supuesto de normalidad
bptest(model_c3) # No se cumple el supuesto de homocedasticidad
vif(model_c3)  # No hay multicolinealidad alta

# RMSE modelo g3_log cluster 3
RMSE <- RMSE_f(model_c3$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c3)
arrange(varimp, -Overall)

# Resiudos modelo g3_log cluster 3
res_mod_c3 <- cbind.data.frame(clus_3$CVEGEO, clus_3$cluster, model_c3$residuals)

hist(res_mod_c3$`model_c3$residuals`)
summary(res_mod_c3$`model_c3$residuals`)

# Identificar los PDs para el modelo cluster 3
Q1<- quantile(res_mod_c3$`model_c3$residuals`, 0.25)
iq.data <- IQR (res_mod_c3$`model_c3$residuals`)

b<-Q1-(iq.data*1.5)
b

PD_g3_c3 = res_mod_c3[which(res_mod_c3$`model_c3$residuals` <b), ]
PD_g3_c3

Bp <- boxplot(res_mod_c3$`model_c3$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c3[res_mod_c3$`model_c3$residuals` %in% PDs,]

# Renombrar variables
PD_g3_c3 <- PD_g3_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `model_c3$residuals`)

# Crear una sola base de resultados y guardarla 
PD_g3_linearreg <- bind_rows(PD_g3_c1, PD_g3_c2, PD_g3_c3)

PD_g3_linearreg <- PD_g3_linearreg %>% 
  mutate(model = "linearreg",
         result_var = "g3_log")

write.csv(PD_g3_linearreg, "PD_g3_linearreg.csv", row.names = F)

#### 5) Unir los PDs de las regresiones lineales para allcrimes, severidad 1, severidad 2 y severidad 3 ####

# Abrir las bases de PDs creadas en los scripts para todos los niveles de severidad, severidad 1 y severidad 2
PD_allcrimes_linearreg <- read_csv("pd_allcrimes_linearreg.csv")
PD_g1_linearreg <- read_csv("pd_g1_linearreg.csv")
PD_g2_linearreg <- read_csv("pd_g2_linearreg.csv")

# Juntar las bases y guardarla
PD_linearreg <- rbind(PD_g1_linearreg, PD_g2_linearreg, PD_g3_linearreg, PD_allcrimes_linearreg)

write.csv(PD_linearreg, "pd_linearreg.csv", row.names = F)

#### 6) Obtener el número de PDS por nivel de severidad y clusters ####

# PDS total (todos los niveles de severidad más severidad 1, 2 y 3)
n_distinct(PD_linearreg$CVEGEO) 

# PDS para todos los niveles de severidad
PD_linearreg_allcrimes <- PD_linearreg %>% 
  filter(result_var== "allcrimes_log")

n_distinct(PD_linearreg_allcrimes$CVEGEO)

# PDs por nivel de severidad
PD_linearreg_porseveridad <- PD_linearreg %>% 
  filter(result_var== "g1_log" | result_var== "g2_log" | result_var== "g3_log")

n_distinct(PD_linearreg_porseveridad$CVEGEO)

# PDS por cluster para los modelos con los delitos separados por nivel de severidad
PD_linearreg_porseveridad <- PD_linearreg_porseveridad %>% 
  group_by(cluster) %>% 
  summarise(total= n())

PD_linearreg_porseveridad

# PDs por cluster para todos los niveles de severidad
PD_linearreg_allcrimes <- PD_linearreg_allcrimes %>% 
  group_by(cluster) %>% 
  summarise(total= n())

PD_linearreg_allcrimes

#### 7) Mapa de PDs regresión lineal ####

# Abrir el marco geoestadistico 2020 de la CDMX a nivel AGEB
# Abrir con readOGR
agebs <- readOGR("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp")
proj4string(agebs)

# Reproyectar con spTransform
agebs <- sp::spTransform(agebs, CRS("+proj=longlat +datum=WGS84 +no_defs"))
proj4string(agebs)

# Convertir a sf y especificar que el sistema de referencia de coordenadas (CRS) sea 4326
agebs_sf <- st_as_sf(agebs)
st_crs(agebs_sf)

st_crs(agebs_sf) = st_crs(4326)
st_crs(agebs_sf)

# Crear una variable para identificar los PDs por tipo de variable de resultado
PD_linearreg <- PD_linearreg %>% 
  mutate(PD = ifelse (result_var == "g1_log", "severity 1",
                      ifelse(result_var == "g2_log", "severity 2",
                             ifelse(result_var == "g3_log", "severity 3", "all crimes"))))

# Unir AGEBS con los PDs
pd <- left_join(agebs_sf, PD_linearreg [,c("CVEGEO", "PD")], by="CVEGEO")

pd <- pd %>% 
  mutate(PD = ifelse (PD %in% NA, "nonPD", PD))

# Inlcuir la variable cluster
pd <- left_join(pd, base[,c("CVEGEO", "cluster")], by="CVEGEO")
pd$cluster <- as.factor(pd$cluster)

# Identificar las AGEBS que son PDS en más de un modelo de regresión lineal
pd <- pd %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

pd <- pd %>% 
  mutate(PD = ifelse (duplicado == 2, "duplicate", PD))

# Ordenar la variable PD
pd$PD <- factor(pd$PD, ordered= TRUE,
                   levels = c("severity 1", "severity 2", "severity 3", "all crimes", "duplicate", "nonPD"))

# Mapa
pd %>% 
  filter(!(cluster %in% NA)) %>% 
  ggplot() +
  geom_sf(aes(color = cluster, fill= PD), size= 1.3)+
  scale_fill_manual(values = c("#ff006e", "#2d00f7", "#006400", "#ffb703", "#001427", "transparent"))+ 
  scale_color_manual(values = c("#ff85a1", "#0ead69", "#4361ee", "#7d8597"))+
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=50),
        legend.text = element_text(size=40))

ggsave("mapa_pds_linearreg.jpg", width = 30, height = 35)

