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

#### Identificar desviaciones positivas con regresiones lineales multivariadas ####

#### Delitos de todos los niveles de severidad ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
colSums(is.na(base))# Hay 17 AGEBS que no tienen información sobre la población (el aeropuerto, panteones, escuelas, etc.) 
base <- na.omit(base)# Eliminar los valores perdidos

# Distribución de las variables dependientes
hist(base$allcrimes)
hist(base$g1)
hist(base$g2)
hist(base$g3)

# Distribución de las variables dependientes transformadas en logaritmo
hist(base$allcrimes_log)
hist(base$g1_log)
hist(base$g2_log)
hist(base$g3_log)

# Gráficas de dispersión (scatterplots) entre la variable dependiente allcrimes_log y las variables independientes
plot(base$POBTOT, base$allcrimes_log, main="Scatterplot",
     xlab="Total population", ylab="victims for all crimes", pch=19)

plot(base$area_agebkm, base$allcrimes_log, main="Scatterplot",
     xlab="Area of the AGEB", ylab="victims for all crimes", pch=19)

plot(base$P_MAS, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of male population", ylab="victims for all crimes", pch=19)

plot(base$P_FEM, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of female population", ylab="victims for all crimes", pch=19)

plot(base$P_0A14, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of 0 to 14 population", ylab="victims for all crimes", pch=19)

plot(base$P_15A24, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of 15 to 24 population", ylab="victims for all crimes", pch=19)

plot(base$P_25A59, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of 25 to 59 population", ylab="victims for all crimes", pch=19)

plot(base$P_60YMAS, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of 25 to 59 population", ylab="victims for all crimes", pch=19)

plot(base$REL_H_M, base$allcrimes_log, main="Scatterplot",
     xlab="Men for every 100 women", ylab="victims for all crimes", pch=19)

plot(base$PEA, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of economically active population", ylab="victims for all crimes", pch=19)

plot(base$HOGFEM, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of female headed household", ylab="victims for all crimes", pch=19)

plot(base$PRES2015, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of population residing in the same federal entity in the last 5 years", ylab="victims for all crimes", pch=19)

plot(base$bienestar_socioeco, base$allcrimes_log, main="Scatterplot",
     xlab="Socioeconomic wellbeing", ylab="victims for all crimes", pch=19)

plot(base$edomex, base$allcrimes_log, main="Scatterplot",
     xlab="Border with the state of Mexico", ylab="victims for all crimes", pch=19)

plot(base$total_viajes, base$allcrimes_log, main="Scatterplot",
     xlab="Total travels of commuters", ylab="victims for all crimes", pch=19)

plot(base$areaverde_por, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of the AGEB with green areas", ylab="victims for all crimes", pch=19)

plot(base$comercio_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Commerce per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_financieros_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Financial services per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_educativos_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Education services per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_salud_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Health services per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_entretenimiento_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Recreational services per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_preparacionalimentos_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Restaurants and bars per km2", ylab="victims for all crimes", pch=19)

plot(base$serv_personales_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Personal services (funeral, laundry, hairdressers) per km2", ylab="victims for all crimes", pch=19)

plot(base$viasprimarias_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Fast roads per km2", ylab="victims for all crimes", pch=19)

plot(base$viassecundarias_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Secondary roads per km2", ylab="victims for all crimes", pch=19)

plot(base$distanciamintransporte_km, base$allcrimes_log, main="Scatterplot",
     xlab="Distance to the public transport", ylab="victims for all crimes", pch=19)

plot(base$paradastrolebus_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Number of Trolebus stops per km2", ylab="victims for all crimes", pch=19)

plot(base$paradasrtp_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Number of RTP stops per km2", ylab="victims for all crimes", pch=19)

plot(base$paradassitis_km2, base$allcrimes_log, main="Scatterplot",
     xlab="Number of SITIS stops per km2", ylab="victims for all crimes", pch=19)

plot(base$sinpavimento_por, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of streets per AGEB without paving", ylab="victims for all crimes", pch=19)

plot(base$ambulantaje_por, base$allcrimes_log, main="Scatterplot",
     xlab="Percentage of streets by AGEB with street commerce", ylab="victims for all crimes", pch=19)


#### 1) Todos los clusters juntos #### 

# Regresión lineal para allcrimes_log y todas las variables independientes, incluyendo la variable cluster

model1 <- lm(allcrimes_log ~ POBTOT+ 
               P_FEM+
               P_MAS+
               area_agebkm+
               P_0A14+
               P_15A24+
               P_25A59+
               P_60YMAS+
               REL_H_M+
               PEA+
               HOGFEM+
               PRES2015+
               bienestar_socioeco+
               edomex+
               total_viajes+
               areaverde_por+
               comercio_km2+
               serv_financieros_km2+
               serv_educativos_km2+
               serv_salud_km2+
               serv_entretenimiento_km2+
               serv_preparacionalimentos_km2+
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

summary(model1)

# Evaluar los supuestos de la regresión lineal para el modelo 1

# Normalidad en los residuos 
# Visualmente
hist(model1$residuals)

# Shapiro- Wilk test (para evaluar normalidad)
shapiro.test(model1$residuals)# El p valor es menor a 0.05, no se cumple el supuesto de la normalidad en los residuos

# Homocedasticidad en los residuos
# Visualmente
# Gráfica de valores predichos contra observados (para evaluar homocedasticidad)
predict(model1)
plot(base$allcrimes_log, predict(model1))
abline(0,1)

# Breusch-Pagan test (para evaluar homocedasticidad)
bptest(model1) #El p valor es menor a 0.05, no se cumple el supuesto de homocedasticidad

# Multicolinealidad
# Hacer una matriz de correlaciones de las variables numéricas que incluimos en el modelo para ver la correlación entre las variables independientes
base_correlacion <- base %>% 
  dplyr::select(c(POBTOT, P_FEM, P_MAS, area_agebkm, P_0A14, P_15A24, P_25A59, P_60YMAS,
                  REL_H_M, PEA, HOGFEM, PRES2015, bienestar_socioeco, total_viajes, 
                  areaverde_por, comercio_km2, serv_financieros_km2, serv_educativos_km2,
                  serv_salud_km2, serv_entretenimiento_km2, serv_preparacionalimentos_km2, 
                  serv_personales_km2, viasprimarias_km2, viassecundarias_km2, 
                  distanciamintransporte_km, paradastrolebus_km2, paradasrtp_km2, 
                  paradassitis_km2, sinpavimento_por, ambulantaje_por))
    
corr <- cor(na.omit(base_correlacion)) # No incluir en el modelo las variables que están altamente correlacionadas (0.7 o más en la matriz de correlaciones)                                        

#Eliminamos del modelo el porcentage de población masculina y femenina y dejamos únicamente relación hombre- mujer, también eliminamos la población de 60 años y más porque está altamente correlacionada con la población de 0 a 14 años. 
#Eliminamos servicios personales porque está altamente correlacionado con servicios de preparación de alimentos. 

# Evaluar la multicolinealidad en el modelo después de eliminar las variables independientes altamente correlacionadas
model1 <- lm(allcrimes_log ~ POBTOT+
               area_agebkm+
               P_0A14+
               P_15A24+
               P_25A59+
               REL_H_M+
               PEA+
               HOGFEM+
               PRES2015+
               bienestar_socioeco+
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
               sinalumbrado_por+ 
               sinpavimento_por+
               ambulantaje_por+
               cluster, data=base)

# Prueba VIF (para evaluar multiocolinealidad)
car::vif(model1) #Hay multicolinealidad alta en las variables cluster y bienestar socioeconómico ya que el valor GVIF es mayor a 5. Eliminamos la variable de bienestar socioeconómico

# Volvemos a correr el modelo sin la variable bienestar socioeconómico
model1 <- lm(allcrimes_log ~ POBTOT+
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
               serv_preparacionalimentos_km2+
               viasprimarias_km2+
               viassecundarias_km2+
               distanciamintransporte_km+
               paradastrolebus_km2+
               paradasrtp_km2+
               paradassitis_km2+ 
               sinpavimento_por+
               ambulantaje_por+
               cluster, data=base)

car::vif(model1) #Ya no hay multicolinealidad alta, ningún valor de GVIF es mayor a 5

# Resumen modelo 1
summary(model1)

# RMSE modelo 1
RMSE_f <- function(error) { sqrt(mean(error^2)) }
RMSE <- RMSE_f(model1$residuals)
RMSE 

# Obtener las desviaciones positivas (PDS) para el modelo 1 por cluster

# Obtener los residuos del modelo 1 
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, model1$residuals)

# Separar los residuos por cluster
list <- split(res_mod, res_mod$`base$cluster`)
list$`1`
c1 <- list$"1"
list$`2`
c2 <- list$"2"
list$`3`
c3 <- list$"3"

# Descriptivos de los residuos del modelo 1 cluster 1
hist(c1$`model1$residuals`)
summary(c1$`model1$residuals`)

# Usar la regla del rango intercuartil para detectar los outliers
Q1<- quantile(c1$`model1$residuals`, 0.25)
iq.data <- IQR (c1$`model1$residuals`)
b<-Q1-(iq.data*1.5)
b

# Identificar los valores de los residuos que son menores al valor mínimo del rango intercuartil (outliers positivos)
PD_model1_c1 = c1[which(c1$`model1$residuals` <b), ]
PD_model1_c1

# Otra técnica para identificar los PDs
Bp <- boxplot(c1$`model1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
c1[c1$`model1$residuals` %in% PDs,]

# Resultados del modelo 1 para el cluster 1
PD_model1_c1 <- PD_model1_c1 %>% 
  rename(CVEGEO = "base$CVEGEO",
         cluster = "base$cluster",
         residuals = "model1$residuals")

# PDs para el modelo 1 cluster 2
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
c2[c2$`model1$residuals` %in% PDs,]

# Resultados del modelo 1 para el cluster 2
PD_model1_c2 <- PD_model1_c2 %>% 
  rename(CVEGEO = "base$CVEGEO",
         cluster = "base$cluster",
         residuals = "model1$residuals")

# PDs para el modelo 1 cluster 3
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

# Resultados del modelo 1 para el cluster 3
PD_model1_c3 <- PD_model1_c3 %>% 
  rename(CVEGEO = "base$CVEGEO",
         cluster = "base$cluster",
         residuals = "model1$residuals")

# Guardar los resultados del modelo 1, PDS por cluster
PD_model1_allcrimes <- bind_rows(PD_model1_c1, PD_model1_c2, PD_model1_c3)

PD_model1_allcrimes <- PD_model1_allcrimes %>% 
  mutate(model = "linearreg_generalmodel",
         result_var = "allcrimes_log")


#### Modelos por cluster ####

#### 2) Cluster 1 ####

# Dividir la base en clusters
list1 <- split(base, base$cluster)
list1
list1$"1"
clus_1<-list1$"1"

# Correr el modelo para cluster 1
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c1 <- lm(allcrimes_log ~ POBTOT+ 
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 PRES2015+
                 bienestar_socioeco+
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

# Comprobación de supuestos 
shapiro.test(model_c1$residuals)# No se cumple el supuesto de normalidad
bptest(model_c1) # No se cumple el supuesto de homocedasticidad
vif(model_c1)  #No hay multicolinealidad alta

# Resumen modelo cluster 1
summary(model_c1)

# RMSE modelo cluster 1
RMSE <- RMSE_f(model_c1$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c1)
arrange(varimp, -Overall)

# Residuos del modelo cluster 1
res_mod_c1 <- cbind.data.frame(clus_1$CVEGEO, clus_1$cluster, model_c1$residuals)

# Identificar los PDS para el modelo cluster 1
hist(res_mod_c1$`model_c1$residuals`)
summary(res_mod_c1$`model_c1$residuals`)

Q1<- quantile(res_mod_c1$`model_c1$residuals`, 0.25)
iq.data <- IQR (res_mod_c1$`model_c1$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_allcrimes_c1 = res_mod_c1[which(res_mod_c1$`model_c1$residuals` <b), ]
PD_allcrimes_c1

Bp <- boxplot(res_mod_c1$`model_c1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c1[res_mod_c1$`model_c1$residuals` %in% PDs,]

# Renombrar variables 
PD_allcrimes_c1 <- PD_allcrimes_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `model_c1$residuals`)

#### 3) Cluster 2 ####

# Dividir la base en clusters
clus_2<-list1$"2"

# Correr el modelo para el cluster 2
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c2 <- lm(allcrimes_log ~ POBTOT+ 
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 PRES2015+
                 bienestar_socioeco+
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

# Comprobación de supuestos 
shapiro.test(model_c2$residuals)# No se cumple el supuesto de normalidad
bptest(model_c2) # No se cumple el supuesto de homocedasticidad
vif(model_c2)  #No hay multicolinealidad alta

# Resumen del modelo cluster 2
summary(model_c2)

# RMSE modelo cluster 2
RMSE <- RMSE_f(model_c2$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c2)
arrange(varimp, -Overall)

# Residuos del modelo cluster 2
res_mod_c2 <- cbind.data.frame(clus_2$CVEGEO, clus_2$cluster, model_c2$residuals)

# Identificar PDs para el modelo cluster 2
hist(res_mod_c2$`model_c2$residuals`)
summary(res_mod_c2$`model_c2$residuals`)

Q1<- quantile(res_mod_c2$`model_c2$residuals`, 0.25)
iq.data <- IQR (res_mod_c2$`model_c2$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_allcrimes_c2 = res_mod_c2[which(res_mod_c2$`model_c2$residuals` <b), ]
PD_allcrimes_c2

Bp <- boxplot(res_mod_c2$`model_c2$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c2[res_mod_c2$`model_c2$residuals` %in% PDs,]

# Renombrar variables
PD_allcrimes_c2 <- PD_allcrimes_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `model_c2$residuals`)

#### 4) Cluster 3 ####

# Dividir la base en clusters
clus_3<-list1$"3"

# Correr el modelo para cluster 3
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c3 <- lm(allcrimes_log ~ POBTOT+ 
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 PRES2015+
                 bienestar_socioeco+
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
                 ambulantaje_por, data=clus_3)

# Resumen del modelo cluster 3
summary(model_c3)

# Comprobación de supuestos 
shapiro.test(model_c3$residuals)# Se cumple el supuesto de normalidad
bptest(model_c3) # Se cumple el supuesto de homocedasticidad
vif(model_c3)  # Hay multicolinealidad perfecta en paradassitis_km2 porque no hay valores de esta variable en el cluster 3, la quitamos del modelo. 

# Volvemos a correr el modelo sin las paradas SITIS
model_c3 <- lm(allcrimes_log ~ POBTOT+ 
                 area_agebkm+
                 P_0A14+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 PRES2015+
                 bienestar_socioeco+
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
                 sinpavimento_por+
                 ambulantaje_por, data=clus_3)

vif(model_c3) # Hay multicolinealidad alta en varias variables, quitamos algunas del modelo para eliminar la multicolinealidad alta.

# Volvemos a correr el modelo sin población de 0 a 14 años, población residente en la CDMX en 2015, comercio, servicios de entretenimiento y servicios educativos para eliminar la multicolinealidad alta en el modelo.
model_c3 <- lm(allcrimes_log ~ POBTOT+
                 area_agebkm+
                 P_15A24+
                 P_25A59+
                 REL_H_M+
                 PEA+
                 HOGFEM+
                 bienestar_socioeco+
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
                 sinpavimento_por+
                 ambulantaje_por, data=clus_3)

vif(model_c3) # Ya no hay multicolinealidad alta
shapiro.test(model_c3$residuals)# Se cumple el supuesto de normalidad
bptest(model_c3) # Se cumple el supuesto de homocedasticidad

# Resumen del modelo cluster 3
summary(model_c3)

# RMSE del modelo cluster 3
RMSE <- RMSE_f(model_c3$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c3)
arrange(varimp, -Overall)

# Residuos modelo cluster 3
res_mod_c3 <- cbind.data.frame(clus_3$CVEGEO, clus_3$cluster, model_c3$residuals)

hist(res_mod_c3$`model_c3$residuals`)
summary(res_mod_c3$`model_c3$residuals`)

# Identificar PDs para el modelo cluster 3
Q1<- quantile(res_mod_c3$`model_c3$residuals`, 0.25)
iq.data <- IQR (res_mod_c3$`model_c3$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_allcrimes_c3 = res_mod_c3[which(res_mod_c3$`model_c3$residuals` <b), ]
PD_allcrimes_c3

Bp <- boxplot(res_mod_c3$`model_c3$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c3[res_mod_c3$`model_c3$residuals` %in% PDs,]

# Renombrar variables
PD_allcrimes_c3 <- PD_allcrimes_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `model_c3$residuals`)

# Crear una sola base de resultados de los modelos por cluster y guardarla 
PD_allcrimes_linearreg <- bind_rows(PD_allcrimes_c1, PD_allcrimes_c2, PD_allcrimes_c3)

PD_allcrimes_linearreg <- PD_allcrimes_linearreg %>% 
  mutate(model = "linearreg",
         result_var = "allcrimes_log")

write.csv(PD_allcrimes_linearreg, "pd_allcrimes_linearreg.csv", row.names = F)

