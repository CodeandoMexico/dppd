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

#### Delitos de severidad 2 ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
colSums(is.na(base))# Hay 17 AGEBS que no tienen información sobre la población (el aeropuerto, panteones, la central de abastos, etc.) 
base <- na.omit(base)# Eliminamos los valores perdidos

#### 1) Todos los clusters juntos #### 

# Regresión lineal para la variable dependiente g2_log, y en las independientes incluir la variable cluster
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. Tampoco se incluye bienestar socioeconómico cuando se incluye la variable cluster porque están altamente correlacionadas.  

model1 <- lm(g2_log ~ POBTOT+
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

# Comprobación de supuestos 
shapiro.test(model1$residuals)# No se cumple el supuesto de normalidad
bptest(model1) # No se cumple el supuesto de homocedasticidad
vif(model1)  # No hay multicolinealidad alta

# Resumen modelo 1
summary(model1)

# RMSE modelo 1
RMSE_f <- function(error) { sqrt(mean(error^2)) }
RMSE <- RMSE_f(model1$residuals)
RMSE 

# Residuos modelo 1 
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, base$g2_log, predict(model1), model1$residuals)

# Obtener los PDs por clusters para el modelo 1

# Separar los residuos del modelo 1 cluster 1 
list <- split(res_mod, res_mod$`base$cluster`)
list$`1`
c1 <- list$"1"

# Descriptivos de los residuos del modelo 1 cluster 1
hist(c1$`model1$residuals`)
summary(c1$`model1$residuals`)
residuals <- c1$`model1$residuals`

# Obtener el rango intercuartil de los residuos del modelo 1 cluster 1
Q1<- quantile(c1$`model1$residuals`, 0.25)
iq.data <- IQR (c1$`model1$residuals`)
b<-Q1-(iq.data*1.5)
b

# Identificar los PDs para el modelo 1 cluster 1
PD_model1_c1 = c1[which(c1$`model1$residuals` <b), ]
PD_model1_c1

# Otra técnica para identificar los PDS modelo 1 cluster 1
Bp <- boxplot(c1$`model1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
c1[c1$`model1$residuals` %in% PDs,]

# PDs para el modelo 1 cluster 2
list$`2`
c2 <- list$"2"

hist(c2$`model1$residuals`)
summary(c2$`model1$residuals`)
residuals <- c2$`model1$residuals`

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

# PDs para el modelo 1 cluster 3
list$`3`
c3 <- list$"3"

hist(c3$`model1$residuals`)
summary(c3$`model1$residuals`)
residuals <- c3$`model1$residuals`

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


#### Modelos por cluster ####

#### 2) Cluster 1 ####

# Dividir la base en clusters
list1 <- split(base, base$cluster)
list1
list1$"1"
clus_1<-list1$"1"

# Correr el modelo para cluster 1
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c1 <- lm(g2_log ~ POBTOT+
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

# 1.3) Resumen modelo cluster 1
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

# Identificar los PDS para el modelo cluster1
Q1<- quantile(res_mod_c1$`model_c1$residuals`, 0.25)
iq.data <- IQR (res_mod_c1$`model_c1$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_g2_c1 = res_mod_c1[which(res_mod_c1$`model_c1$residuals` <b), ]
PD_g2_c1

Bp <- boxplot(res_mod_c1$`model_c1$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c1[res_mod_c1$`model_c1$residuals` %in% PDs,]

# Renombrar variables
PD_g2_c1 <- PD_g2_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `model_c1$residuals`)

#### 3) Cluster 2 ####

# Dividir la base en clusters
list1$"2"
clus_2<-list1$"2"

# Correr el modelo cluster 2
# No se incluye P_FEM, P_MAS, P_60YMAS y serv_personales_km2 para eliminar la multicolinealidad alta. 

model_c2 <- lm(g2_log ~ POBTOT+
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

# Resumen modelo cluster 2
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

# Resiudos modelo cluster 2
res_mod_c2 <- cbind.data.frame(clus_2$CVEGEO, clus_2$cluster, model_c2$residuals)

hist(res_mod_c2$`model_c2$residuals`)
summary(res_mod_c2$`model_c2$residuals`)

# Identificar PDs para el modelo cluster 2
Q1<- quantile(res_mod_c2$`model_c2$residuals`, 0.25)
iq.data <- IQR (res_mod_c2$`model_c2$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_g2_c2 = res_mod_c2[which(res_mod_c2$`model_c2$residuals` <b), ]
PD_g2_c2

Bp <- boxplot(res_mod_c2$`model_c2$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c2[res_mod_c2$`model_c2$residuals` %in% PDs,]

# Renombrar variables
PD_g2_c2 <- PD_g2_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `model_c2$residuals`)

#### 4) Cluster 3 ####

# Dividir la base en clusters
clus_3<-list1$"3"

# Correr el modelo para el cluster 3
# No se incluye la variable paradassitis_km2 porque causa multicolinealidad perfecta (ya que no hay paradas SITIS en las AGEB del cluster 3).
# Tampoco incluimos en el modelo la población de 0 a 14 años, población residente en la CDMX en 2015, comercio, servicios de entretenimiento y servicios educativos para elminar la multicolienalidad alta.

model_c3 <- lm(g2_log ~ POBTOT+
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

# Resumen del modelo cluster 3
summary(model_c3)

# Comprobación de supuestos 
shapiro.test(model_c3$residuals) # Se cumple el supuesto de normalidad
bptest(model_c3) # No se cumple el supuesto de homocedasticidad
vif(model_c3)  # No hay multicolinealidad alta

# RMSE modelo g2_log cluster 3
RMSE <- RMSE_f(model_c3$residuals)
RMSE 

# Variables más importantes
varimp <- varImp(model_c3)
arrange(varimp, -Overall)

# Residuos del modelo cluster 3
res_mod_c3 <- cbind.data.frame(clus_3$CVEGEO, clus_3$cluster, model_c3$residuals)

hist(res_mod_c3$`model_c3$residuals`)
summary(res_mod_c3$`model_c3$residuals`)

# Identificar los PDs para el modelo cluster 3
Q1<- quantile(res_mod_c3$`model_c3$residuals`, 0.25)
iq.data <- IQR (res_mod_c3$`model_c3$residuals`)
b<-Q1-(iq.data*1.5)
b

PD_g2_c3 = res_mod_c3[which(res_mod_c3$`model_c3$residuals` <b), ]
PD_g2_c3

Bp <- boxplot(res_mod_c3$`model_c3$residuals`)
Bp$out
PDs <- Bp$out[Bp$out<0]
res_mod_c3[res_mod_c3$`model_c3$residuals` %in% PDs,]

# Renombrar variables 
PD_g2_c3 <- PD_g2_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `model_c3$residuals`)

# Crear una sola base de resultados y guardarla 
PD_g2_linearreg <- bind_rows(PD_g2_c1, PD_g2_c2, PD_g2_c3)

PD_g2_linearreg <- PD_g2_linearreg %>% 
  mutate(model = "linearreg",
         result_var = "g2_log")

write.csv(PD_g2_linearreg, "pd_g2_linearreg.csv", row.names = F)

