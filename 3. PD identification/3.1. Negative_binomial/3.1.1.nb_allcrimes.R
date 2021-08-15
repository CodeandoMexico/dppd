rm(list=ls())

# Paquetes
library("tidyverse")
library("MuMIn")
library("car")
library("effects")
library("data.table")
library("nlme")
library("lmerTest")
library("progress")
library("faraway")
library("ggplot2")
library("pscl")
library("boot")
library("lme4")
library("MASS")
library("lmtest")
library("sandwich")
library("AER") 
library("sjPlot")
library("sjmisc")
library("ggplot2")
library("performance")
library("cowplot")
library("utils")
library("readr")

#### Identificar desviaciones positivas con regresión binomial negativa y selección de modelos ####

#### Delitos de todos los niveles de severidad ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
base <- na.omit(base)

#### 1) Todos los clusters juntos #### 

# Especificar el modelo global 1 (con todas las variables independientes)
options(na.action="na.fail")
globmod<- MASS::glm.nb(allcrimes ~ POBTOT+ 
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
                         ambulantaje_por, base)
summary(globmod)

# Tabla de selección de modelo

dd <-dredge(globmod,  m.lim=c(0,4))

# Mejor modelo a partir de la tabla de selección de modelo

best_mod<- glm.nb(allcrimes ~ area_agebkm+
                    distanciamintransporte_km+
                    POBTOT+
                    serv_preparacionalimentos_km2, base)

# Resumen del modelo 1
summary(best_mod)

# ANOVA
Anova(best_mod,test="Wald")

# RMSE
rmse <- rmse(best_mod)
rmse

# Residuos del modelo 1
residuals <- residuals(best_mod)
plot(residuals(best_mod))
hist(residuals(best_mod))
summary(residuals(best_mod))

# Identificación de PDS modelo 1
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, base$allcrimes, residuals(best_mod))

# Usar la regla del rango intercuartil para detectar las desviaciones positivas
Q1<-quantile(residuals(best_mod), 0.25)
iq.data <- IQR (residuals(best_mod))

b<-Q1-(iq.data*1.5)
b
pos_dev = res_mod[which(res_mod$`residuals(best_mod)` <b), ]
pos_dev

# Otra técnica para identificar los PDs
Bp <- boxplot(residuals(best_mod))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Obtener las desviaciones positivas por cluster

# PDS modelo 1 cluster 1

# Separar los residuos por cluster
list <- split(res_mod, res_mod$`base$cluster`)
list$`1`
c1 <- list$"1"

# Descriptivos de los residuos del modelo 1 cluster 1
hist(c1$`residuals(best_mod)`)
summary(c1$`residuals(best_mod)`)
residuals <- c1$`residuals(best_mod)`

# Usar la regla del rango intercuartil para detectar las desviaciones positivas modelo 1 cluster 1
Q1<- quantile(c1$`residuals(best_mod)`, 0.25)
iq.data <- IQR (c1$`residuals(best_mod)`)

b<-Q1-(iq.data*1.5)
b
pos_dev = c1[which(c1$`residuals(best_mod)` <b), ]
pos_dev

# Otra técnica para identificar los PDs del modelo 1 cluster 1
Bp <- boxplot(c1$`residuals(best_mod)`)
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# PDS modelo 1 cluster 2
list$"2"
c2 <- list$"2"
hist(c2$`residuals(best_mod)`)

Q1<-quantile(c2$`residuals(best_mod)`, 0.25)
Q1
iq.data <- IQR (c2$`residuals(best_mod)`)
iq.data
b<-Q1-(iq.data*1.5)
b
pos_dev = c2[which(c2$`residuals(best_mod)` <b), ]
pos_dev

boxplot(c2$`residuals(best_mod)`)

# PDS modelo 1 cluster 3
list$"3"
c3 <- list$"3"
hist(c3$`residuals(best_mod)`)
boxplot(c3$`residuals(best_mod)`)

Q1<-quantile(c3$`residuals(best_mod)`, 0.25)
Q1
iq.data <- IQR (c3$`residuals(best_mod)`)
iq.data
b<-Q1-(iq.data*1.5)
b
pos_dev = c3[which(c3$`residuals(best_mod)` <b), ]
pos_dev

#### Mejor modelo por cluster ####

#### 2) Cluster 1 ####

list1 <- split(base, base$cluster)
list1
list1$"1"
clus_1<-list1$"1"

# Modelo global cluster 1

options(na.action="na.fail")

globmod_c1<- MASS::glm.nb(allcrimes ~ POBTOT+ 
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
                            ambulantaje_por, data=clus_1)

summary(globmod_c1)

# Tabla de selección de modelos cluster 1

dd1<-dredge(globmod_c1,  m.lim=c(0,4))

# Mejor modelo cluster 1 
best_mod_c1<- glm.nb(allcrimes ~ ambulantaje_por +
                       area_agebkm +
                       POBTOT +
                       serv_preparacionalimentos_km2, data=clus_1)

summary(best_mod_c1)

Anova(best_mod_c1,test="Wald")

# RMSE
rmse <- rmse(best_mod_c1)
rmse

# Residuos
residuals <- residuals(best_mod_c1)
summary(residuals)
hist(residuals(best_mod_c1))

# PDS
res_mod_c1<-cbind.data.frame(clus_1$CVEGEO, clus_1$cluster, residuals(best_mod_c1))

Q1<-quantile(residuals(best_mod_c1), 0.25)
iq.data <- IQR (residuals(best_mod_c1))

b<-Q1-(iq.data*1.5)
b
pd_allcrimes_c1 = res_mod_c1[which(res_mod_c1$`residuals(best_mod_c1)` <b), ]
pd_allcrimes_c1

Bp <- boxplot(residuals(best_mod_c1))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_allcrimes_c1 <- pd_allcrimes_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `residuals(best_mod_c1)`)

#### 3) Cluster 2 ####

list1$"2"
clus_2<-list1$"2"

# Modelo global cluster 2

options(na.action="na.fail")
globmod_c2<- MASS::glm.nb(allcrimes ~ POBTOT+ 
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
                         ambulantaje_por, data=clus_2)
summary(globmod_c2)

# Tabla de selección de modelos cluster 2

dd2<-dredge(globmod_c2,  m.lim=c(0,4))

# Mejor modelo cluster 2
best_mod_c2<- glm.nb(allcrimes ~ ambulantaje_por +
                       distanciamintransporte_km +
                       POBTOT +
                       serv_financieros, data= clus_2)

summary(best_mod_c2)

# Anova
Anova(best_mod_c2, test="Wald")

# RMSE
rmse <- rmse(best_mod_c2)
rmse

# Residuos
residuals <- residuals(best_mod_c2)
plot(residuals(best_mod_c2))
hist(residuals(best_mod_c2))
summary(residuals(best_mod_c2))

# PDS 
res_mod_c2<-cbind.data.frame(clus_2$CVEGEO,clus_2$cluster,residuals(best_mod_c2))

Q1<-quantile(residuals(best_mod_c2), 0.25)
iq.data <- IQR (residuals(best_mod_c2))

b<-Q1-(iq.data*1.5)
b
pd_allcrimes_c2 = res_mod_c2[which(res_mod_c2$residuals <b), ]
pd_allcrimes_c2

Bp <- boxplot(residuals(best_mod_c2))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_allcrimes_c2 <- pd_allcrimes_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `residuals(best_mod_c2)`)

#### 4) Cluster 3 ####

list1$"3"
clus_3<-list1$"3"

# Modelo global cluster 3
options(na.action="na.fail")
globmod_c3 <- MASS::glm.nb(allcrimes ~ POBTOT+ 
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
                            ambulantaje_por, data=clus_3)

summary(globmod_c3)

# Tabla selección de modelo cluster 3

dd3<-dredge(globmod_c3,  m.lim=c(0,4))

# Mejor modelo cluster 3 
best_mod_c3<- glm.nb(allcrimes ~ ambulantaje_por+
                       area_agebkm+
                       POBTOT+
                       serv_preparacionalimentos_km2, data=clus_3)

summary(best_mod_c3)

# RMSE
rmse <- rmse(best_mod_c3)
rmse

# Residuos
residuals <- residuals(best_mod_c3)
plot(residuals(best_mod_c3))
hist(residuals(best_mod_c3))
summary(residuals(best_mod_c3))

# PDs
res_mod_c3<-cbind.data.frame(clus_3$CVEGEO, clus_3$cluster, residuals(best_mod_c3))

Q1<-quantile(residuals(best_mod_c3), 0.25)
iq.data <- IQR (residuals(best_mod_c3))

b<-Q1-(iq.data*1.5)
b
pd_allcrimes_c3 = res_mod_c3[which(res_mod_c3$`residuals(best_mod_c3)` <b), ]
pd_allcrimes_c3

Bp <- boxplot(residuals(best_mod_c3))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_allcrimes_c3 <- pd_allcrimes_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `residuals(best_mod_c3)`)

# Crear una sola base de resultados y guardarla 
pd_allcrimes_nb <- bind_rows(pd_allcrimes_c1, pd_allcrimes_c2, pd_allcrimes_c3)

pd_allcrimes_nb <- pd_allcrimes_nb %>% 
  mutate(model = "nb",
         result_var = "allcrimes")

write.csv(pd_allcrimes_nb, "pd_allcrimes_nb.csv", row.names = F)

