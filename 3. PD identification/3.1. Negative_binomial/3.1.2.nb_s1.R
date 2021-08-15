rm(list=ls())

# Paquetes
library("MuMIn")
library("car")
library("effects")
library("data.table")
library("nlme")
library("lmerTest")
library("progress")
library("effects")
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
library("performance")
library("readr")
library("tidyverse")

#### Identificar desviaciones positivas con regresión binomial negativa y selección de modelos ####

#### Delitos de severidad 1 ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
base <- na.omit(base)

#### 1) Todos los clusters juntos #### 

# Modelo global 1
options(na.action="na.fail")

globmod<- MASS::glm.nb(g1 ~ POBTOT+ 
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

# Tabla de selección de modelo 1

dd <-dredge(globmod,  m.lim=c(0,4))

# Mejor modelo 1
best_mod<- glm.nb(g1 ~ ambulantaje_por +
                    area_agebkm + 
                    POBTOT + 
                    serv_preparacionalimentos_km2, base)

summary(best_mod)
Anova(best_mod,test="Wald")

# Residuos
plot(best_mod$residuals)
hist(best_mod$residuals)
summary(best_mod$residuals)

# RMSE
rmse <- rmse(best_mod)
rmse

# PDS
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, base$g1, residuals(best_mod))

hist(residuals(best_mod))
Q1<-quantile(residuals(best_mod), 0.25)
iq.data <- IQR (residuals(best_mod))

b<-Q1-(iq.data*1.5)
b
pos_dev = res_mod[which(res_mod$residuals <b), ]
pos_dev

Bp <- boxplot(res_mod$residuals)
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Obtener las desviaciones positivas por cluster

# PDS modelo 1 cluster 1
list1 <- split(res_mod, res_mod$"base$cluster")
c1 <- list1$"1"
hist(c1$residuals)

Q1<-quantile(c1$residuals, 0.25)
iq.data <- IQR (c1$residuals)
b<-Q1-(iq.data*1.5)
b
pos_dev = c1[which(c1$residuals <b), ]
pos_dev

boxplot(c1$residuals)

# PDS modelo 1 cluster 2
c2 <- list1$"2"
hist(c2$residuals)

Q1<-quantile(c2$residuals, 0.25)
Q1
iq.data <- IQR (c2$residuals)
iq.data
b<-Q1-(iq.data*1.5)
b
pos_dev = c2[which(c2$residuals <b), ]
pos_dev

# PDS modelo 1 cluster 3
c3 <- list1$"3"
hist(c3$residuals)
boxplot(c3$residuals)

Q1<-quantile(c3$residuals, 0.25)
Q1
iq.data <- IQR (c3$residuals)
iq.data
b<-Q1-(iq.data*1.5)
b
pos_dev = c3[which(c3$residuals <b), ]
pos_dev

#### Mejores modelos por cluster ####

#### 2) Cluster 1 #### 

list1 <- split(base, base$cluster)
list1
list1$"1"
clus_1<-list1$"1"

options(na.action="na.fail")

# Modelo global cluster 1

globmod_c1<- MASS::glm.nb(g1 ~ POBTOT+ 
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

# Tabla de selección de modelo cluster 1

dd1 <-dredge(globmod_c1,  m.lim=c(0,4))

# Mejor modelo cluster 1

summary(best_mod_c1<- glm.nb(g1 ~ ambulantaje_por +
                               area_agebkm +
                               POBTOT+
                               serv_preparacionalimentos_km2, data=clus_1))
Anova(best_mod_c1,test="Wald")

# RMSE
rmse <- rmse(best_mod_c1)
rmse

# PDS 
residuals <- residuals(best_mod_c1)
summary(residuals)

res_mod_c1<-cbind.data.frame(clus_1$CVEGEO,clus_1$cluster,residuals(best_mod_c1))

hist(residuals(best_mod_c1))
Q1<-quantile(residuals(best_mod_c1), 0.25)
iq.data <- IQR (residuals(best_mod_c1))

b<-Q1-(iq.data*1.5)
b
pd_g1_c1 = res_mod_c1[which(res_mod_c1$residuals <b), ]
pd_g1_c1

Bp <- boxplot(res_mod_c1$residuals)
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_g1_c1 <- pd_g1_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `residuals(best_mod_c1)`)

#### 3) Cluster 2 ####

clus_2<-list1$"2"

# Modelo global cluster 2
options(na.action="na.fail")
globmod_c2<- MASS::glm.nb(g1 ~ POBTOT+ 
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

# Tabla de selección de modelo cluster 2

dd2<-dredge(globmod_c2,  m.lim=c(0,4))

# Mejor modelo cluster 2
summary(best_mod_c2<- glm.nb(g1 ~ area_agebkm +
                               POBTOT +
                               serv_financieros_km2 +
                               serv_preparacionalimentos_km2, data=clus_2))

Anova(best_mod_c2,test="Wald")

# RMSE
rmse <- rmse(best_mod_c2)
rmse

# PDS
residuals <- residuals(best_mod_c2)
summary(residuals)
hist(residuals)

res_mod_c2<-cbind.data.frame(clus_2$CVEGEO,clus_2$cluster,residuals(best_mod_c2))

hist(residuals(best_mod_c2))
Q1<-quantile(residuals(best_mod_c2), 0.25)
iq.data <- IQR (residuals(best_mod_c2))

b<-Q1-(iq.data*1.5)
b
pd_g1_c2 = res_mod_c2[which(res_mod_c2$residuals <b), ]
pd_g1_c2

Bp <- boxplot(residuals(best_mod_c2))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_g1_c2 <- pd_g1_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `residuals(best_mod_c2)`)


#### 4) cluster 3 ####

clus_3<-list1$"3"

# Modelo global cluster 3
options(na.action="na.fail")
globmod_c3<- MASS::glm.nb(g1 ~ POBTOT+ 
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

# Tabla de selección de modelo cluster 3

dd3_g1<-dredge(globmod_c3,  m.lim=c(0,4))

# Mejor modelo cluster 3
summary(best_mod_c3<- glm.nb(g1 ~ POBTOT +
                               serv_personales_km2 + 
                               serv_preparacionalimentos_km2 + 
                               viassecundarias, data=clus_3))

Anova(best_mod_c3,test="Wald")

# RMSE
rmse <- rmse(best_mod_c3)
rmse

# PDS
residuals <- residuals(best_mod_c3)
summary(residuals)
hist(residuals)

res_mod_c3<-cbind.data.frame(clus_3$CVEGEO,clus_3$cluster,residuals(best_mod_c3))

Q1<-quantile(residuals(best_mod_c3), 0.25)
iq.data <- IQR (residuals(best_mod_c3))

b<-Q1-(iq.data*1.5)
b
pd_g1_c3 = res_mod_c3[which(res_mod_c3$`residuals(best_mod_c3)` <b), ]
pd_g1_c3

Bp <- boxplot(residuals(best_mod_c3))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar las variables
pd_g1_c3 <- pd_g1_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `residuals(best_mod_c3)`)

# Crear una sola base de resultados y guardarla 
pd_g1_nb <- bind_rows(pd_g1_c1, pd_g1_c2, pd_g1_c3)

pd_g1_nb <- pd_g1_nb %>% 
  mutate(model = "nb",
         result_var = "g1")

write.csv(pd_g1_nb, "pd_g1_nb.csv", row.names = F)

