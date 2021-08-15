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
library("readr")
library("tidyverse")
library("rgdal")
library("sf")
library("sp")

#### Identificar desviaciones positivas con regresión binomial negativa y selección de modelos ####

#### Delitos de severidad 3 ####

# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

# Especificar las variables que son factores
base <- transform(base, edomex=as.factor(edomex), cluster=as.factor(cluster))

# Eliminar los valores perdidos 
base <- na.omit(base)

#### 1) Todos los clusters juntos #### 

# Modelo global 1

options(na.action="na.fail")
globmod<- MASS::glm.nb(g3 ~ POBTOT+ 
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
best_mod<- glm.nb(g3 ~ area_agebkm +
                    distanciamintransporte_km+
                    serv_financieros_km2+
                    serv_preparacionalimentos_km2, base)

summary(best_mod)

Anova(best_mod,test="Wald")

r2(best_mod)

# RMSE
rmse <- rmse(best_mod, normalized = T, verbose = TRUE)
rmse

# Residuos
residuals <- residuals(best_mod)
plot(residuals(best_mod))
hist(residuals(best_mod))
summary(residuals(best_mod))


# PDS
res_mod <- cbind.data.frame(base$CVEGEO, base$cluster, base$g3, residuals(best_mod))

Q1<-quantile(residuals(best_mod), 0.25)
iq.data <- IQR (residuals(best_mod))

b<-Q1-(iq.data*1.5)
b
pos_dev = res_mod[which(res_mod$`residuals(best_mod)` <b), ]
pos_dev

Bp <- boxplot(residuals(best_mod))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Obtener las desviaciones positivas por cluster

# PDS modelo 1 cluster 1

list <- split(res_mod, res_mod$`base$cluster`)
list$`1`
c1 <- list$"1"
hist(c1$`residuals(best_mod)`)
summary(c1$`residuals(best_mod)`)
residuals <- c1$`residuals(best_mod)`

Q1<- quantile(c1$`residuals(best_mod)`, 0.25)
iq.data <- IQR (c1$`residuals(best_mod)`)

b<-Q1-(iq.data*1.5)
b
pos_dev = c1[which(c1$`residuals(best_mod)` <b), ]
pos_dev

boxplot(c1$`residuals(best_mod)`)

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
globmod_c1<- MASS::glm.nb(g3 ~ POBTOT+ 
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

dd1_g3 <-dredge(globmod_c1,  m.lim=c(0,4))

# Mejor modelo cluster 1

summary(best_mod_c1<- glm.nb(g3 ~ area_agebkm+
                               distanciamintransporte_km+
                               serv_financieros_km2+
                               serv_preparacionalimentos_km2, data=clus_1))
Anova(best_mod_c1,test="Wald")

# RMSE
rmse <- rmse(best_mod_c1)
rmse

# PDS
residuals <- residuals(best_mod_c1)
summary(residuals)
hist(residuals(best_mod_c1))

res_mod_c1<-cbind.data.frame(clus_1$CVEGEO, clus_1$cluster, residuals(best_mod_c1))

hist(residuals(best_mod_c1))
Q1<-quantile(residuals(best_mod_c1), 0.25)
iq.data <- IQR (residuals(best_mod_c1))
iq.data

b<-Q1-(iq.data*1.5)
b
pd_g3_c1 = res_mod_c1[which(res_mod_c1$`residuals(best_mod_c1)` <b), ]
pd_g3_c1

Bp <- boxplot(residuals(best_mod_c1))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_g3_c1 <- pd_g3_c1 %>% 
  rename(CVEGEO = `clus_1$CVEGEO`,
         cluster = `clus_1$cluster`,
         residuals = `residuals(best_mod_c1)`)

#### 3) Cluster 2 ####
list1$"2"
clus_2<-list1$"2"

# Modelo global cluster 2 

options(na.action="na.fail")
globmod_c2<- MASS::glm.nb(g3 ~ POBTOT+ 
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

dd2_g3<-dredge(globmod_c2,  m.lim=c(0,4))

# Mejor modelo cluster 2
best_mod_c2<- glm.nb(g3 ~ area_agebkm +
                       distanciamintransporte_km +
                       serv_financieros_km2 +
                       serv_preparacionalimentos_km2, data=clus_2)

summary(best_mod_c2)
Anova(best_mod_c2,test="Wald")

# RMSE
rmse <- rmse(best_mod_c2)
rmse

residuals <- residuals(best_mod_c2)
plot(residuals)
hist(residuals)
summary(residuals)

# PDs 
res_mod_c2<-cbind.data.frame(clus_2$CVEGEO, clus_2$cluster,residuals(best_mod_c2))

hist(residuals(best_mod_c2))
Q1<-quantile(residuals(best_mod_c2), 0.25)
iq.data <- IQR (residuals(best_mod_c2))

b<-Q1-(iq.data*1.5)
b
pd_g3_c2 = res_mod_c2[which(res_mod_c2$residuals <b), ]
pd_g3_c2

Bp <- boxplot(residuals(best_mod_c2))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar variables
pd_g3_c2 <- pd_g3_c2 %>% 
  rename(CVEGEO = `clus_2$CVEGEO`,
         cluster = `clus_2$cluster`,
         residuals = `residuals(best_mod_c2)`)

#### 4) Cluster 3 ####
list1$"3"
clus_3<-list1$"3"

# Modelo global cluster 3

options(na.action="na.fail")
globmod_c3<- MASS::glm.nb(g3 ~ POBTOT+ 
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

dd3_g3<-dredge(globmod_c3,  m.lim=c(0,4))

# Mejor modelo cluster 3

best_mod_c3<- glm.nb(g3 ~ area_agebkm +
                       distanciamintransporte_km +
                       serv_educativos_km2+
                       serv_preparacionalimentos_km2, data=clus_3)

summary(best_mod_c3)

# RMSE
rmse <- rmse(best_mod_c3)
rmse

# PDs
residuals <- residuals(best_mod_c3)
plot(residuals(best_mod_c3))
hist(residuals(best_mod_c3))
summary(residuals(best_mod_c3))

res_mod_c3<-cbind.data.frame(clus_3$CVEGEO, clus_3$cluster, residuals(best_mod_c3))

Q1<-quantile(residuals(best_mod_c3), 0.25)
iq.data <- IQR (residuals(best_mod_c3))

b<-Q1-(iq.data*1.5)
b
pd_g3_c3 = res_mod_c3[which(res_mod_c3$`residuals(best_mod_c3)` <b), ]
pd_g3_c3

Bp <- boxplot(residuals(best_mod_c3))
Bp$out
PDs <- Bp$out[Bp$out<0]
PDs

# Renombrar
pd_g3_c3 <- pd_g3_c3 %>% 
  rename(CVEGEO = `clus_3$CVEGEO`,
         cluster = `clus_3$cluster`,
         residuals = `residuals(best_mod_c3)`)

# Crear una sola base de resultados y guardarla 
pd_g3_nb <- bind_rows(pd_g3_c1, pd_g3_c2, pd_g3_c3)

pd_g3_nb <- pd_g3_nb %>% 
  mutate(model = "nb",
         result_var = "g3")

write.csv(pd_g3_nb, "pd_g3_nb.csv", row.names = F)

#### 5) Unir los resultados de los modelo para todos los niveles de severidad, severidad 1, 2 y 3 ####

# Abrir las bases creadas en los scripts para all crimes, g1 y g2
pd_allcrimes_nb <- read_csv("pd_allcrimes_nb.csv")
pd_g1_nb <- read_csv("pd_g1_nb.csv")
pd_g2_nb <- read_csv("pd_g2_nb.csv")

# Juntar las bases y guardarla 
pd_nb <- rbind(pd_allcrimes_nb, pd_g1_nb, pd_g2_nb, pd_g3_nb)

write.csv(pd_nb, "pd_nb.csv", row.names = F)

#### 6) Número de PDs por niveles de severidad  y clusters ####

# PDS total (todos los niveles de severidad más severidad 1, 2 y 3)
n_distinct(pd_nb$CVEGEO)# Hay 7 PDs 

# PDS para todos los niveles de severidad
pd_nb_allcrimes <- pd_nb %>% 
  filter(result_var== "allcrimes")

n_distinct(pd_nb_allcrimes$CVEGEO) 

# PDs para los modelos con por nivel de severidad
pd_nb_porseveridad <- pd_nb %>% 
  filter(result_var== "g1" | result_var== "g2" | result_var== "g3")

n_distinct(pd_nb_porseveridad$CVEGEO)# Hay 3 PDs 

# PDS por cluster para los modelos con los delitos separados por nivel de severidad
pd_porseveridad_porcluster <- pd_nb_porseveridad %>% 
  group_by(cluster) %>% 
  summarise(total= n())

pd_porseveridad_porcluster

# PDs por cluster para todos los niveles de severidad
pd_allcrimes_porcluster <- pd_nb_allcrimes %>% 
  group_by(cluster) %>% 
  summarise(total= n())

pd_allcrimes_porcluster


#### 7) Mapa de PDs ####

# Abrir el marco geoestadistico 2020 de la CDMX a nivel AGEB
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

# Crear una variable para identificar si las AGEBS son PDs de qué tipo de variable de resultado
pd_nb <- pd_nb %>% 
  mutate(PD = ifelse (result_var == "g1", "severity 1",
                      ifelse(result_var == "g2", "severity 2",
                             ifelse(result_var == "g3", "severity 3", "all crimes"))))

# Unir AGEBS con los PD
pd <- left_join(agebs_sf, pd_nb[,c("CVEGEO", "PD")], by="CVEGEO")

pd <- pd %>% 
  mutate(PD = ifelse (PD %in% NA, "nonPD", PD))

# Inlcuir la variable cluster
pd <- left_join(pd, base[,c("CVEGEO", "cluster")], by="CVEGEO")
pd$cluster <- as.factor(pd$cluster)

# Identificar las AGEBS que son PD en más de un modelo
pd <- pd %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

pd <- pd %>% 
  mutate(PD = ifelse (duplicado == 2, "duplicate", 
                      ifelse(duplicado == 3, "triplicate", PD)))

# Ordenar la variable PD
pd$PD <- factor(pd$PD, ordered= TRUE,
                levels = c("severity 3", "all crimes", "duplicate", "triplicate", "nonPD"))

# Mapa
pd %>% 
  filter(!(cluster %in% NA)) %>% 
  ggplot() +
  geom_sf(aes(color = cluster, fill= PD), size= 1.3)+
  scale_fill_manual(values = c("#006400", "#ffb703", "#001427", "#F77F00", "transparent"))+ 
  scale_color_manual(values = c("#ff85a1", "#0ead69", "#4361ee", "#7d8597"))+
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=50),
        legend.text = element_text(size=40))

ggsave("mapa_pds_nb.jpg", width = 30, height = 35)


