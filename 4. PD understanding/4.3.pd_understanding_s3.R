rm(list=ls())

library(readr)
library(dplyr)
library(tidyverse)
library(easypackages)
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(latticeExtra)
library(sf)
library(ggpubr)
library(ggplot2)

#### PDs understanding ####

#### Severidad 3 ####

#### 1) Todos los clusters juntos ####

# Abrir la base creada en el script 4.1.pd_understanding_variables
base <- read_csv("base_pdunderstanding.csv")

#Revisar si las variables dependientes continuas siguen una distribucion normal

# Para el grupo de Non PDs
grupos <- split(base, base$pd_s3)
non_pds<-grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

#Para el grupo de PDs de delitos de gravedad 3
pds<-grupos$"PDs"

normalidad <- lapply(pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 en la mayoría de las variables. Los datos no siguen una distribucion normal tampoco para el grupo de PDs

# Revisar la similitud en la distribución de los dos grupos
par(mfrow=c(1,2))

hist(non_pds$distanciaminpoli_km,main='Nearest police station Non PDs',xlab='km')
hist(pds$distanciaminpoli_km, main='Nearest police station PDs', xlab='km')

hist(non_pds$total_conboton_km2,main='STV with panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_conboton_km2, main='STV with panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_sinboton_km2,main='STV without panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_sinboton_km2, main='STV without panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_g3_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_g3_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

hist(non_pds$g3_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
hist(pds$g3_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

hist(non_pds$g3_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
hist(pds$g3_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

hist(non_pds$g3_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
hist(pds$g3_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

hist(non_pds$g3_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
hist(pds$g3_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

hist(non_pds$g3_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
hist(pds$g3_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

hist(non_pds$g3_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
hist(pds$g3_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

hist(non_pds$g3_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
hist(pds$g3_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(base, x = "pd_s3", y = "distanciaminpoli_km", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(base, x = "pd_s3", y = "total_conboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(base, x = "pd_s3", y = "total_sinboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(base, x = "pd_s3", y = "total_stv_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05, no hay diferencia significativa. 


# Agebs con pasos seguros 

table_pasos <-table(base$pd_s3, base$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$pasos_seguros)
fisher # La distribución es igual p-valor mayor a 0.05


# Agebs con pasos seguros buffer de 500 metros
table <-table(base$pd_s3, base$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$pasos_seguros_buff)
fisher # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Agebs con intersecciones seguras
table_intersecciones <-table(base$pd_s3, base$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$intersecciones_seguras)
fisher # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(base$pd_s3, base$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# 10.6.2) Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Chi square test
test <- chisq.test(table(base$pd_s3, base$intersecciones_seguras_buff))
test # El p valor es mayor a 0.05, se acepta la hipótesis nula que significa que las dos variables son idependientes, no hay relación entre las agebs PDs y no PDs y la presencia de pasos seguros.  Fisher Exact probability test

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$intersecciones_seguras_buff)
fisher # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(base$pd_s3, base$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$senderos)
fisher # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Terrenos baldíos

# Tabla de contingencia
table_vacantlots <-table(base$pd_s3, base$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_s3, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(base$pd_s3, base$baldio)
fisher # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(base, x = "pd_s3", y = "areaverde_pequena_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_s3, data = base,
                   exact = FALSE)
res# El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# Boxplot

ggboxplot(base, x = "pd_s3", y = "sinalumbrado_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Víctimas hombres
summ_hom <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_g3_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_g3_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(base, x = "pd_s3", y = "victimas_hombres_g3_pob", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(victimas_hombres_g3_pob ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Hora promedio

# Resumen de estadísticos
summ_hora <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_horamedia, na.rm = TRUE),
    IQR = IQR(g3_horamedia, na.rm = TRUE)
  )

summ_hora

# Boxplot

ggboxplot(data=subset(base, !is.na(g3_horamedia)), na.omit=T, x = "pd_s3", y = "g3_horamedia", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Mean hour of crimes", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_horamedia ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 5am a 11am

# Resumen de estadísticos
summ_5to11 <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_5a11_por, na.rm = TRUE),
    IQR = IQR(g3_5a11_por, na.rm = TRUE)
  )

summ_5to11

# Boxplot

ggboxplot(data=subset(base, !is.na(g3_5a11_por)), x = "pd_s3", y = "g3_5a11_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 5 to 11 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_5a11_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 12 to 16 hrs

# Resumen de estadísticos
summ_12to16 <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_12a16_por, na.rm = TRUE),
    IQR = IQR(g3_12a16_por, na.rm = TRUE)
  )

summ_12to16

# Boxplot

ggboxplot(data=subset(base, !is.na(g3_12a16_por)), x = "pd_s3", y = "g3_12a16_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 12 to 16 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_12a16_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 17 a 23 hrs
# Resumen de estadísticos
summ_17to23 <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_17a23_por, na.rm = TRUE),
    IQR = IQR(g3_17a23_por, na.rm = TRUE)
  )

summ_17to23

# Boxplot

ggboxplot(data=subset(base, !is.na(g3_17a23_por)), x = "pd_s3", y = "g3_17a23_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 17 to 23 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(g3_17a23_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Porcentaje de delitos ocurridos de 24 a 4 hrs
# Resumen de estadísticos
summ_24to4 <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_24a4_por, na.rm = TRUE),
    IQR = IQR(g3_24a4_por, na.rm = TRUE)
  )

summ_24to4

# Boxplot
ggboxplot(data=subset(base, !is.na(g3_24a4_por)), x = "pd_s3", y = "g3_24a4_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 24 to 4 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_24a4_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Entre semana
# Resumen de estadísticos
summ_week <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_entresem_por, na.rm = TRUE),
    IQR = IQR(g3_entresem_por, na.rm = TRUE)
  )

summ_week

# Boxplot
ggboxplot(data=subset(base, !is.na(g3_entresem_por)), x = "pd_s3", y = "g3_entresem_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes that occurred during the week", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_entresem_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Fin de semana
# Resumen de estadísticos
summ_weekend <- group_by(base, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_finde_por, na.rm = TRUE),
    IQR = IQR(g3_finde_por, na.rm = TRUE)
  )

summ_weekend

# Boxplot
ggboxplot(data=subset(base, !is.na(g3_finde_por)), x = "pd_s3", y = "g3_finde_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes that occurred during the weekend", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_finde_por ~ pd_s3, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


#### PDs understanding por clusters ####

#### 2) Cluster 1 ####

clusters <- split(base, base$cluster)
c1 <- clusters$"1"

#Revisar si las variables dependientes continuas siguen una distribucion normal

# Para el grupo de Non PDs
grupos <- split(c1, c1$pd_s3)
non_pds<-grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

#Para el grupo de PDs de delitos de gravedad 3
pds<-grupos$"PDs"

normalidad <- lapply(pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 en la mayoría de las variables. Los datos no siguen una distribucion normal tampoco para el grupo de PDs

# Revisar la similitud en la distribución de los dos grupos
par(mfrow=c(1,2))

hist(non_pds$distanciaminpoli_km,main='Nearest police station Non PDs',xlab='km')
hist(pds$distanciaminpoli_km, main='Nearest police station PDs', xlab='km')

hist(non_pds$total_conboton_km2,main='STV with panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_conboton_km2, main='STV with panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_sinboton_km2,main='STV without panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_sinboton_km2, main='STV without panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_g3_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_g3_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

hist(non_pds$g3_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
hist(pds$g3_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

hist(non_pds$g3_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
hist(pds$g3_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

hist(non_pds$g3_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
hist(pds$g3_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

hist(non_pds$g3_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
hist(pds$g3_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

hist(non_pds$g3_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
hist(pds$g3_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

hist(non_pds$g3_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
hist(pds$g3_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

hist(non_pds$g3_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
hist(pds$g3_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(c1, x = "pd_s3", y = "distanciaminpoli_km", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(c1, x = "pd_s3", y = "total_conboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(c1, x = "pd_s3", y = "total_sinboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(c1, x = "pd_s3", y = "total_stv_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05. No hay diferencia estadísticamente significativa. 


# Agebs con pasos seguros 
table_pasos <-table(c1$pd_s3, c1$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# 10.5.1) Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$pasos_seguros)
fisher # El p valor es mayor a 0.05, no hay diferencia singificativa. 


# Agebs con pasos seguros buffer de 500 metros
table <-table(c1$pd_s3, c1$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$pasos_seguros_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con intersecciones seguras
table_intersecciones <-table(c1$pd_s3, c1$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$intersecciones_seguras)
fisher # El p valor es ligeramente mayor a 0.05. No hay diferencia significativa

# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(c1$pd_s3, c1$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# 10.6.2) Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$intersecciones_seguras_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(c1$pd_s3, c1$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$senderos)
fisher #El p valor es mayor a 0.05, no hay diferencia significativa

# Terrenos baldíos

# Tabla de contingencia
table_vacantlots <-table(c1$pd_s3, c1$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_s3, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(c1$pd_s3, c1$baldio)
fisher # El p valor es mayor a 0.05, no hay diferencia significativa. 

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(c1, x = "pd_s3", y = "areaverde_pequena_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_s3, data = c1,
                   exact = FALSE)
res# El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 


# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# 10.1.2) Boxplot

ggboxplot(c1, x = "pd_s3", y = "sinalumbrado_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# 10.1.2) Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Víctimas hombres
summ_hom <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_g3_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_g3_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(c1, x = "pd_s3", y = "victimas_hombres_g3_pob", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# 10.7.3) Wilcoxon test

res <- wilcox.test(victimas_hombres_g3_pob ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 


# Hora promedio

# Resumen de estadísticos
summ_hora <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_horamedia, na.rm = TRUE),
    IQR = IQR(g3_horamedia, na.rm = TRUE)
  )

summ_hora

# 10.10.2) Boxplot

ggboxplot(data=subset(c1, !is.na(g3_horamedia)), na.omit=T, x = "pd_s3", y = "g3_horamedia", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Mean hour of crimes", xlab = "", 
          legend.title = "")

# 10.10.3) Wilcoxon test

res <- wilcox.test(g3_horamedia ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 5am a 11am

# Resumen de estadísticos
summ_5to11 <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_5a11_por, na.rm = TRUE),
    IQR = IQR(g3_5a11_por, na.rm = TRUE)
  )

summ_5to11

# Boxplot

ggboxplot(data=subset(c1, !is.na(g3_5a11_por)), x = "pd_s3", y = "g3_5a11_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 5 to 11 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_5a11_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 12 to 16 hrs

# Resumen de estadísticos
summ_12to16 <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_12a16_por, na.rm = TRUE),
    IQR = IQR(g3_12a16_por, na.rm = TRUE)
  )

summ_12to16

# Boxplot

ggboxplot(data=subset(c1, !is.na(g3_12a16_por)), x = "pd_s3", y = "g3_12a16_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 12 to 16 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_12a16_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 17 a 23 hrs
# Resumen de estadísticos
summ_17to23 <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_17a23_por, na.rm = TRUE),
    IQR = IQR(g3_17a23_por, na.rm = TRUE)
  )

summ_17to23

# Boxplot

ggboxplot(data=subset(c1, !is.na(g3_17a23_por)), x = "pd_s3", y = "g3_17a23_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 17 to 23 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(g3_17a23_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia significativa.


# Porcentaje de delitos ocurridos de 24 a 4 hrs
# Resumen de estadísticos
summ_24to4 <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_24a4_por, na.rm = TRUE),
    IQR = IQR(g3_24a4_por, na.rm = TRUE)
  )

summ_24to4

# Boxplot
ggboxplot(data=subset(c1, !is.na(g3_24a4_por)), x = "pd_s3", y = "g3_24a4_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes occurred from 24 to 4 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_24a4_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Entre semana
# Resumen de estadísticos
summ_week <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_entresem_por, na.rm = TRUE),
    IQR = IQR(g3_entresem_por, na.rm = TRUE)
  )

summ_week

# Boxplot
ggboxplot(data=subset(c1, !is.na(g3_entresem_por)), x = "pd_s3", y = "g3_entresem_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes that occurred during the week", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_entresem_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Fin de semana
# Resumen de estadísticos
summ_weekend <- group_by(c1, pd_s3) %>%
  summarise(
    count = n(),
    median = median(g3_finde_por, na.rm = TRUE),
    IQR = IQR(g3_finde_por, na.rm = TRUE)
  )

summ_weekend

# Boxplot
ggboxplot(data=subset(c1, !is.na(g3_finde_por)), x = "pd_s3", y = "g3_finde_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of crimes that occurred during the weekend", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(g3_finde_por ~ pd_s3, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


#### 3) Cluster 2 ####
# Solo hay 2 PDs, no realizamos la etapa de PD understanding

#### 4) Cluster 3 ####
c3 <- clusters$"3"

#Revisar si las variables dependientes continuas siguen una distribucion normal

# Para el grupo de Non PDs
grupos <- split(c3, c3$pd_s3)
non_pds<-grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60)],shapiro.test) # Las PDs de gravedad 3 cluster 3 no tienen víctimas en carpetas de investigación
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

#Para el grupo de PDs de delitos de gravedad 3
pds<-grupos$"PDs"

normalidad <- lapply(pds[,c(46:49, 55, 60)],shapiro.test)
normalidad #El p valor es menor a 0.05 en la mayoría de las variables. Los datos no siguen una distribucion normal tampoco para el grupo de PDs

# Revisar la similitud en la distribución de los dos grupos
par(mfrow=c(1,2))

hist(non_pds$distanciaminpoli_km,main='Nearest police station Non PDs',xlab='km')
hist(pds$distanciaminpoli_km, main='Nearest police station PDs', xlab='km')

hist(non_pds$total_conboton_km2,main='STV with panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_conboton_km2, main='STV with panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_sinboton_km2,main='STV without panic buttons Non PDs',xlab='Number per km2')
hist(pds$total_sinboton_km2, main='STV without panic buttons PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$total_stv_km2,main='STV Non PDs',xlab='Number per km2')
hist(pds$total_stv_km2, main='STV PDs', xlab='Number per km2')

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_g3_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_g3_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

# No tenemos víctimas en carpetas de investigación en los PDs de gravedad 3 cluster 3 por lo que no podemos analizar la hora ni el día de ocurrencia de los delitos. 

#hist(non_pds$g3_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
#hist(pds$g3_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

#hist(non_pds$g3_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
#hist(pds$g3_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

#hist(non_pds$g3_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
#hist(pds$g3_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

#hist(non_pds$g3_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
#hist(pds$g3_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

#hist(non_pds$g3_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
#hist(pds$g3_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

#hist(non_pds$g3_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
#hist(pds$g3_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

#hist(non_pds$g3_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
#hist(pds$g3_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(c3, x = "pd_s3", y = "distanciaminpoli_km", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que la diferencia es estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(c3, x = "pd_s3", y = "total_conboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(c3, x = "pd_s3", y = "total_sinboton_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(c3, x = "pd_s3", y = "total_stv_km2", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es mayor a 0.05. No hay diferencia estadísticamente significativa. 


# Agebs con pasos seguros 
table_pasos <-table(c3$pd_s3, c3$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$pasos_seguros)
fisher # El p valor es mayor a 0.05, no hay diferencia significativa


# Agebs con pasos seguros buffer de 500 metros
table <-table(c3$pd_s3, c3$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$pasos_seguros_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con intersecciones seguras
table_intersecciones <-table(c3$pd_s3, c3$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$intersecciones_seguras)
fisher # El p valor es mayor a 0.05, no hay diferencia significativa

# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(c3$pd_s3, c3$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$intersecciones_seguras_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(c3$pd_s3, c3$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$senderos)
fisher # El p valor es mayor a 0.05, no hay diferencia estadísticamente significativa. 

# Terrenos baldíos

# Tabla de contingencia
table_vacantlots <-table(c3$pd_s3, c3$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(c3) +
  aes(x = pd_s3, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#404788FF", "#DCE319FF"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(c3$pd_s3, c3$baldio)
fisher # El p valor es mayor a 0.05, no hay diferencia estadísticamente significativa. 

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(c3, x = "pd_s3", y = "areaverde_pequena_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_s3, data = c3,
                   exact = FALSE)
res# El p valor es menor a 0.05 por lo que hay diferencia estadísticamente significativa. 


# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# Boxplot

ggboxplot(c3, x = "pd_s3", y = "sinalumbrado_por", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que hay diferencia estadísticamente significativa. 


# Víctimas hombres
summ_hom <- group_by(c3, pd_s3) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_g3_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_g3_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(c3, x = "pd_s3", y = "victimas_hombres_g3_pob", 
          color = "pd_s3", palette = c("#404788FF", "#DCE319FF"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(victimas_hombres_g3_pob ~ pd_s3, data = c3,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa. 

# No podemos hacer el resto de comparaciones entre ambos grupos porque las AGEBS PDs no tienen victimas en carpetas de investigación 