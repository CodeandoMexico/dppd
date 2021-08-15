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

#### PD understanding ####

# Hacer comparaciones entre los grupos de AGEBS PDS y no PDS

#### Todos los niveles de severidad ####

#### 1) Todos los clusters juntos ####

# Abrir la base creada en el script 4.1.pd_understanding_variables
base <- read_csv("base_pdunderstanding.csv")

#Revisar si las variables dependientes continuas siguen una distribucion normal

# Para todos los grupos
normalidad <- lapply(base[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal

# Para el grupo de Non PDs
grupos <- split(base, base$pd_allcrimes)
non_pds<-grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

# Para el grupo de PDs
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

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

hist(non_pds$allcrimes_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
hist(pds$allcrimes_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

hist(non_pds$allcrimes_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
hist(pds$allcrimes_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

hist(non_pds$allcrimes_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
hist(pds$allcrimes_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, pero siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(base, x = "pd_allcrimes", y = "distanciaminpoli_km", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(base, x = "pd_allcrimes", y = "total_conboton_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(base, x = "pd_allcrimes", y = "total_sinboton_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(base, x = "pd_allcrimes", y = "total_stv_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es menor a 0.05. Se rechaza la H0 de igualdad entre las PDs y no Pds. 


# Agebs con pasos seguros 
table_pasos <-table(base$pd_allcrimes, base$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# 10.5.1) Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$pasos_seguros)
fisher # La distribución es igual p-valor mayor a 0.05


# Agebs con pasos seguros buffer de 500 metros
table <-table(base$pd_allcrimes, base$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$pasos_seguros_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con intersecciones seguras
table_intersecciones <-table(base$pd_allcrimes, base$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$intersecciones_seguras)
fisher #La distribución es igual p-valor mayor a 0.05


# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(base$pd_allcrimes, base$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Chi square test
test <- chisq.test(table(base$pd_allcrimes, base$intersecciones_seguras_buff))
test # El p valor es mayor a 0.05, se acepta la hipótesis nula que significa que las dos variables son idependientes, no hay relación entre las agebs PDs y no PDs y la presencia de pasos seguros.  Fisher Exact probability test

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$intersecciones_seguras_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(base$pd_allcrimes, base$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$senderos)
fisher #La distribución es igual p-valor mayor a 0.05

# Terrenos baldíos

# Tabla de contingencia
table_vacantlots <-table(base$pd_allcrimes, base$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(base) +
  aes(x = pd_allcrimes, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(base$pd_allcrimes, base$baldio)
fisher #La distribución es igual, p-valor mayor a 0.05

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(base, x = "pd_allcrimes", y = "areaverde_pequena_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_allcrimes, data = base,
                   exact = FALSE)
res# El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 


# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# Boxplot

ggboxplot(base, x = "pd_allcrimes", y = "sinalumbrado_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que rechazamos la H0 de que las medianas son iguales. 
# Hay diferencia estadísticamente significativa. 

wilcox.test(sinalumbrado_por ~ pd_allcrimes, data = base, 
            exact = FALSE, alternative = "greater")


# Víctimas hombres
summ_hom <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(base, x = "pd_allcrimes", y = "victimas_hombres_pob", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(victimas_hombres_pob ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 


# Hora promedio

# Resumen de estadísticos
summ_hora <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_horamedia, na.rm = TRUE),
    IQR = IQR(allcrimes_horamedia, na.rm = TRUE)
  )

summ_hora

# 10.10.2) Boxplot

ggboxplot(data=subset(base, !is.na(allcrimes_horamedia)), na.omit=T, x = "pd_allcrimes", y = "allcrimes_horamedia", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Mean hour of crimes", xlab = "", 
          legend.title = "")

# 10.10.3) Wilcoxon test

res <- wilcox.test(allcrimes_horamedia ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 5am a 11am

# Resumen de estadísticos
summ_5to11 <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_5a11_por, na.rm = TRUE),
    IQR = IQR(allcrimes_5a11_por, na.rm = TRUE)
  )

summ_5to11

# Boxplot

ggboxplot(data=subset(base, !is.na(allcrimes_5a11_por)), x = "pd_allcrimes", y = "allcrimes_5a11_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 5 to 11 hrs", xlab = "", 
          legend.title = "")

#  Wilcoxon test

res <- wilcox.test(allcrimes_5a11_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 12 to 16 hrs

# Resumen de estadísticos
summ_12to16 <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_12a16_por, na.rm = TRUE),
    IQR = IQR(allcrimes_12a16_por, na.rm = TRUE)
  )

summ_12to16

# Boxplot

ggboxplot(data=subset(base, !is.na(allcrimes_12a16_por)), x = "pd_allcrimes", y = "allcrimes_12a16_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 12 to 16 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_12a16_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que aceptamos la H0 de que las medias no son iguales. 
# Hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 17 a 23 hrs

# Resumen de estadísticos
summ_17to23 <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_17a23_por, na.rm = TRUE),
    IQR = IQR(allcrimes_17a23_por, na.rm = TRUE)
  )

summ_17to23

# Boxplot

ggboxplot(data=subset(base, !is.na(allcrimes_17a23_por)), x = "pd_allcrimes", y = "allcrimes_17a23_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 17 to 23 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_17a23_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 24 a 4 hrs

# Resumen de estadísticos
summ_24to4 <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_24a4_por, na.rm = TRUE),
    IQR = IQR(allcrimes_24a4_por, na.rm = TRUE)
  )

summ_24to4

# Boxplot
ggboxplot(data=subset(base, !is.na(allcrimes_24a4_por)), x = "pd_allcrimes", y = "allcrimes_24a4_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 24 to 4 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_24a4_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Entre semana

# Resumen de estadísticos
summ_week <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_entresem_por, na.rm = TRUE),
    IQR = IQR(allcrimes_entresem_por, na.rm = TRUE)
  )

summ_week

# Boxplot
ggboxplot(data=subset(base, !is.na(allcrimes_entresem_por)), x = "pd_allcrimes", y = "allcrimes_entresem_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the week", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_entresem_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que aceptamos la H0 de que las medias no son iguales. 
# Hay diferencia estadísticamente significativa.


# Fin de semana

# Resumen de estadísticos
summ_weekend <- group_by(base, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_finde_por, na.rm = TRUE),
    IQR = IQR(allcrimes_finde_por, na.rm = TRUE)
  )

summ_weekend

# Boxplot
ggboxplot(data=subset(base, !is.na(allcrimes_finde_por)), x = "pd_allcrimes", y = "allcrimes_finde_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the weekend", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_finde_por ~ pd_allcrimes, data = base,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que aceptamos la H0 de que las medias no son iguales. 
# Hay diferencia estadísticamente significativa.

#### PD understanding por clusters ####

#### 2) Cluster 1 ####

# Abrir la base creada en el script 4.1.pd_understanding_variables
base <- read_csv("base_pdunderstanding.csv")

# Dividir la base en clusters
clusters <- split(base, base$cluster)
c1 <- clusters$"1"

# Para el grupo de Non PDs
grupos <- split(c1, c1$pd_allcrimes)
non_pds<- grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

#Para el grupo de PDs
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

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

hist(non_pds$allcrimes_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
hist(pds$allcrimes_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

hist(non_pds$allcrimes_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
hist(pds$allcrimes_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

hist(non_pds$allcrimes_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
hist(pds$allcrimes_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(c1, x = "pd_allcrimes", y = "distanciaminpoli_km", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(c1, x = "pd_allcrimes", y = "total_conboton_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(c1, x = "pd_allcrimes", y = "total_sinboton_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que rechazamos la H0.
# Hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(c1, x = "pd_allcrimes", y = "total_stv_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05. No hay diferencia significativa


# Agebs con pasos seguros 
table_pasos <-table(c1$pd_allcrimes, c1$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$pasos_seguros)
fisher # La distribución es igual, el p-valor mayor a 0.05. No hay diferencia significativa


# Agebs con pasos seguros buffer de 500 metros
table <-table(c1$pd_allcrimes, c1$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$pasos_seguros_buff)
fisher #La distribución es igual, el p-valor mayor a 0.05. No hay diferencia significativa

# Agebs con intersecciones seguras
table_intersecciones <-table(c1$pd_allcrimes, c1$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$intersecciones_seguras)
fisher #La distribución es igual p-valor mayor a 0.05


# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(c1$pd_allcrimes, c1$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$intersecciones_seguras_buff)
fisher #La distribución es igual p-valor mayor a 0.05


# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(c1$pd_allcrimes, c1$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$senderos)
fisher #La distribución es igual p-valor mayor a 0.05

# Terrenos baldíos

# Tabla de contingencia
table_vacantlots <-table(c1$pd_allcrimes, c1$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(c1) +
  aes(x = pd_allcrimes, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(c1$pd_allcrimes, c1$baldio)
fisher #La distribución es igual, p-valor mayor a 0.05

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(c1, x = "pd_allcrimes", y = "areaverde_pequena_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)
res# El p valor es igual a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 


# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# Boxplot

ggboxplot(c1, x = "pd_allcrimes", y = "sinalumbrado_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que rechazamos la H0 de que las medianas son iguales. 
# Hay diferencia estadísticamente significativa. 

# Víctimas hombres

summ_hom <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(c1, x = "pd_allcrimes", y = "victimas_hombres_pob", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(victimas_hombres_pob ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 


# Hora promedio

# Resumen de estadísticos
summ_hora <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_horamedia, na.rm = TRUE),
    IQR = IQR(allcrimes_horamedia, na.rm = TRUE)
  )

summ_hora

# Boxplot

ggboxplot(data=subset(c1, !is.na(allcrimes_horamedia)), na.omit=T, x = "pd_allcrimes", y = "allcrimes_horamedia", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Mean hour of crimes", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_horamedia ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.

res <- t.test(allcrimes_horamedia ~ pd_allcrimes, data = c1, var.equal = TRUE)
res

# Porcentaje de delitos ocurridos de 5am a 11am

# Resumen de estadísticos
summ_5to11 <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_5a11_por, na.rm = TRUE),
    IQR = IQR(allcrimes_5a11_por, na.rm = TRUE)
  )

summ_5to11

# Boxplot

ggboxplot(data=subset(c1, !is.na(allcrimes_5a11_por)), x = "pd_allcrimes", y = "allcrimes_5a11_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 5 to 11 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_5a11_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 12 to 16 hrs

# Resumen de estadísticos
summ_12to16 <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_12a16_por, na.rm = TRUE),
    IQR = IQR(allcrimes_12a16_por, na.rm = TRUE)
  )

summ_12to16

# Boxplot

ggboxplot(data=subset(c1, !is.na(allcrimes_12a16_por)), x = "pd_allcrimes", y = "allcrimes_12a16_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 12 to 16 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_12a16_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia significativa. 


# Porcentaje de delitos ocurridos de 17 a 23 hrs
# Resumen de estadísticos
summ_17to23 <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_17a23_por, na.rm = TRUE),
    IQR = IQR(allcrimes_17a23_por, na.rm = TRUE)
  )

summ_17to23

# Boxplot

ggboxplot(data=subset(c1, !is.na(allcrimes_17a23_por)), x = "pd_allcrimes", y = "allcrimes_17a23_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 17 to 23 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_17a23_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 24 a 4 hrs
# Resumen de estadísticos
summ_24to4 <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_24a4_por, na.rm = TRUE),
    IQR = IQR(allcrimes_24a4_por, na.rm = TRUE)
  )

summ_24to4

# Boxplot
ggboxplot(data=subset(c1, !is.na(allcrimes_24a4_por)), x = "pd_allcrimes", y = "allcrimes_24a4_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 24 to 4 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_24a4_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Entre semana
# Resumen de estadísticos
summ_week <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_entresem_por, na.rm = TRUE),
    IQR = IQR(allcrimes_entresem_por, na.rm = TRUE)
  )

summ_week

# Boxplot
ggboxplot(data=subset(c1, !is.na(allcrimes_entresem_por)), x = "pd_allcrimes", y = "allcrimes_entresem_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the week", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_entresem_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Fin de semana
# Resumen de estadísticos
summ_weekend <- group_by(c1, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_finde_por, na.rm = TRUE),
    IQR = IQR(allcrimes_finde_por, na.rm = TRUE)
  )

summ_weekend

# Boxplot
ggboxplot(data=subset(c1, !is.na(allcrimes_finde_por)), x = "pd_allcrimes", y = "allcrimes_finde_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the weekend", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_finde_por ~ pd_allcrimes, data = c1,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que rechazamos la H0. 
# No hay diferencia estadísticamente significativa.

#### 3) Cluster 2 ####

# Dividir la base en clusters
c2 <- clusters$"2"

# Para el grupo de Non PDs
grupos <- split(c2, c2$pd_allcrimes)
non_pds<- grupos$"Non PDs"

normalidad <- lapply(non_pds[,c(46:49, 55, 60, 69:75)],shapiro.test)
normalidad #El p valor es menor a 0.05 por lo que se rechaza la H0 de normalidad. Los datos no siguen una distribucion normal en el grupo de Non PDs

#Para el grupo de PDs
pds <- grupos$"PDs"

normalidad <- lapply(pds[,c(46:49, 55, 60, 69:73)],shapiro.test) # No incluimos las variables de % de delitos entre semana y en fin de semana porque todos los valores entre semana son iguales a 1 y en fin de semana iguales a cero. 
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

hist(non_pds$areaverde_pequena_por,main='Small green areas Non PDs',xlab='% of the AGEB occupied by small green areas')
hist(pds$areaverde_pequena_por, main='Small green areas PDs', xlab='% of the AGEB occupied by small green areas')

hist(non_pds$sinalumbrado_por,main='Without public lighting Non PDs',xlab='% of the streets without public lighting')
hist(pds$sinalumbrado_por, main='Without public lighting PDs', xlab='% of the streets without public lighting')

hist(non_pds$victimas_hombres_pob,main='Male victims Non PDs',xlab='Male victim for every inhabitant')
hist(pds$victimas_hombres_pob, main='Male victims PDs', xlab='Male victim for every inhabitant')

hist(non_pds$allcrimes_horamedia,main='Mean hour of crimes Non PDs',xlab='Mean hour')
hist(pds$allcrimes_horamedia, main='Mean hour of crimes PDs', xlab='Mean hour')

hist(non_pds$allcrimes_5a11_por,main='Crimes occurred from 5 to 11 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_5a11_por, main='Crimes occurred from 5 to 11 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_12a16_por,main='Crimes occurred from 12 to 16 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_12a16_por, main='Crimes occurred from 12 to 16 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_17a23_por,main='Crimes occurred from 17 to 23 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_17a23_por, main='Crimes occurred from 17 to 23 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_24a4_por,main='Crimes occurred from 24 to 4 hours Non PDs',xlab='% of crimes')
hist(pds$allcrimes_24a4_por, main='Crimes occurred from 24 to 4 hours PDs', xlab='% of crimes')

hist(non_pds$allcrimes_entresem_por,main='Crimes occurred during the week Non PDs',xlab='% of crimes')
hist(pds$allcrimes_entresem_por, main='Crimes occurred during the week PDs', xlab='% of crimes')

hist(non_pds$allcrimes_finde_por,main='Crimes occurred during the weekend Non PDs',xlab='% of crimes')
hist(pds$allcrimes_finde_por, main='Crimes occurred during the weekend PDs', xlab='% of crimes')

# Ya que los datos no se distribuyen de manera normal, siguen una distribución similar en los dos grupos y éstos son independientes,
# se sugiere usar la prueba Mann-Whitney para comprobar si hay diferencias significativas entre los dos grupos

# Comparación de medianas usando la prueba Mann- Whitney

# Distancia a la estación de policías más cercana

# Resumen de estadísticos
summ_distance <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(distanciaminpoli_km, na.rm = TRUE),
    IQR = IQR(distanciaminpoli_km, na.rm = TRUE)
  )

summ_distance

# Boxplot
ggboxplot(c2, x = "pd_allcrimes", y = "distanciaminpoli_km", 
          color = "pd_allcrimes", palette = c("#3c2642", "#2a9134"),
          ylab = "Distance to the nearest police station km", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(distanciaminpoli_km ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medianas son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems con botones de pánico por km2

# Resumen de estadísticos
summ_boton <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_conboton_km2, na.rm = TRUE),
    IQR = IQR(total_conboton_km2, na.rm = TRUE)
  )

summ_boton

# Boxplot
ggboxplot(c2, x = "pd_allcrimes", y = "total_conboton_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV with panic buttons per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_conboton_km2 ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa. 

# Número de totems sin boton de pánico por km2
# Resumen de estadísticos
summ_noboton <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_sinboton_km2, na.rm = TRUE),
    IQR = IQR(total_sinboton_km2, na.rm = TRUE)
  )

summ_noboton

# Boxplot
ggboxplot(c2, x = "pd_allcrimes", y = "total_sinboton_km2", 
          color = "pd_allcrimes", palette = c("#3c2642", "#2a9134"),
          ylab = "STV without panic buttons per km2", xlab = "", 
          legend.title = "")

# Wilcoxon test
res <- wilcox.test(total_sinboton_km2 ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0. Los dos grupos son iguales.
# No hay diferencia estadísticamente significativa. 

# Número de totems en total
# Resumen de estadísticos

summ_stv <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(total_stv_km2, na.rm = TRUE),
    IQR = IQR(total_stv_km2, na.rm = TRUE)
  )

summ_stv

# Boxplot
ggboxplot(c2, x = "pd_allcrimes", y = "total_stv_km2", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "STV per km2", xlab = "", 
          legend.title = "")  

# Wilcoxon test
res <- wilcox.test(total_stv_km2 ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es menor a 0.05. Se rechaza la H0 de igualdad entre las PDs y no Pds. 


# Agebs con pasos seguros 
table_pasos <-table(c2$pd_allcrimes, c2$pasos_seguros) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher en vez de la chi cuadrada
table_pasos
round(prop.table(table_pasos, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = pasos_seguros) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Pasos seguros")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$pasos_seguros)
fisher # La distribución es igual p-valor mayor a 0.05


# Agebs con pasos seguros buffer de 500 metros
table <-table(c2$pd_allcrimes, c2$pasos_seguros_buff) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table
round(prop.table(table, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = pasos_seguros_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$pasos_seguros_buff)
fisher #La distribución es igual p-valor mayor a 0.05

# Agebs con intersecciones seguras
table_intersecciones <-table(c2$pd_allcrimes, c2$intersecciones_seguras) #Ya que el valor de una de las celdas de la tabla de contingencia es menor a cinco, usamos prueba de Fisher
table_intersecciones
round(prop.table(table_intersecciones, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Intersecciones seguras")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$intersecciones_seguras)
fisher #La distribución es igual p-valor mayor a 0.05


# Agebs con intersecciones seguras buffer de 500 metros

# Tabla de contingencia
table_intersecciones_buff <-table(c2$pd_allcrimes, c2$intersecciones_seguras_buff)#Ya que el valor en ninguna celda de la tabla de contingencia es menor a cinco, podemos usar la prueba chi cuadrada
table_intersecciones_buff
round(prop.table(table_intersecciones_buff, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = intersecciones_seguras_buff) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Safe steps")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$intersecciones_seguras_buff)
fisher #La distribución es igual p-valor mayor a 0.05


# Agebs con Senderos seguros

# Tabla de contingencia senderos
table_senderos <-table(c2$pd_allcrimes, c2$senderos) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_senderos
round(prop.table(table_senderos, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = senderos) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Senderos seguros")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$senderos)
fisher #La distribución es igual p-valor mayor a 0.05

# Terrenos baldíos

# Tabla de contingencia
#base <- base %>% 
table_vacantlots <-table(c2$pd_allcrimes, c2$baldio) #Ya que el valor en una celda de la tabla de contingencia es menor a cinco, usamos la prueba de Fisher
table_vacantlots
round(prop.table(table_vacantlots, 1), 2)

# Gráfica de barras
ggplot(c2) +
  aes(x = pd_allcrimes, fill = baldio) +
  geom_bar(position = "fill", width=0.8) +
  scale_fill_manual(values= c("#3c1642", "#2a9134"))+
  ylab("Percentage") +
  labs(fill = "Vacant lots")

# Fisher's exact test
fisher <- fisher.test(c2$pd_allcrimes, c2$baldio)
fisher #La distribución es igual, p-valor mayor a 0.05

# Áreas verdes pequeñas

# Resumen de estadísticos
summ_areaverdepeque <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(areaverde_pequena_por, na.rm = TRUE),
    IQR = IQR(areaverde_pequena_por, na.rm = TRUE)
  )

summ_areaverdepeque

# Boxplot
ggboxplot(c2, x = "pd_allcrimes", y = "areaverde_pequena_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of small green areas", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(areaverde_pequena_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)
res# El p valor es mayor a 0.05 por lo que no hay diferencia significativa. 


# Alumbrado público

# Resumen de estadísticos
summ_light <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(sinalumbrado_por, na.rm = TRUE),
    IQR = IQR(sinalumbrado_por, na.rm = TRUE)
  )

summ_light

# Boxplot

ggboxplot(c2, x = "pd_allcrimes", y = "sinalumbrado_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of streets without public lighting", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(sinalumbrado_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.  

# Víctimas hombres
summ_hom <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(victimas_hombres_pob, na.rm = TRUE),
    IQR = IQR(victimas_hombres_pob, na.rm = TRUE)
  )

summ_hom

# Boxplot

ggboxplot(c2, x = "pd_allcrimes", y = "victimas_hombres_pob", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Male victims per inhabitant", xlab = "", 
          legend.title = "")  

# Wilcoxon test

res <- wilcox.test(victimas_hombres_pob ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res  # El p valor es mayor a 0.05 por lo no hay diferencia estadísticamente significativa. 

# Hora promedio

# Resumen de estadísticos
summ_hora <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_horamedia, na.rm = TRUE),
    IQR = IQR(allcrimes_horamedia, na.rm = TRUE)
  )

summ_hora

# Boxplot

ggboxplot(data=subset(c2, !is.na(allcrimes_horamedia)), na.omit=T, x = "pd_allcrimes", y = "allcrimes_horamedia", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Mean hour of crimes", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_horamedia ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 5am a 11am

# Resumen de estadísticos
summ_5to11 <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_5a11_por, na.rm = TRUE),
    IQR = IQR(allcrimes_5a11_por, na.rm = TRUE)
  )

summ_5to11

# Boxplot

ggboxplot(data=subset(c2, !is.na(allcrimes_5a11_por)), x = "pd_allcrimes", y = "allcrimes_5a11_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 5 to 11 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_5a11_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 12 to 16 hrs

# Resumen de estadísticos
summ_12to16 <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_12a16_por, na.rm = TRUE),
    IQR = IQR(allcrimes_12a16_por, na.rm = TRUE)
  )

summ_12to16

# Boxplot

ggboxplot(data=subset(c2, !is.na(allcrimes_12a16_por)), x = "pd_allcrimes", y = "allcrimes_12a16_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 12 to 16 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_12a16_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que aceptamos la H0 de que las medias no son iguales. 
# Hay diferencia estadísticamente significativa.


# Porcentaje de delitos ocurridos de 17 a 23 hrs
# Resumen de estadísticos
summ_17to23 <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_17a23_por, na.rm = TRUE),
    IQR = IQR(allcrimes_17a23_por, na.rm = TRUE)
  )

summ_17to23

# Boxplot

ggboxplot(data=subset(c2, !is.na(allcrimes_17a23_por)), x = "pd_allcrimes", y = "allcrimes_17a23_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 17 to 23 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_17a23_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que no hay diferencia estadísticamente significativa.

# Porcentaje de delitos ocurridos de 24 a 4 hrs
# Resumen de estadísticos
summ_24to4 <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_24a4_por, na.rm = TRUE),
    IQR = IQR(allcrimes_24a4_por, na.rm = TRUE)
  )

summ_24to4

# Boxplot
ggboxplot(data=subset(c2, !is.na(allcrimes_24a4_por)), x = "pd_allcrimes", y = "allcrimes_24a4_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes occurred from 24 to 4 hrs", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_24a4_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es mayor a 0.05 por lo que aceptamos la H0 de que las medias son iguales. 
# No hay diferencia estadísticamente significativa.


# Entre semana
# Resumen de estadísticos
summ_week <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_entresem_por, na.rm = TRUE),
    IQR = IQR(allcrimes_entresem_por, na.rm = TRUE)
  )

summ_week

# Boxplot
ggboxplot(data=subset(c2, !is.na(allcrimes_entresem_por)), x = "pd_allcrimes", y = "allcrimes_entresem_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the week", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_entresem_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que hay diferencia estadísticamente significativa. 
# En los PDs los delitos ocurren en una mayor proporción entre semana


# Fin de semana
# Resumen de estadísticos
summ_weekend <- group_by(c2, pd_allcrimes) %>%
  summarise(
    count = n(),
    median = median(allcrimes_finde_por, na.rm = TRUE),
    IQR = IQR(allcrimes_finde_por, na.rm = TRUE)
  )

summ_weekend

# Boxplot
ggboxplot(data=subset(c2, !is.na(allcrimes_finde_por)), x = "pd_allcrimes", y = "allcrimes_finde_por", 
          color = "pd_allcrimes", palette = c("#3c1642", "#2a9134"),
          ylab = "Percentage of crimes that occurred during the weekend", xlab = "", 
          legend.title = "")

# Wilcoxon test

res <- wilcox.test(allcrimes_finde_por ~ pd_allcrimes, data = c2,
                   exact = FALSE)

res # El p valor es menor a 0.05 por lo que hay diferencia estadísticamente significativa.
# En los PDs los delitos ocurren en menor proporción en el fin de semana

#### 4) Cluster 3 ####
# No hay PDS así que no se realiza la etapa de PD understanding