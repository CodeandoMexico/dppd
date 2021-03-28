rm(list=ls())

#Paquetes
library(readr)
library(tidyverse)
library(dplyr)
library(sf)
library(cluster)
library(fossil)

####Variables para construir grupos homogeneos####
#Abrir el marco geoestadistico de la CDMX a nivel ageb
agebs_sf <- st_read("Espacios publicos/Índice/inputs/inegi/marcogeo2020/cdmx/conjunto_de_datos/09a.shp")
agebs_sf = st_transform(agebs_sf, 4326)

#Abrir los datos del Censo 2020 para la CDMX
poblacion <- read_csv("Espacios publicos/Índice/inputs/inegi/censo2020/RESAGEBURB_09CSV20.csv",
                      locale= locale(encoding ="UTF-8"))

#Crear un identificador para cada AGEB
poblacion$ENTIDAD <- sprintf("%02d", poblacion$ENTIDAD)
poblacion$MUN <- sprintf("%03d", poblacion$MUN)
poblacion$LOC <- sprintf("%04d", poblacion$LOC)
poblacion$AGEB <- sprintf("%04s", poblacion$AGEB)
poblacion$AGEB <- gsub(" ", "0", poblacion$AGEB)

poblacion$CVEGEO <- paste0(poblacion$ENTIDAD, poblacion$MUN, poblacion$LOC,
                           poblacion$AGEB) %>% as.character()

#Seleccionar las variables de interes
poblacion <- poblacion %>% 
  filter(NOM_LOC=="Total AGEB urbana") %>% 
  dplyr::select(CVEGEO, POBTOT, POBMAS, POBFEM, TOTHOG, HOGJEF_F, REL_H_M, PEA, P_12YMAS,
         P15YM_SE, P_15YMAS, P_15A17, P_18A24, P_60YMAS)

poblacion[,2:14] <- lapply(poblacion[,2:14] , as.numeric)#Se introducen NA en vez de asteriscos (*) cuando hay valores faltantes

#Crear el porcentaje de poblacion por grupos de edad de 0-14, 15-24 y 25-59
poblacion <- poblacion %>% 
  dplyr::mutate(p_0a14 = POBTOT - P_15YMAS,
                p_15a24 = P_15A17 + P_18A24, 
                p_25a59 = POBTOT - (p_0a14 + p_15a24  + P_60YMAS),
                p_60ymas = P_60YMAS)

poblacion <- poblacion %>% 
  mutate_at(vars(14:17), list(~./POBTOT*100))

#Calcular la densidad de poblacion (personas por km2)
area <- agebs_sf %>% 
  mutate(area_ageb = as.numeric(st_area(agebs_sf$geometry)))
poblacion <- left_join(area[,c("CVEGEO", "area_ageb")], poblacion, by="CVEGEO")

poblacion <- poblacion %>% 
  mutate(area_agebkm2 = area_ageb/1000000,
         densidadpob = POBTOT/area_agebkm2) 

#Crear el porcentaje de poblacion economicamente activa, hogares con jefatura femenina y poblacion de 15 años y mas sin educacion formal
poblacion <- poblacion %>% 
  mutate(pea = PEA/P_12YMAS*100,
         hogfem = HOGJEF_F/TOTHOG*100,
         p_se = P15YM_SE/P_15YMAS*100, 
         p_mas = POBMAS/POBTOT*100,
         p_fem = POBFEM/POBTOT*100)

#Revisar los valores faltantes
colSums(is.na(poblacion)) #Vemos que hay 17 agebs sin informacion en el censo 
sininfo <- poblacion %>% 
  filter(POBTOT %in% NA)

#Ubicamos las AGEB que no tienen informacion del censo
ggplot()+
  geom_sf(data= sininfo, fill="blue")+
  geom_sf(data = agebs_sf, fill="transparent") #Se trata de AGEBs como el aeropuerto, la central de abastos, etc. 

#En las AGEB que tienen informacion del Censo pero hay valores faltantes, los reemplazamos con su valor promedio
caracter <- c("pea", "hogfem", "p_se", "p_mas", "p_fem", "REL_H_M", "p_0a14", "p_15a24", "p_25a59", "p_60ymas")

poblacion <- poblacion %>% 
  st_drop_geometry()

poblacion[,caracter] <- lapply(poblacion[,caracter], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

#Identificar las AGEB que son frontera con el Estado de Mexico
#Abrir el marco geoestadistico del Estado de Mexico
edomex_sf <- st_read("Espacios publicos/Índice/inputs/inegi/macrogeo/edomex/conjunto_de_datos/15a.shp")
edomex_sf <- st_transform(edomex_sf, 4326)

#Hacer la interseccion entre las AGEB de la CDMX y las del Edo Mex
int_edomex <- st_intersection(agebs_sf, edomex_sf)

#Identificar con 1 aquellas AGEB de la CDMX que intersectan con las del Edo Mex
int_edomex <- int_edomex %>% 
  mutate(edomex = 1) %>% 
  group_by(CVEGEO) %>% 
  filter(row_number()==1) %>% 
  st_drop_geometry()

poblacion <- left_join(poblacion, int_edomex[, c("CVEGEO", "edomex")], by="CVEGEO") 

poblacion <- poblacion %>% 
  mutate(edomex = ifelse(edomex %in% NA, 0, edomex))

#Abrir la base del Indice de Marginacion Urbana (IMU)
marginacion <- read_csv("Espacios publicos/Índice/inputs/conapo/base_IMU_conapo.csv",
                        locale= locale(encoding ="Latin1"))
#Seleccionar la CDMX
marginacion <- marginacion %>% 
  filter(ent == "09")

marginacion <- left_join(poblacion, marginacion[,c("cve_ageb", "GMU2010")], 
                       by= c("CVEGEO" = "cve_ageb")) %>% 
  left_join(agebs_sf, by="CVEGEO") %>% 
  st_as_sf()

sum(is.na(marginacion$GMU2010)) #Hay AGEBS que no tienen identificado grado de marginacion, asi que les asignamos el grado de marginacion mas frecuente de sus AGEBS vecinas

#Identificar a las AGEB vecinas 
interseccion <- st_intersection(marginacion, marginacion)
tempo <- interseccion %>% 
  filter(is.na(GMU2010)) %>% 
  st_drop_geometry()

#Escoger  el valor del grado de marginacion mas frecuente entre las AGEB vecinas para reemplazar los valores faltantes
tempo <- tempo %>% 
  group_by(CVEGEO) %>% 
  summarise(GMU2010 = names(table(GMU2010.1))[which.max(table(GMU2010.1))], .groups = 'drop')

marginacion <- marginacion %>% 
  left_join(tempo, by = "CVEGEO") %>%
  mutate(GMU = ifelse(is.na(GMU2010.x), GMU2010.y, GMU2010.x))
  
poblacion <- left_join(poblacion, marginacion[, c("CVEGEO", "GMU")], by="CVEGEO")
sum(is.na(poblacion$GMU))

#Calcular la afluencia de personas a las AGEB
#Abrir base de viajes de la EOD (incluir codigo)
viajes <- read_csv("Espacios publicos/Índice/inputs/inegi/EOD/viajes/EOD_viajes_destino.csv")
viajes$cve_distrito_destino <- as.character(viajes$cve_distrito_destino)
viajes$Distrito <- str_sub(viajes$cve_distrito_destino, -3, -1)

#Abrir el shape de los distritos de la EOD
distritos_sf <- st_read("Espacios publicos/Índice/inputs/inegi/EOD/distritos_shp/DistritosEODHogaresZMVM2017.shp")
st_crs(distritos_sf)
distritos_sf <- st_transform(distritos_sf, 4326)

#Unimos el shape de los distritos con los datos de la EOD
distritos_sf$area_distrito <- as.numeric(st_area(distritos_sf))
distritos_sf <- left_join(distritos_sf, viajes[, 2:5], by="Distrito")

#Quedarse unicamente con los 85 primeros distritos ya que son los que corresponden a la CDMX
distritos_sf <- distritos_sf %>% 
  filter(Distrito <= "085")

#Hacer la intereseccion de los distritos con las AGEB y calcular el area de las intersecciones
interseccion <- st_intersection(distritos_sf, area)
interseccion$areaint <- as.numeric(st_area(interseccion$geometry))
n_distinct(interseccion$CVEGEO)

#Una AGEB puede estar en mas de un distrito, por lo que se le asigna el valor del distrito que mas area ocupe en la AGEB
tempo <- interseccion %>% 
  mutate(proporcion = round(areaint*100/area_ageb, 2)) %>% 
  group_by(CVEGEO) %>% 
  filter(proporcion == max(proporcion)) %>% 
  st_drop_geometry()

poblacion <- left_join(poblacion, tempo[, c("CVEGEO", "total_viajes", 
                                            "total_viajes_mujeres", 
                                            "total_viajes_hombres")], by="CVEGEO")

####Cluster 1####
#Calculamos el cluster con todas las variables de interes
set.seed(123)
data <- poblacion %>% 
  dplyr::select(CVEGEO, densidadpob, p_0a14, p_15a24, p_25a59, p_60ymas, p_mas, p_fem, 
               hogfem, p_se, pea, total_viajes, GMU, edomex)

data <- transform(data, GMU=as.factor(GMU), edomex = as.factor(edomex))

#Revisar si las variables continuas siguen una distribucion normal
normalidad <- lapply(data[,2:12],shapiro.test)
normalidad #No siguen una distribucion normal

#Eliminar los ceros
colSums(is.na(data))
sum(is.na(data))
data <- na.omit(data)

#Guardar el identificador CVEGEO
CVEGEO<- data$CVEGEO

#Calculamos la distancia de gower, convirtiendo a logaritmo las variables continuas que no siguen una distribucion normal
gowerdistance <- daisy(data[, -1],
                       metric = "gower",
                       type = list(logratio = c(1, 11)))

summary(gowerdistance)

#Calculamos Silhouette para conocer el numero de clusters optimo
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gowerdistance),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette")
lines(silhouette)

#Escogemos 4 clusters para tener balance en el tamaño de los clusters y usamos el algoritmo PAM para construir los clusters
pam = pam(gowerdistance, diss = TRUE, k= 4)
medoids <- data[pam$medoids, ]
medoids
pam_summary <- data %>% 
  mutate(cluster = pam$clustering) %>% 
  group_by(cluster) %>% 
  do(cluster_summary = summary(.))

#Resumen cluster 1
pam_summary$cluster_summary

#Unimos el cluster con el identificador de las AGEB
cluster <- as.data.frame(cbind(pam$clustering, CVEGEO))
cluster <- cluster %>% 
  rename(c1 = V1)

####Cluster 2####
#Calculamos un cluster unicamente con densidad de poblacion, afluencia de viajes y grado de marginacion
data <- poblacion %>% 
  dplyr::select(CVEGEO, densidadpob, total_viajes, GMU)

data <- transform(data, GMU=as.factor(GMU))

#Revisar si las variables continuas siguen una distribucion normal
normalidad <- lapply(data[,2:3],shapiro.test)
normalidad #No siguen una distribucion normal

#Eliminar los ceros
colSums(is.na(data))
sum(is.na(data))
data <- na.omit(data)

#Guardar el identificador CVEGEO
CVEGEO<- data$CVEGEO

#Calculamos la distancia de gower, convirtiendo a logaritmo las variables continuas
gowerdistance <- daisy(data[, -1],
                  metric = "gower",
                  type = list(logratio = c(1, 2)))

summary(gowerdistance)

#Calculamos Silhouette para conocer el numero de clusters optimo
silhouette <- c()
silhouette = c(silhouette, NA)
for(i in 2:10){
  pam_clusters = pam(as.matrix(gowerdistance),
                     diss = TRUE,
                     k = i)
  silhouette = c(silhouette, pam_clusters$silinfo$avg.width)
}
plot(1:10, silhouette,
     xlab = "Clusters",
     ylab = "Silhouette")
lines(silhouette)

#Escogemos 4 clusters
pam = pam(gowerdistance, diss = TRUE, k= 4)
medoids <- data[pam$medoids, ]
medoids
pam_summary <- data %>% 
  mutate(cluster = pam$clustering) %>% 
  group_by(cluster) %>% 
  do(cluster_summary = summary(.))

#Resumen de cluster 2
pam_summary$cluster_summary

#Unir el cluster 2 con el identificador de la AGEB y ver cuantas AGEB hay por cluster
cluster2 <- as.data.frame(cbind(pam$clustering, CVEGEO))
cluster2 <- cluster2 %>% 
  rename(c2 = V1)
table(cluster2$c2)

#Para conocer la similitud entre ambos clusters calculamos el rand index
cluster <- left_join(cluster, cluster2, by= "CVEGEO")
cluster <- transform(cluster, c1=as.numeric(c1), c2= as.numeric(c2))
rand.index(cluster$c1, cluster$c2) #Muy cercano a 1, lo que significa que ambos clusters son muy similares. Escogemos el cluster 2 ya que es mas sintetico.

#Mapa de las AGEB de la CDMX por cluster
poblacion <- left_join(poblacion, cluster2, by="CVEGEO") %>% 
  st_as_sf()

ggplot() +
  geom_sf(data= poblacion, aes(fill = c2)) +
  scale_fill_manual(values = c("#f28482", "#00b4d8", "#06d6a0", "#ffd166", "#073b4c")) +
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=50),
        legend.text = element_text(size=30))

#Guardar mapa
ggsave("Espacios publicos/Índice/mapas/cluster.jpg", width = 25, height = 30)

#Guardar base 
basecluster <- poblacion %>% 
  rename(cluster = c2) %>% 
  dplyr::select(CVEGEO, cluster, POBTOT, p_mas, p_fem, p_0a14, p_15a24, p_25a59, p_60ymas,
         REL_H_M, pea, hogfem, p_se, GMU, edomex, densidadpob, total_viajes, area_agebkm2) %>% 
  st_drop_geometry()

write.csv(basecluster, "Espacios publicos/PD identification/Output/bases/basecluster.csv", row.names = F)
