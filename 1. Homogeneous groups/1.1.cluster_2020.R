rm(list=ls())

# Paquetes
library(readr)
library(tidyverse)
library(dplyr)
library(sf)
library(fossil)
library(rgdal)
library(leaflet)
library(mapview)
library(factoextra)
library(FactoMineR)
library(dendextend)

#### Crear grupos homogéneos ####

#### 1) Crear las variables para construir los grupos homogéneos ####

# Abrir el marco geoestadistico 2020 de la CDMX a nivel AGEB con readOGR
agebs <- readOGR("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp")

# Conocer la proyección
proj4string(agebs)

# Reproyectar con spTransform
agebs <- sp::spTransform(agebs, CRS("+proj=longlat +datum=WGS84 +no_defs"))
proj4string(agebs)

# Convertir a sf y especificar que el sistema de referencia de coordenadas (CRS) sea 4326
agebs_sf <- st_as_sf(agebs)
st_crs(agebs_sf)

st_crs(agebs_sf) = st_crs(4326)
st_crs(agebs_sf)

### 1.1) Utilizar los datos del Censo 2020 para crear variables sobre la población en las AGEBS urbanas ###

# Abrir el censo 2020
poblacion <- read_csv("censo2020/RESAGEBURB_09CSV20.csv",
                      locale= locale(encoding ="UTF-8"))

# Crear un identificador para las AGEBS
poblacion$ENTIDAD <- sprintf("%02d", poblacion$ENTIDAD)
poblacion$MUN <- sprintf("%03d", poblacion$MUN)
poblacion$LOC <- sprintf("%04d", poblacion$LOC)
poblacion$AGEB <- sprintf("%04s", poblacion$AGEB)
poblacion$AGEB <- gsub(" ", "0", poblacion$AGEB)

poblacion$CVEGEO <- paste0(poblacion$ENTIDAD, poblacion$MUN, poblacion$LOC,
                           poblacion$AGEB) %>% as.character()

# Filtrar por nivel AGEB y seleccionar las variables que vamos a utilizar
poblacion <- poblacion %>% 
  filter(NOM_LOC=="Total AGEB urbana") %>% 
  dplyr::select(CVEGEO, POBTOT, POBMAS, POBFEM, TOTHOG, HOGJEF_F, REL_H_M, PEA, P_12YMAS,
                PRES2015, P_5YMAS, P15YM_SE, P_15YMAS, P_15A17, P_18A24, P_60YMAS, P_6A11, 
                P_12A14, P6A11_NOA, P12A14NOA, P15YM_SE, P15PRI_IN, P15PRI_CO, P15SEC_IN, 
                PSINDER, VIVTOT, TVIVHAB, TVIVPAR, VIVPAR_HAB, TVIVPARHAB, VPH_PISODT, 
                VPH_PISOTI, VPH_AGUADV, VPH_AEASP, VPH_AGUAFV, VPH_EXCSA, VPH_LETR, VPH_DRENAJ,
                VPH_NODREN, PRO_OCUP_C, VPH_LAVAD, VPH_REFRI, VPH_PC, VPH_INTER, VPH_SINCINT)

# Convertirlas en valores numericos
poblacion[,2:44] <- lapply(poblacion[,2:44] , as.numeric)# Se introducen NA en vez de asteriscos (*) cuando hay valores faltantes

# Obtener la poblacion por grupos de edad de 0 a 14, 15 a 24, 25 a 59 y 60 y más
poblacion <- poblacion %>% 
  dplyr::mutate(P_0A14 = POBTOT - P_15YMAS,
                P_15A24 = P_15A17 + P_18A24, 
                P_25A59 = POBTOT - (P_0A14 + P_15A24  + P_60YMAS),
                P_60YMAS = P_60YMAS)

# Obtener el porcentaje de población por grupos de edad
poblacion <- poblacion %>% 
  mutate_at(vars(c(P_0A14, P_15A24, P_25A59, P_60YMAS)), list(~./POBTOT*100))

# Obtener la densidad de población (población/area)
# Calcular el área de las AGEBS y unir esa variable a la base de población
area <- agebs_sf %>% 
  mutate(area_ageb = as.numeric(st_area(agebs_sf$geometry)))

poblacion <- left_join(area[,c("CVEGEO", "area_ageb")], poblacion, by="CVEGEO")

# Convertir el area de metros cuadrados a km cuadrados y calcular la densidad de población
poblacion <- poblacion %>% 
  mutate(area_agebkm = area_ageb/1000000,
         densidadpob = POBTOT/area_agebkm) 

# Calcular la población femenina y masculina a partir de la población total para recuperar valores perdidos
poblacion <- poblacion %>% 
  mutate(POBMAS = ifelse(POBMAS %in%NA, POBTOT*.5, POBMAS),
         POBFEM = ifelse(POBFEM %in%NA, POBTOT*.5, POBFEM)) 

# Obtener el porcentaje de población economicamente activa, hogares con jefatura femenina, población de 15 años y mas sin educación formal y población residente en la misma entidad federativa en los últimos 5 años
poblacion <- poblacion %>% 
  mutate(PEA = PEA/P_12YMAS*100,
         HOGFEM = HOGJEF_F/TOTHOG*100,
         P_SE = P15YM_SE/P_15YMAS*100, 
         P_MAS = POBMAS/POBTOT*100,
         P_FEM = POBFEM/POBTOT*100,
         PRES2015 = PRES2015/P_5YMAS*100)

summary(poblacion$PEA)

# Reemplazar los valores faltantes de las variables anteriores con su valor medio
variables <- c("PEA", "HOGFEM", "P_SE", "P_MAS", "P_FEM", "REL_H_M", "P_0A14", "P_15A24", "P_25A59", "P_60YMAS", "PRES2015")

poblacion <- poblacion %>% 
  st_drop_geometry()

poblacion[,variables] <- lapply(poblacion[,variables], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

### 1.2) Crear variables sobre el nivel de marginación de las AGEBS ###

# Crear el porcentaje de población de 6 a 14 años que no asiste a la escuela
poblacion <- poblacion %>% 
  dplyr::mutate(P6A14_NOA = P6A11_NOA + P12A14NOA,
                P_6A14 = P_6A11 + P_12A14,
                P6A14_NOA = P6A14_NOA/P_6A14*100)

# Sustituir los valores faltantes con el valor promedio de la variable
poblacion$P6A14_NOA[is.na(poblacion$P6A14_NOA)] <- mean(poblacion$P6A14_NOA, na.rm = TRUE)

# Crear el porcentaje de población de 15 años o más sin educación básica completa
poblacion <- poblacion %>% 
  dplyr::mutate(P15YM_SEBC = P15YM_SE + P15PRI_IN + P15PRI_CO + P15SEC_IN,
                P15YM_SEBC = P15YM_SEBC/P_15YMAS*100)

poblacion$P15YM_SEBC[is.na(poblacion$P15YM_SEBC)] <- mean(poblacion$P15YM_SEBC, na.rm = TRUE)

# Crear el porcentaje de la población sin derechohabiencia a los servicios de salud
poblacion <- poblacion %>% 
  dplyr::mutate(PSINDER = PSINDER/POBTOT*100)

poblacion$PSINDER[is.na(poblacion$PSINDER)] <- mean(poblacion$PSINDER, na.rm = TRUE)

# Crear el porcentaje de viviendas particulares habitadas que no disponen de drenaje
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_NODREN = ifelse(VPH_NODREN %in% NA, TVIVPARHAB- VPH_DRENAJ, VPH_NODREN), # Para recuperar los NA cuando hay 3 o menos viviendas por AGEB
                VPH_NODREN = VPH_NODREN/TVIVPARHAB*100)

poblacion$VPH_NODREN[is.na(poblacion$VPH_NODREN)] <- mean(poblacion$VPH_NODREN, na.rm = TRUE)

# Crear el porcentaje de viviendas particulares habitadas sin excusado con conexción de agua (con letrina, pozo u hoyo)
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_LETR = ifelse(VPH_LETR %in% NA, TVIVPARHAB- VPH_EXCSA, VPH_LETR),
                VPH_LETR = VPH_LETR/TVIVPARHAB*100)

poblacion$VPH_LETR[is.na(poblacion$VPH_LETR)] <- mean(poblacion$VPH_LETR, na.rm = TRUE)

# Crear el porcentaje de viviendas particulares habitadas que no disponen de agua entubada en el ámbito de la vivienda
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_AGUAFV = ifelse(VPH_AGUAFV %in% NA, TVIVPARHAB- VPH_AGUADV, VPH_AGUAFV),
                VPH_AGUAFV = VPH_AGUAFV/TVIVPARHAB*100)

poblacion$VPH_AGUAFV[is.na(poblacion$VPH_AGUAFV)] <- mean(poblacion$VPH_AGUAFV, na.rm = TRUE)

# Crear el porcentaje de viviendas particulares habitadas con piso de tierra
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_PISOTI = ifelse(VPH_PISOTI %in% NA, TVIVPARHAB- VPH_PISODT, VPH_PISOTI),
                VPH_PISOTI = VPH_PISOTI/TVIVPARHAB*100)

poblacion$VPH_PISOTI[is.na(poblacion$VPH_PISOTI)] <- mean(poblacion$VPH_PISOTI, na.rm = TRUE)

# Obtener el promedio de ocupantes por cuarto en viviendas particulares habitadas
summary(poblacion$PRO_OCUP_C)

poblacion$PRO_OCUP_C[is.na(poblacion$PRO_OCUP_C)] <- mean(poblacion$PRO_OCUP_C, na.rm = TRUE)

# Obtener el porcentaje de viviendas particulares habitadas que no disponen de lavadora
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_SINLAVAD = TVIVPARHAB- VPH_LAVAD,
                VPH_SINLAVAD = VPH_SINLAVAD/ TVIVPARHAB *100)

poblacion$VPH_SINLAVAD[is.na(poblacion$VPH_SINLAVAD)] <- mean(poblacion$VPH_SINLAVAD, na.rm = TRUE)

# Obtener el porcentaje de viviendas particulares habitadas que no disponen de refrigerador
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_SINREFRI = TVIVPARHAB- VPH_REFRI,
                VPH_SINREFRI = VPH_SINREFRI/ TVIVPARHAB *100)

poblacion$VPH_SINREFRI[is.na(poblacion$VPH_SINREFRI)] <- mean(poblacion$VPH_SINREFRI, na.rm = TRUE)

# Obtener el porcentaje de viviendas particulares habitadas que no disponen de computadora, tablet o laptop
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_SINPC = TVIVPARHAB- VPH_PC,
                VPH_SINPC = VPH_SINPC/ TVIVPARHAB *100)

poblacion$VPH_SINPC[is.na(poblacion$VPH_SINPC)] <- mean(poblacion$VPH_SINPC, na.rm = TRUE)

# Obtener el porcentaje de viviendas particulares habitadas que no disponen de internet 
poblacion <- poblacion %>% 
  dplyr::mutate(VPH_SININTER = TVIVPARHAB- VPH_INTER,
                VPH_SININTER = VPH_SININTER/ TVIVPARHAB *100)

poblacion$VPH_SININTER[is.na(poblacion$VPH_SININTER)] <- mean(poblacion$VPH_SININTER, na.rm = TRUE)

### 1.3) Construir una variable que indique las AGEBS de la CDMX que son frontera con el Estado de México ###

# Abrir el marco geoestadistico del Estado de Mexico con readOGR
edomex <- readOGR("marco_geoestadistico/edomex/conjunto_de_datos/15a.shp")

# Reproyectar con spTransform
edomex <- sp::spTransform(edomex, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Convertir a sf y especificar CRS 4326
edomex_sf <- st_as_sf(edomex)

st_crs(edomex_sf) = st_crs(4326)
st_crs(edomex_sf)

# Hacer la interseccion entre las AGEBS de la CDMX y las del Estado de México
int_edomex <- st_intersection(agebs_sf, edomex_sf)

# Identificar con 1 las AGEBS de la CDMX que intersectan con las del Edo Mex
int_edomex <- int_edomex %>% 
  mutate(edomex = 1) %>% 
  group_by(CVEGEO) %>% 
  filter(row_number()==1) %>% 
  st_drop_geometry()

poblacion <- left_join(poblacion, int_edomex[, c("CVEGEO", "edomex")], by="CVEGEO") 

# Identificar con cero las AGEBS que no intersectan con el Edo Mex
poblacion <- poblacion %>% 
  mutate(edomex = ifelse(edomex %in% NA, 0, edomex))

### 1.4) Calcular la afluencia a las AGEB (viajes) con la Encuesta Origen Destino (EOD) ###

# Abrir base de viajes de la EOD (incluir codigo)
viajes <- read_csv("EOD/viajes/EOD_viajes_destino.csv")

# Quitar los dígitos de la entidad a la clave del distrito 
viajes$cve_distrito_destino <- as.character(viajes$cve_distrito_destino)
viajes$Distrito <- str_sub(viajes$cve_distrito_destino, -3, -1)

# Abrir el shape de los distritos de la EOD
distritos_sf <- st_read("EOD/distritos_shp/DistritosEODHogaresZMVM2017.shp")
st_crs(distritos_sf)
distritos_sf <- st_transform(distritos_sf, 4326)

# Calcular el area de los distritos y unir el shape de los distritos con los datos de la EOD
distritos_sf$area_distrito <- as.numeric(st_area(distritos_sf))
distritos_sf <- left_join(distritos_sf, viajes[, 2:5], by="Distrito")

# Quedarse unicamente con los 85 primeros distritos ya que son los que corresponden a la CDMX
distritos_sf <- distritos_sf %>% 
  filter(Distrito <= "085")

# Hacer la intereseccion de los distritos con las AGEBS y calcular el area de las intersecciones
interseccion <- st_intersection(distritos_sf, area)
interseccion$areaint <- as.numeric(st_area(interseccion$geometry))
n_distinct(interseccion$CVEGEO)

# Una AGEB puede estar en mas de un distrito, por lo que se le asigna el valor del distrito que más area ocupa en la AGEB
tempo <- interseccion %>% 
  mutate(proporcion = round(areaint*100/area_ageb, 2)) %>% 
  group_by(CVEGEO) %>% 
  filter(proporcion == max(proporcion)) %>% 
  st_drop_geometry()

# Pegar las variables de la EOD a nivel AGEB a la base de población
poblacion <- left_join(poblacion, tempo[, c("CVEGEO", "total_viajes", 
                                            "total_viajes_mujeres", 
                                            "total_viajes_hombres")], by="CVEGEO")

# Unir la base de poblacion al shape de las AGEBS 
poblacion <- left_join(poblacion, agebs_sf, by="CVEGEO")

poblacion <- poblacion %>% 
  st_as_sf()

## 1.5) Identificar las AGEBS que no tienen información en el Censo 2020
sum(is.na(poblacion$POBTOT)) # El censo no presenta información de 17 AGEBs

# Ubicar las AGEBS que no tienen informacion del censo en un mapa
sininfo <- poblacion %>% 
  filter(POBTOT %in% NA)

# Mapa leaflet de las AGEBS sin información del censo para identificar cuáles son

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=sininfo,
              fillColor = "blue",
              color = "blue",
              weight = 1) %>% 
  addPolygons(data=agebs,
  fillColor= "transparent",
  color= "black",
  weight = 1) -> mapa

mapa

mapshot(mapa, file = paste0("mapa_agebs.jpg"))

#### 2) Crear los grupos homogéneos ####

### 2.1) Cluster con todas las variables ###

#Nos quedamos con todas las variables creadas
cluster_todas <- poblacion %>% 
  dplyr::select(CVEGEO, P6A14_NOA, P15YM_SEBC, PSINDER, VPH_NODREN, 
                VPH_LETR, VPH_AGUAFV, VPH_PISOTI, PRO_OCUP_C,
                VPH_SINLAVAD, VPH_SINREFRI, VPH_SINPC, VPH_SININTER,
                densidadpob, total_viajes, REL_H_M, HOGFEM, PEA, PRES2015,
                edomex, P_0A14, P_15A24, P_25A59, P_60YMAS) %>% 
  st_drop_geometry()

# Revisar los valores faltantes 
colSums(is.na(cluster_todas)) # Son los 17 para los que no se tiene información del censo

# Eliminar los valores faltantes
cluster_todas <- na.omit(cluster_todas)

# Guardar la clave de las AGEBS
CVEGEO <- data.frame(cluster_todas[,1])

# Hacer una matriz de correlaciones de las variables numéricas
corr <- cor(cluster_todas[, -c(1, 20)])

# Se elimina la variabel P6A14_NOA porque la correlación con el resto de las variables sobre marginación es muy baja y en signo contrario al esperado
cluster_todas <- cluster_todas %>% 
  dplyr::select(- P6A14_NOA)

cluster_todas <- na.omit(cluster_todas)

CVEGEO <- data.frame(cluster_todas[,1])

# Hacer un PCA con todas las variables 
pca <- prcomp(cluster_todas[,-1], center = TRUE, scale = TRUE)
summary(pca)

# Número de componentes principales de acuerdo con el método del codo (Elbow method)
plot(pca, type='l') # Seleccionar los seis primeros componentes

# Seleccionar los seis primeros componentes
comp <- data.frame(pca$x[,1:6])

# Seleccionar el número óptimo de clusters 
fviz_nbclust(comp, FUN=kmeans, method = "silhouette")  + # El número recomendado es dos
  theme(axis.text.x = element_text(size = 40, color = "black"), title = element_text(size = 50, color = "black"),
        axis.text.y = element_text(size = 40)) +
  geom_line(aes(group = 1), color = "#238A8DFF", size = 2) + 
  geom_point(group = 1, size = 5, color = "#238A8DFF") +
  geom_vline(xintercept = 2, group = 1, size = 0.8, color = "#238A8DFF", linetype = "longdash") 

ggsave("silhouette_cluster.jpg", width = 30, height = 16)

# Hacer el cluster con el método de K medias con dos grupos
set.seed(555)
modelo <- kmeans(comp, centers = 2)

# Agregar la variable cluster a la base 
comp <- comp %>% 
  mutate(cluster_todas = modelo$cluster)

# Invertir cluster 1 y 2
comp <- comp %>% 
  mutate(cluster_todas = (cluster_todas),
         cluster_todas = case_when(
           cluster_todas == "1" ~ "2",
           cluster_todas == "2" ~ "1",
         ))

# Conocer el valor medio de las variables por clusters
cluster_todas <- cbind(cluster_todas, comp)
colnames(cluster_todas)

cluster_resumen  <-  cluster_todas %>% 
  group_by(cluster_todas) %>% 
  summarise(total = n(), 
            media_pc1 = mean(PC1), 
            media_pc2 = mean(PC2),
            media_pc3 = mean(PC3),
            media_pc4 = mean(PC4),
            media_pc5 = mean(PC5),
            media_pc6 = mean(PC6),
            media_P15YM_SEBC = mean(P15YM_SEBC),
            media_PSINDER = mean(PSINDER),
            media_VPH_NODREN = mean(VPH_NODREN),
            media_VPH_LETR = mean(VPH_LETR),
            media_VPH_AGUAFV = mean(VPH_AGUAFV),
            media_VPH_PISOTI = mean(VPH_PISOTI),
            media_PRO_OCUP_C = mean(PRO_OCUP_C),
            media_VPH_SINLAVAD = mean(VPH_SINLAVAD),
            media_VPH_SINREFRI = mean(VPH_SINREFRI),
            media_VPH_SINPC = mean(VPH_SINPC),
            media_VPH_SININTER = mean(VPH_SININTER),
            media_densidadpob = mean(densidadpob),
            media_total_viajes = mean(total_viajes),
            media_REL_H_M = mean(REL_H_M),
            media_HOGFEM = mean(HOGFEM),
            media_PEA = mean(PEA),
            media_PRES2015 = mean(PRES2015),
            prop_edomex = sum(edomex)/total*100,
            media_P_0A14 = mean(P_0A14),
            media_P_15A24 = mean(P_15A24),
            media_P_25A59 = mean(P_25A59))


# Mapa cluster con todas las variables
cluster_todas <- left_join(agebs_sf, cluster_todas, by = "CVEGEO")
cluster_todas$cluster_todas <- as.factor(cluster_todas$cluster_todas)

cluster_todas %>% 
  filter(!(cluster_todas %in% NA)) %>% 
  ggplot() +
  geom_sf(aes(fill = cluster_todas), color="black", size= 0.6) +
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=60),
        legend.text = element_text(size=40))+ 
  labs(fill="Homogeneous grouping") +
  scale_fill_viridis_d(
    guide = guide_legend(keyheight = unit(15, units = "mm"), keywidth=unit(50, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) 
  ) +
  theme(plot.background=element_rect(fill = "#ececec"),
        legend.background = element_rect(fill = "#ececec", color = NA),
        legend.position = c(0.8, 0.04))

ggsave("cluster.jpg", width = 35, height = 40)


# Mapa Leaflet del cluster con todas las variables

pal <- colorFactor(
  palette = c('#481567FF', '#1F968BFF'),
  domain = cluster_todas$cluster_todas,
  na.color = NA
)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=cluster_todas,
              color = ~pal(cluster_todas),
              weight = 1,
              opacity = 0.7,
              fillOpacity = 0.7)%>% 
  addPolygons(data= agebs,
              fillColor = "transparent",
              color= "black",
              weight = 1) %>% 
  addLegend("bottomright", pal = pal, values = cluster_todas$cluster_todas,
            title = "Homogeneous grouping",
            na.label = "",
            opacity = 1)-> mapa

mapa

mapshot(mapa, file = paste0("cluster_leaflet.jpg"))


### 2.2) Cluster 1 ###

# Quedarnos únicamente con las variables sobre marginación, densidad poblacional y viajes totales
cluster <- poblacion %>% 
  dplyr::select(CVEGEO, P6A14_NOA, P15YM_SEBC, PSINDER, VPH_NODREN, 
                VPH_LETR, VPH_AGUAFV, VPH_PISOTI, PRO_OCUP_C,
                VPH_SINLAVAD, VPH_SINREFRI, VPH_SINPC, VPH_SININTER,
                densidadpob, total_viajes) %>% 
  st_drop_geometry()

# Revisar los valores faltantes 
colSums(is.na(cluster)) # Son los 17 para los que no se tiene información del censo

# Eliminar los valores faltantes
cluster <- na.omit(cluster)

# Guardar la clave de las AGEBS
CVEGEO <- data.frame(cluster[,1])

# Hacer una matriz de correlaciones
corr <- cor(cluster[,-1]) 

# Se elimina la variabel P6A14_NOA porque la correlación con el resto de las variables es muy baja y en signo contrario al esperado
cluster <- cluster %>% 
  dplyr::select(- P6A14_NOA) 

cluster <- na.omit(cluster)

CVEGEO <- data.frame(cluster[,1])

# Hacer un PCA con el conjunto de variables 
pca <- prcomp(cluster[,-1], center = TRUE, scale = TRUE)
summary(pca)

# Número de componentes principales de acuerdo con el método del codo (Elbow method)
plot(pca, type='l') # Seleccionar los tres primeros componentes

# Seleccionar los tres primeros componentes
comp <- data.frame(pca$x[,1:3])

# Seleccionar el número óptimo de clusters con el método de la silueta
fviz_nbclust(comp, FUN=kmeans, method = "silhouette")  + # El número recomendado es tres
  theme(axis.text.x = element_text(size = 40, color = "black"), title = element_text(size = 50, color = "black"),
        axis.text.y = element_text(size = 40)) +
  geom_line(aes(group = 1), color = "#238A8DFF", size = 3) + 
  geom_point(group = 1, size = 5, color = "#238A8DFF") +
  geom_vline(xintercept = 3, group = 1, size = 0.8, color = "#238A8DFF", linetype = "longdash") 

ggsave("silhouette_cluster1.jpg", width = 30, height = 16)

# Hacer el cluster con el método de K medias con tres grupos
set.seed(555)
modelo <- kmeans(comp, centers = 3)

# Agregar la variable cluster a la base 
comp <- comp %>% 
  mutate(cluster = modelo$cluster)

# Invertir cluster 1 y 2 
comp <- comp %>% 
  mutate(cluster = (cluster),
         cluster = case_when(
           cluster == "1" ~ "2",
           cluster == "2" ~ "1",
           cluster == "3" ~ "3",
         ))

# Conocer el valor medio de las variables por clusters
cluster <- cbind(cluster, comp)
colnames(cluster)

# Agregar las variables que no se incluyeron en el cluster para conocer sus valores medios
cluster <- left_join(cluster, poblacion[, c("CVEGEO", "REL_H_M", "HOGFEM", "PEA", "PRES2015", 
                                            "edomex", "P_0A14", "P_15A24", "P_25A59", "P_60YMAS")],
                     by= "CVEGEO")

cluster_resumen  <-  cluster %>% 
  group_by(cluster) %>% 
  summarise(total = n(), 
            media_pc1 = mean(PC1), 
            media_pc2 = mean(PC2),
            media_pc3 = mean(PC3),
            media_densidadpob = mean(densidadpob),
            media_total_viajes = mean(total_viajes),
            media_P15YM_SEBC = mean(P15YM_SEBC),
            media_PSINDER = mean(PSINDER),
            media_VPH_NODREN = mean(VPH_NODREN),
            media_VPH_LETR = mean(VPH_LETR),
            media_VPH_AGUAFV = mean(VPH_AGUAFV),
            media_VPH_PISOTI = mean(VPH_PISOTI),
            media_PRO_OCUP_C = mean(PRO_OCUP_C),
            media_VPH_SINLAVAD = mean(VPH_SINLAVAD),
            media_VPH_SINREFRI = mean(VPH_SINREFRI),
            media_VPH_SINPC = mean(VPH_SINPC),
            media_VPH_SININTER = mean(VPH_SININTER),
            media_REL_H_M = mean(REL_H_M),
            media_HOGFEM = mean(HOGFEM),
            media_PEA = mean(PEA),
            media_PRES2015 = mean(PRES2015),
            prop_edomex = sum(edomex)/total*100,
            media_P_0A14 = mean(P_0A14),
            media_P_15A24 = mean(P_15A24),
            media_P_25A59 = mean(P_25A59),
            media_P_60YMAS = mean(P_60YMAS))

# Mapa cluster 1
cluster <- left_join(agebs_sf, cluster, by = "CVEGEO")
cluster$cluster <- as.factor(cluster$cluster)

cluster %>% 
  filter(!(cluster %in% NA)) %>% 
  ggplot() +
  geom_sf(aes(fill = cluster), color="black", size= 0.6) +
  #scale_fill_manual(values = c("#f28482", "#00b4d8", "#06d6a0", "#ffd166")) +
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=60),
        legend.text = element_text(size=60))+ 
  labs(fill="Homogeneous grouping") +
  scale_fill_viridis_d(
    guide = guide_legend(keyheight = unit(25, units = "mm"), keywidth=unit(45, units = "mm"), label.position = "bottom", title.position = 'top', label.hjust = 0.5, nrow=1) 
  ) +
  theme(plot.background=element_rect(fill = "#ececec"),
        legend.background = element_rect(fill = "#ececec", color = NA),
        legend.position = c(0.8, 0.04))

ggsave("cluster1.jpg", width = 35, height = 40)

# Mapa cluster 1 con Leaflet

pal <- colorFactor(
  palette = c('#481567FF', '#1F968BFF', '#FDE725FF'),
  domain = cluster$cluster,
  na.color = "transparent"
)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=cluster,
              color = ~pal(cluster),
              weight = 1,
              opacity = 0.7,
              fillOpacity = 0.7)%>% 
  addPolygons(data= agebs,
              fillColor = "transparent",
              color= "black",
              weight = 1) %>% 
  addLegend("bottomright", pal = pal, values = cluster$cluster,
            title = "Homogeneous grouping",
            na.label = "",
            opacity = 1)-> mapa

mapa

mapshot(mapa, file = paste0("cluster1_leaflet.jpg"))

#### 3) Rand index ####

cluster_todas <- cluster_todas %>% 
  filter(!is.na(cluster_todas)) %>% 
  st_drop_geometry()

cluster <- cluster %>% 
  filter(!is.na(cluster)) %>% 
  st_drop_geometry()

# Convertir en numéricas las variables
cluster_todas$cluster_todas <- as.numeric(cluster_todas$cluster_todas)
cluster$cluster <- as.numeric(cluster$cluster)

# Ejecutar el rand index entre ambos clusters 
rand.index(cluster_todas$cluster_todas, cluster$cluster)# El rand index es de 0.785

#### 4) Crear variable de bienestar socioeconómico ####

# Crear una sola variable que resuma las variables de marginación
marginacion <- poblacion %>% 
  dplyr::select(CVEGEO, P15YM_SEBC, PSINDER, VPH_NODREN, 
                VPH_LETR, VPH_AGUAFV, VPH_PISOTI, PRO_OCUP_C,
                VPH_SINLAVAD, VPH_SINREFRI, VPH_SINPC, VPH_SININTER) %>% 
  st_drop_geometry()

# Eliminar los valores faltantes
colSums(is.na(marginacion)) 
marginacion <- na.omit(marginacion)

# Guardar la clave de las AGEBS
CVEGEO <- data.frame(marginacion[,1])

# Hacer un PCA con el conjunto de variables 
pca_marginacion <- prcomp(marginacion[,-1], center = TRUE, scale = TRUE)
summary(pca_marginacion)

# Escogemos el primer componente principal ya que explica el 53.3% de la varianza
comp_marginacion <- data.frame(pca_marginacion$x[,1])

# Pegamos el primer componente a la base 
marginacion <- cbind(marginacion, comp_marginacion)

# Ya que el primer componente es positivo cuando hay menor marginación, renombramos la variable como bienestar socioeconómico (bienestar_socioeco)
marginacion <- marginacion %>% 
  mutate(bienestar_socioeco = pca_marginacion.x...1.)

# Guardar las variables que vamos a utilizar en la identificación de PDS 
base <- poblacion %>% 
  select(CVEGEO, POBTOT, POBMAS, POBFEM, area_agebkm, densidadpob, REL_H_M, HOGFEM, PEA, PRES2015,
         total_viajes, edomex, P_0A14, P_15A24, P_25A59, P_60YMAS, P_MAS, P_FEM) %>% 
  st_drop_geometry()

base <- left_join(base, marginacion[,c("CVEGEO", "bienestar_socioeco")], by="CVEGEO")
base <- left_join(base, cluster[,c("CVEGEO", "cluster")], by="CVEGEO")

write.csv(base, "basecluster.csv", row.names = F)

