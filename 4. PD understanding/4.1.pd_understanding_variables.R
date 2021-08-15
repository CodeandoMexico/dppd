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

# Cargar la base de datos creada en el script 2.1.performance_measure_beforecovid
base <- read_csv("baseageb.csv")

# Revisar los valores perdidos 
colSums(is.na(base))# Hay 17 AGEBS que no tienen información sobre la población (el aeropuerto, panteones, la central de abastos, etc.). 
base <- na.omit(base)# Eliminamos los valores perdidos

#### 1) Crear las variables para la etapa de PD understanding ####

# 1.1) Cercanía con estación de policías

# Abrir el marco geoestadistico de la CDMX a nivel AGEB
# Abrir con readOGR
agebs <- readOGR("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp")

# Reproyectar con spTransform
agebs <- sp::spTransform(agebs, CRS("+proj=longlat +datum=WGS84 +no_defs"))

# Convertir a sf y especificar que el sistema de referencia de coordenadas (CRS) sea 4326
agebs_sf <- st_as_sf(agebs)
st_crs(agebs_sf) = st_crs(4326)

# Calcular los centroides de las AGEBS
centrosageb <- st_centroid(agebs_sf)

# Cargar la base del DENUE 
denue_sf <- st_read("DENUE/denue_inegi_09_.shp")

# Seleccionar las estaciones de policía 
poli <- denue_sf %>% 
  dplyr::select(nom_estab, codigo_act, nombre_act, latitud, longitud) %>% 
  filter(str_detect(nom_estab, "ESTACION DE POLICIA|BASE DE POLICIA|MODULO DE POLICIA|MODULO DE SEGURIDAD|ESTACIÓN DE POLICIA|MODULO DE LA POLICIA|MODULO DE VIGILANCIA|MODULO POLICIA"))

# Calcular la distancia entre el centro de la ageb y la estación de policía
st_crs(poli) = st_crs(4326)
distance_poli <- st_distance(centrosageb, poli)

# Escoger la distancia mínima y convertir la distancia de metros a km
distanciamin <- centrosageb %>%
  mutate(distanciaminpoli = apply(distance_poli, 1, function(x) min(x)),
         distanciaminpoli_km = distanciaminpoli/1000) %>% 
  st_drop_geometry()

# Unir la distancia mínima a una estación de policía en km con el resto de las variables
base <- left_join(base, distanciamin[,c("CVEGEO", "distanciaminpoli_km")], 
                  by="CVEGEO")

# 1.2) STV (cámaras) con botones de pánico

# Cargar la base ubicacion-acceso-gratuito-internet-wifi-C5
wifi_sf <- st_read("wifi-c5/ubicacion-acceso-gratuito-internet-wifi-c5.shp")
st_crs(wifi_sf)

# Hacer la intersección entre las AGEB y los postes para saber en qué AGEB está cada poste
intwifi <- st_intersection(agebs_sf, wifi_sf)

# Obtener el número de STV con boton de pánico, sin botón de pánico y el total por AGEB
intwifi  <- intwifi %>%  
  mutate(conboton = ifelse(boton== "CON BOTON", 1, 0),
         sinboton = ifelse(boton== "SIN BOTON", 1, 0),
         stv = 1) %>% 
  group_by(CVEGEO) %>% 
  mutate(total_conboton = sum(conboton),
         total_sinboton = sum(sinboton),
         total_stv = sum(stv)) %>% 
  filter(row_number()==1) %>% 
  ungroup() %>% 
  st_drop_geometry()

# Unir a la base
base <- left_join(base, intwifi[,c("CVEGEO", "total_conboton", "total_sinboton", "total_stv")], by="CVEGEO")

# Agregar cero a las AGEBS que no cuentan con STV
base <- base %>% 
  mutate_at(vars(total_conboton, total_sinboton, total_stv), list(~replace(., is.na(.), 0)))

# Calcular el número de STV por kilómetro cuadrado en cada AGEB
base <- base %>% 
  mutate_at(vars(total_conboton: total_stv), funs(km2= ./area_agebkm))

# 1.3) Pasos seguros e intersecciones seguras

# Cargar la base de pasos seguros 
pasos_seguros_sf <- st_read("pasos_seguros/pasos-seguros-cdmx.shp")
st_crs(pasos_seguros_sf)

# Cargar la base de intersecciones seguras
intersecciones_seguras_sf <- st_read("intersecciones_seguras/intersecciones-seguras.shp")
st_crs(intersecciones_seguras_sf)

# Ver en dónde se encuentran los pasos y las intersecciones seguras
ggplot()+
  geom_sf(data = agebs_sf)+
  geom_sf(data = pasos_seguros_sf, color = "blue", size= 1) +
  geom_sf(data = intersecciones_seguras_sf, color = "purple", size = 1)

# Encontrar las AGEB que han sido intervenidas con pasos seguros e intersecciones seguras
# Hacer la intersección con las AGEBS
intpasos <- st_intersection(agebs_sf, pasos_seguros_sf)
intintersecciones <- st_intersection(agebs_sf, intersecciones_seguras_sf)

# Identificar a las AGEBS que tienen al menos un punto de paso o intersección segura
intpasos <- intpasos %>% 
  group_by(CVEGEO) %>% 
  summarise(pasos_seguros = 1, .groups = 'drop') %>% 
  st_drop_geometry()

intintersecciones <- intintersecciones %>% 
  group_by(CVEGEO) %>% 
  summarise(intersecciones_seguras = 1, .groups = 'drop') %>% 
  st_drop_geometry()

# Unir la variable a la base y reemplazar los NA con ceros ya que en esas AGEB no hay pasos o intersecciones seguras
base <- left_join(base, intpasos, by="CVEGEO")
base <- left_join(base, intintersecciones, by="CVEGEO")

base <- base %>% 
  mutate_at(vars(pasos_seguros, intersecciones_seguras), list(~replace(., is.na(.), 0)))

base <- base %>% 
  mutate(pasos_seguros = ifelse(pasos_seguros == 0, "No", "Si"),
         intersecciones_seguras = ifelse(intersecciones_seguras == 0, "No", "Si"))

#  1.4) Encontrar las agebs que están dentro de 500 metros alrededor (buffer) de los puntos y las intersecciones seguras

# Reproyectar
st_crs(agebs_sf)
agebs_sf_proj <- st_transform(agebs_sf, "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(pasos_seguros_sf)
pasos_seguros_sf = st_transform(pasos_seguros_sf, "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

st_crs(intersecciones_seguras_sf)
intersecciones_seguras_sf <- st_transform(intersecciones_seguras_sf, "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# Hacer un buffer de 500 m alrededor de los puntos de los pasos y las intersecciones seguras
pasos_buff = st_buffer(pasos_seguros_sf, 500) 
intersecciones_buff = st_buffer(intersecciones_seguras_sf, 500) 

# Hacer la intersección entre los pasos y las intersecciones seguras y las AGEBS
pasos_buff_int <- st_intersection(agebs_sf_proj, pasos_buff)
intersecciones_buff_int <- st_intersection(agebs_sf_proj, intersecciones_buff)

# Identificar las agebs que están intersectadas por el buffer de los pasos e intersecciones seguras
pasos_buff_int <- pasos_buff_int %>% 
  group_by(CVEGEO) %>% 
  summarise(pasos_seguros_buff = 1, .groups = 'drop') %>% 
  st_drop_geometry()

intersecciones_buff_int <- intersecciones_buff_int %>% 
  group_by(CVEGEO) %>% 
  summarise(intersecciones_seguras_buff = 1, .groups = 'drop') %>% 
  st_drop_geometry()

# Unir la variable a la base y reemplazar los NA con ceros 
base <- left_join(base, pasos_buff_int, by="CVEGEO")
base <- left_join(base, intersecciones_buff_int, by="CVEGEO")

base <- base %>% 
  mutate_at(vars(pasos_seguros_buff, intersecciones_seguras_buff), list(~replace(., is.na(.), 0)))

base <- base %>% 
  mutate(pasos_seguros_buff = ifelse(pasos_seguros_buff == 0, "No", "Si"),
         intersecciones_seguras_buff = ifelse(intersecciones_seguras_buff == 0, "No", "Si"))

# 1.5) Lotes baldíos

# Abrir el shape de areas verdes
areasverdes <- st_read("areasverdes/cdmx_areas_verdes_2017.shp")
st_crs(areasverdes)

# Tipos de áreas verdes
unique(areasverdes$subcat_sed)

# Seleccionar los lotes baldíos
table(areasverdes$subcat_sed)

baldios <- areasverdes %>% 
  filter(subcat_sed == "Terrenos baldíos")

# Ver en dónde se encuentran los lotes baldíos
ggplot()+
  geom_sf(data = agebs_sf)+
  geom_sf(data = baldios, fill = "brown", color = "brown", size = 2)

# Encontrar las agebs que cuentan con lotes baldíos
# Hacer la intersección con las agebs
intbaldios <- st_intersection(agebs_sf, baldios)

# Identificar a las agebs que tienen al menos un lote baldío
intbaldios <- intbaldios %>% 
  group_by(CVEGEO) %>% 
  summarise(baldio = 1, .groups = 'drop') %>% 
  st_drop_geometry()

# Unir la variable a la base y reemplazar los NA con ceros 
base <- left_join(base, intbaldios, by="CVEGEO")

base <- base %>% 
  mutate(baldio = ifelse(baldio %in% NA, 0, baldio),
         baldio = ifelse(baldio == 0, "No", "Si"))

# 1.6) Áreas verdes pequeñas

# De la base de areas verdes, seleccionar las areas verdes pequeñas
table(areasverdes$subcat_sed)

areasverdes_pequenas <- areasverdes %>% 
  filter(subcat_sed == "Veg. Arbórea, arbustiva y herbácea de glorietas" |
           subcat_sed == "Jardineras públicas y privadas" | 
           subcat_sed == "Camellones centrales y laterales" |
           subcat_sed == "Vialidades")

# Ver en dónde se encuentran las áreas verdes pequeñas
ggplot()+
  geom_sf(data = agebs_sf)+
  geom_sf(data = areasverdes_pequenas, fill = "darkgreen", color = "darkgreen", size = 1)

# Hacer la interseccion entre las areas verdes y las AGEB
interseccion <- st_intersection(agebs_sf, areasverdes_pequenas)
interseccion$areaverde_pequena <- as.numeric(st_area(interseccion$geometry))

# Calcular el area verde en km2 y como porcentaje del area de la AGEB
tempo <- interseccion %>% 
  group_by(CVEGEO) %>% 
  summarise(areaverde_pequena_km = as.numeric(sum(areaverde_pequena)/1000000), .groups = 'drop') %>% 
  ungroup() %>% 
  st_drop_geometry()

base <- left_join(base, tempo, by="CVEGEO")
base <- base %>% 
  mutate(areaverde_pequena_km = ifelse(areaverde_pequena_km %in% NA, 0, areaverde_pequena_km),
         areaverde_pequena_por = areaverde_pequena_km/area_agebkm*100)

# 1.7) Senderos seguros
senderos_seguros_sf <- st_read("senderos_seguros/Senderos_Seguros_FINAL-line.shp")
st_crs(senderos_seguros_sf)
  
# Ver en dónde se encuentran los senderos seguros
ggplot()+
    geom_sf(data = agebs_sf)+
    geom_sf(data = senderos_seguros_sf, color = "blue", size= 2)
  
# Encontrar las AGEBS que cuentan con senderos seguros
# Hacer la intersección entre los senderos seguros y las AGEB
senderos <- st_intersection(agebs_sf, senderos_seguros_sf)
n_distinct(senderos$CVEGEO)

# Identificar a las AGEBS que son intersectadas por un sendero seguro
senderos <- senderos %>% 
    group_by(CVEGEO) %>% 
    summarise(senderos = 1, .groups = 'drop') %>% 
    st_drop_geometry()
  
# Unir la variable a la base y reemplazar los NA con ceros 
base <- left_join(base, senderos, by="CVEGEO")

base <- base %>% 
  mutate(senderos = ifelse(senderos %in% NA, 0, senderos))

# 1.8) Encontrar las agebs que están dentro de 500 metros alrededor (buffer) de los senderos seguros

# Reproyectar
st_crs(senderos_seguros_sf)
senderos_seguros_sf = st_transform(senderos_seguros_sf, "+proj=utm +zone=17 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  
# Hacer un buffer de 500m alrededor de los senderos seguros
senderos_buff = st_buffer(senderos_seguros_sf, 500)

# Hacer la intersección entre los pasos y las intersecciones seguras y las AGEB
senderos_buff_int <- st_intersection(agebs_sf_proj, senderos_buff)

# Identificar las agebs que están intersectadas por el bufer de los senderos seguros
senderos_buff_int <- senderos_buff_int %>% 
    group_by(CVEGEO) %>% 
    summarise(senderos_seguros_buff = 1, .groups = 'drop') %>% 
    st_drop_geometry()

# Unir el identificador a la base y reemplazar los NA con ceros 
base <- left_join(base, senderos_buff_int, by="CVEGEO")

base <- base %>% 
  mutate(senderos_seguros_buff = ifelse(senderos_seguros_buff %in% NA, 0, senderos_seguros_buff))

base <- base %>% 
  mutate(senderos = ifelse(senderos == 0, "No", "Si"),
         senderos_seguros_buff = ifelse(senderos_seguros_buff == 0, "No", "Si"))

# 1.9) Víctimas hombres en carpetas de investigación

#Abrir la base de victimas
victimas <- read_csv("victimas/victimas-en-carpetas-de-investigacion-pgj.csv",
                     locale = locale(encoding = "UTF-8"))

#Agrupamos los delitos de género que pueden ocurrir en el espacio publico, asi como los robos en el transporte publico y a transeuntes con y sin violencia
victimas <- victimas %>% 
  mutate(delito_agrupado = (delito),
         delito_agrupado = case_when(
           delito_agrupado== "ABUSO SEXUAL" ~"abusosexual",
           delito_agrupado== "VIOLACION" ~ "violacion",
           delito_agrupado== "VIOLACION EQUIPARADA" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA" ~ "violacion",
           delito_agrupado== "VIOLACION EQUIPARADA POR CONOCIDO" ~ "violacion",
           delito_agrupado== "TENTATIVA DE VIOLACION" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA EQUIPARADA POR CONOCIDO" ~ "violacion",
           delito_agrupado== "ESTUPRO" ~ "violacion",
           delito_agrupado== "FEMINICIDIO"~ "feminicidio",
           delito_agrupado== "FEMINICIDIO POR ARMA BLANCA"~ "feminicidio",
           delito_agrupado== "FEMINICIDIO POR DISPARO DE ARMA DE FUEGO" ~"feminicidio",
           delito_agrupado== "FEMINICIDIO POR GOLPES"~"feminicidio",
           delito_agrupado== "TENTATIVA DE FEMINICIDIO" ~ "feminicidio",
           delito_agrupado== "PRIVACION DE LA LIBERTAD PERSONAL (REALIZAR ACTO SEXUAL)" ~ "privacion",
           delito_agrupado== "PRIVACION DE LA LIBERTAD PERSONAL" ~ "privacion",
           delito_agrupado== "TRATA DE PERSONAS" ~ "tratapersonas",
           delito_agrupado== "ACOSO SEXUAL"~ "acososexual",
           delito_agrupado== "ACOSO SEXUAL AGRAVADO EN CONTRA DE MENORES" ~"acososexual", 
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA" ~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METROBUS SIN VIOLENCIA" ~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO SIN VIOLENCIA"~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO SIN VIOLENCIA"~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TROLEBUS SIN VIOLENCIA"~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN LIGERO SIN VIOLENCIA" ~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN SUBURBANO SIN VIOLENCIA"~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN RTP SIN VIOLENCIA" ~ "robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN ECOBUS SIN VIOLENCIA" ~"robopasajero_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN AUTOBUS FORANEO SIN VIOLENCIA"~ "robopasajero_sinviolencia", 
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA" ~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA"~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO CON VIOLENCIA"~"robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA" ~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA" ~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TROLEBUS CON VIOLENCIA" ~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN LIGERO CON VIOLENCIA"~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN SUBURBANO CON VIOLENCIA" ~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN RTP CON VIOLENCIA"~"robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN ECOBUS CON VIOLENCIA"~"robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN AUTOBÚS FORÁNEO CON VIOLENCIA" ~"robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA" ~"robopasajero_conviolencia", 
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA SIN VIOLENCIA" ~"robotranseunte_sinviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE DE CELULAR SIN VIOLENCIA"~ "robotranseunte_sinviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE A BORDO DE TAXI PUBLICO Y PRIVADO SIN VIOLENCIA" ~"robotranseunte_sinviolencia", 
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN NEGOCIO CON VIOLENCIA" ~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE DE CELULAR CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN PARQUES Y MERCADOS CON VIOLENCIA" ~"robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE SALIENDO DEL BANCO CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE SALIENDO DEL CAJERO CON VIOLENCIA" ~"robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN RESTAURANT CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE CONDUCTOR DE TAXI PUBLICO Y PRIVADO CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN HOTEL CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN TERMINAL DE PASAJEROS CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE Y VEHICULO CON VIOLENCIA"~ "robotranseunte_conviolencia"))

# Agrupamos los delitos por nivel de gravedad
victimas <- victimas %>% 
  mutate(gravedad = (delito_agrupado),
         gravedad = case_when(
           gravedad== "abusosexual" ~ "gravedad1",
           gravedad== "violacion" ~ "gravedad1",
           gravedad== "feminicidio"~ "gravedad1",
           gravedad== "privacion"~ "gravedad1",
           gravedad== "tratapersonas"~ "gravedad1",
           gravedad== "acososexual"~ "gravedad2",
           gravedad== "robopasajero_conviolencia"~ "gravedad2",
           gravedad== "robotranseunte_conviolencia"~"gravedad2",
           gravedad== "robopasajero_sinviolencia"~ "gravedad3",
           gravedad== "robotranseunte_sinviolencia"~"gravedad3"))

# Eliminar los delitos para los que no se tiene ubicación y especificar que el sistema de referencia de coordenadas (CRS) sea 4326
victimas <- victimas %>% 
  filter(!is.na(longitud))

victimas <- st_as_sf(victimas, coords = c("longitud", "latitud"), crs = 4326)

# Identificar los delitos que ocurrieron antes del inicio de la cuarentena (23 de marzo 2020)
victimas <- victimas %>% 
  mutate(fechahecho = as.Date(fechahecho, "%d/%m/%Y"),
         antes_pandemia = ifelse(fechahecho < "2020-03-24", 1, 0) )

# QUedarse sólo con los delitos antes de pandemia
victimas <- victimas %>% 
  filter(antes_pandemia == 1)

# Quedarse solo con las victimas hombres para los delitos de gravedad 3 y todos los delitos
victimas_hombres <- victimas %>% 
  filter(sexo== "Masculino",
         !is.na(gravedad))

victimas_hombres_g3 <- victimas %>% 
  filter(sexo== "Masculino",
         gravedad == "gravedad3")

# Hacer la interseccion entre la ubicacion donde ocurrió el delito y las AGEBS
# Todas las severdiades 
interseccion <- st_intersection(victimas_hombres, agebs_sf)

# Severidad 3
interseccion_g3 <- st_intersection(victimas_hombres_g3, agebs_sf)

# Contar cuántas víctimas hombres hay por AGEB
interseccion <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(victimas_hombres = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()

interseccion_g3 <- interseccion_g3 %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(victimas_hombres_g3 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()

# Unir las variables a la base 
base <- left_join(base, interseccion, by="CVEGEO")
base <- left_join(base, interseccion_g3, by="CVEGEO")

base <- base %>% 
  mutate_at(vars(victimas_hombres: victimas_hombres_g3), list(~replace(., is.na(.), 0)))

# Calcular las víctimas en relación a la población total
base <- base %>% 
  mutate(victimas_hombres_pob = victimas_hombres/POBTOT,
         victimas_hombres_g3_pob = victimas_hombres_g3/POBTOT)

# 1.9) Hora y día de ocurrencia de los delitos a mujeres por AGEB

# Crear dia de la semana y hora en que ocurrieron los delitos
victimas <- victimas %>% 
  mutate(fechahecho = as.Date(fechahecho, "%d/%m/%Y"),
         diasemana = weekdays(as.Date(fechahecho)),
         diasemana = factor(diasemana, levels = c("lunes", "martes", "miércoles", 
                                                  "jueves", "viernes", "sábado", 
                                                  "domingo")),
         hora = as.POSIXlt(horahecho)$hour)

# Crear categorías
victimas <- victimas %>% 
  mutate(hora_grupos = ifelse(hora >=5 & hora <= 11, "5a11",
                              ifelse(hora >=12 & hora <= 16, "12a16", 
                                     ifelse(hora>=17 & hora <=23, "17a23", "24a4"))),
         hora_grupos = factor(hora_grupos, levels = c("5a11", "12a16", "17a23", "24a4")),
         diasemana_grupos = ifelse(diasemana == "sábado", "findesemana", 
                                   ifelse(diasemana == "domingo", "findesemana", 
                                          "entresemana")),
         diasemana_grupos = factor(diasemana_grupos, levels = c("entresemana", 
                                                                "findesemana")))

# Hacer intersección entre delitos gravedad 3 y AGEBS
g3 <- victimas %>% 
  filter(sexo== "Femenino",
         gravedad == "gravedad3")

interseccion <- st_intersection(g3, agebs_sf)

# Calcular la hora media por AGEB para los delitos de gravedad 3
interseccion <- interseccion %>% 
  group_by(CVEGEO) %>% 
  mutate(g3_horamedia = mean(hora)) %>% 
  ungroup()

# Calcular el porcentaje de delitos de gravedad 3 ocurridos según categorías de horas 
interseccion <- interseccion %>% 
  mutate(g3_5a11 = ifelse(hora_grupos== "5a11", 1, 0),
         g3_12a16 = ifelse(hora_grupos == "12a16", 1, 0),
         g3_17a23 = ifelse(hora_grupos == "17a23", 1, 0),
         g3_24a4 = ifelse(hora_grupos == "24a4", 1, 0)) %>% 
  group_by(CVEGEO) %>% 
  mutate(delitos_porageb = n(),
         g3_5a11_por = sum(g3_5a11)/delitos_porageb,
         g3_12a16_por = sum(g3_12a16)/delitos_porageb,
         g3_17a23_por = sum(g3_17a23)/delitos_porageb,
         g3_24a4_por = sum(g3_24a4)/delitos_porageb) 

# Calcular el porcentaje de delitos de gravedad 3 ocurridos entre semana y en fin de semana
interseccion <- interseccion %>% 
  mutate(g3_entresem = ifelse(diasemana_grupos== "entresemana", 1, 0),
         g3_finde = ifelse(diasemana_grupos == "findesemana", 1, 0)) %>% 
  group_by(CVEGEO) %>% 
  mutate(g3_entresem_por = sum(g3_entresem)/delitos_porageb,
         g3_finde_por = sum(g3_finde)/delitos_porageb)
           
interseccion <- interseccion %>% 
  filter(row_number()==1)%>% 
  st_drop_geometry()

base <- left_join(base, interseccion[,c("CVEGEO", "g3_horamedia", "g3_5a11_por",
                                        "g3_12a16_por", "g3_17a23_por", 
                                        "g3_24a4_por", "g3_entresem_por", 
                                        "g3_finde_por")], by="CVEGEO") 

# Hacer intersección entre delitos de todos los niveles de gravedad y las AGEBS
allcrimes <- victimas %>% 
  filter(sexo== "Femenino", 
         !is.na(gravedad))

interseccion <- st_intersection(allcrimes, agebs_sf)

# Calcular la hora media por AGEB para delitos de todos los niveles de gravedad 
interseccion <- interseccion %>% 
  group_by(CVEGEO) %>% 
  mutate(allcrimes_horamedia = mean(hora)) %>% 
  ungroup()

# Calcular el porcentaje de delitos de gravedad 3 ocurridos según categorías de horas 
interseccion <- interseccion %>% 
  mutate(allcrimes_5a11 = ifelse(hora_grupos== "5a11", 1, 0),
         allcrimes_12a16 = ifelse(hora_grupos == "12a16", 1, 0),
         allcrimes_17a23 = ifelse(hora_grupos == "17a23", 1, 0),
         allcrimes_24a4 = ifelse(hora_grupos == "24a4", 1, 0)) %>% 
  group_by(CVEGEO) %>% 
  mutate(allcrimes_porageb = n(),
         allcrimes_5a11_por = sum(allcrimes_5a11)/allcrimes_porageb,
         allcrimes_12a16_por = sum(allcrimes_12a16)/allcrimes_porageb,
         allcrimes_17a23_por = sum(allcrimes_17a23)/allcrimes_porageb,
         allcrimes_24a4_por = sum(allcrimes_24a4)/allcrimes_porageb) 

# Calcular el porcentaje de delitos de gravedad 3 ocurridos entre semana y en fin de semana
interseccion <- interseccion %>% 
  mutate(allcrimes_entresem = ifelse(diasemana_grupos== "entresemana", 1, 0),
         allcrimes_finde = ifelse(diasemana_grupos == "findesemana", 1, 0)) %>% 
  group_by(CVEGEO) %>% 
  mutate(allcrimes_entresem_por = sum(allcrimes_entresem)/allcrimes_porageb,
         allcrimes_finde_por = sum(allcrimes_finde)/allcrimes_porageb)

interseccion <- interseccion %>% 
  filter(row_number()==1) %>% 
  st_drop_geometry()

base <- left_join(base, interseccion[,c("CVEGEO", "allcrimes_horamedia", "allcrimes_5a11_por",
                                        "allcrimes_12a16_por", "allcrimes_17a23_por", 
                                        "allcrimes_24a4_por", "allcrimes_entresem_por", 
                                        "allcrimes_finde_por")], by="CVEGEO") 

# 1.10) Víctimas mujeres en carpetas de investigación con otra categorización de los delitos

#Abrir la base de victimas

#Agrupamos los delitos de genero que pueden ocurrir en el espacio publico, asi como los robos en el transporte publico y a transeuntes con y sin violencia
victimas_mujeres <- victimas %>% 
  mutate(delito_agrupado = (delito),
         delito_agrupado = case_when(
           delito_agrupado== "VIOLENCIA FAMILIAR" ~"violencia_familiar",
           delito_agrupado== "ABUSO SEXUAL" ~"abusosexual",
           delito_agrupado== "VIOLACION" ~ "violacion",
           delito_agrupado== "VIOLACION EQUIPARADA" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA EQUIPARADA" ~ "violacion",
           delito_agrupado== "VIOLACION EQUIPARADA POR CONOCIDO" ~ "violacion",
           delito_agrupado== "TENTATIVA DE VIOLACION" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA EQUIPARADA POR CONOCIDO" ~ "violacion",
           delito_agrupado== "ESTUPRO" ~ "abusosexual", 
           delito_agrupado== "FEMINICIDIO"~ "feminicidio",
           delito_agrupado== "FEMINICIDIO POR ARMA BLANCA"~ "feminicidio",
           delito_agrupado== "FEMINICIDIO POR DISPARO DE ARMA DE FUEGO" ~"feminicidio",
           delito_agrupado== "FEMINICIDIO POR GOLPES"~"feminicidio",
           delito_agrupado== "TENTATIVA DE FEMINICIDIO" ~ "feminicidio",
           delito_agrupado== "PRIVACION DE LA LIBERTAD PERSONAL (REALIZAR ACTO SEXUAL)" ~ "privacion",
           delito_agrupado== "PRIVACION DE LA LIBERTAD PERSONAL" ~ "privacion",
           delito_agrupado== "TRATA DE PERSONAS" ~ "tratapersonas",
           delito_agrupado== "ACOSO SEXUAL"~ "acososexual",
           delito_agrupado== "ACOSO SEXUAL AGRAVADO EN CONTRA DE MENORES" ~"acososexual", 
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METRO SIN VIOLENCIA" ~ "robopasajero_sit_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METROBUS SIN VIOLENCIA" ~ "robopasajero_sit_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO SIN VIOLENCIA"~ "robopasajero_microbus_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO SIN VIOLENCIA"~ "robopasajero_transporte_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TROLEBUS SIN VIOLENCIA"~ "robopasajero_sit_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN LIGERO SIN VIOLENCIA" ~ "borrar",
           delito_agrupado== "ROBO A PASAJERO EN TREN SUBURBANO SIN VIOLENCIA"~ "borrar",
           delito_agrupado== "ROBO A PASAJERO EN RTP SIN VIOLENCIA" ~ "robopasajero_sit_inviolencia",
           delito_agrupado== "ROBO A PASAJERO EN ECOBUS SIN VIOLENCIA" ~"robopasajero_sit_sinviolencia",
           delito_agrupado== "ROBO A PASAJERO EN AUTOBUS FORANEO SIN VIOLENCIA"~ "robopasajero_sinviolencia", 
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METRO CON VIOLENCIA" ~ "robopasajero_sit_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE METROBUS CON VIOLENCIA"~ "robopasajero_sit_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO COLECTIVO CON VIOLENCIA"~"robopasajero_microbus_conviolencia",
           delito_agrupado== "ROBO A PASAJERO A BORDO DE TRANSPORTE PÚBLICO CON VIOLENCIA" ~ "robopasajero_transporte_conviolencia", 
           delito_agrupado== "ROBO A PASAJERO A BORDO DE PESERO Y VEHICULO CON VIOLENCIA" ~ "borrar",
           delito_agrupado== "ROBO A PASAJERO EN TROLEBUS CON VIOLENCIA" ~ "robopasajero_sit_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN TREN LIGERO CON VIOLENCIA"~ "borrar",
           delito_agrupado== "ROBO A PASAJERO EN TREN SUBURBANO CON VIOLENCIA" ~ "borrar", 
           delito_agrupado== "ROBO A PASAJERO EN RTP CON VIOLENCIA"~"robopasajero_sit_conviolencia",  
           delito_agrupado== "ROBO A PASAJERO EN ECOBUS CON VIOLENCIA"~"robopasajero_sit_conviolencia",
           delito_agrupado== "ROBO A PASAJERO EN AUTOBÚS FORÁNEO CON VIOLENCIA" ~"robopasajero_conviolencia",
           delito_agrupado== "ROBO A PASAJERO / CONDUCTOR DE VEHICULO CON VIOLENCIA" ~"robopasajero_conviolencia", 
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA SIN VIOLENCIA" ~"robotranseunte_sinviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE DE CELULAR SIN VIOLENCIA"~ "robotranseunte_sinviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE A BORDO DE TAXI PUBLICO Y PRIVADO SIN VIOLENCIA" ~"robopasajero_sinviolencia", 
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN NEGOCIO CON VIOLENCIA" ~ "robotranseunte_negocio_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE DE CELULAR CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN PARQUES Y MERCADOS CON VIOLENCIA" ~"robotranseunte_parques_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE A BORDO DE TAXI PÚBLICO Y PRIVADO CON VIOLENCIA"~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE SALIENDO DEL BANCO CON VIOLENCIA"~ "robotranseunte_banco_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE SALIENDO DEL CAJERO CON VIOLENCIA" ~"borrar",
           delito_agrupado== "ROBO A TRANSEUNTE EN RESTAURANT CON VIOLENCIA"~ "robotranseunte_restaurant_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE CONDUCTOR DE TAXI PUBLICO Y PRIVADO CON VIOLENCIA"~ "robopasajero_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE EN HOTEL CON VIOLENCIA"~ "borrar",
           delito_agrupado== "ROBO A TRANSEUNTE EN TERMINAL DE PASAJEROS CON VIOLENCIA"~ "borrar",
           delito_agrupado== "ROBO A TRANSEUNTE EN VIA PUBLICA (NOMINA) CON VIOLENCIA"~ "robotranseunte_conviolencia",
           delito_agrupado== "ROBO A TRANSEUNTE Y VEHICULO CON VIOLENCIA"~ "borrar")) 

#Quedarse solo con las victimas mujeres por delitos agrupados, que sufrieron delitos antes de que inició la cuarentena y con datos sobre la ubicacion del delito
victimas_mujeres <- victimas_mujeres %>% 
  filter(sexo== "Femenino",
          !is.na(delito_agrupado))

#Hacer la interseccion entre la ubicacion del delito y las AGEBS
victimas_mujeres <- st_as_sf(victimas_mujeres, coords = c("longitud", "latitud"), crs = 4326)
interseccion <- st_intersection(victimas_mujeres, agebs_sf)

#Calcular el numero de victimas de delitos según tipo de gravedad por AGEB
tempo <- interseccion %>% 
  filter(delito_agrupado == "violencia_familiar") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(violencia_familiar = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "abusosexual") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(abusosexual = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "violacion") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(violacion = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "feminicidio") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(feminicidio = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "privacion") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(privacion = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "tratapersonas") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(tratapersonas = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "acososexual") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(acososexual = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_negocio_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_negocio_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_parques_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_parques_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_restaurant_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_restaurant_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_banco_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_banco_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_sit_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_sit_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_microbus_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_microbus_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_transporte_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_transporte_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_microbus_sinviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_microbus_sinviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_transporte_sinviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_transporte_sinviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_conviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_conviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_sinviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_sinviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robopasajero_sit_sinviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robopasajero_sit_sinviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(delito_agrupado == "robotranseunte_sinviolencia") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(robotranseunte_sinviolencia = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

base <- base %>% 
  mutate_at(vars("violencia_familiar":"robotranseunte_sinviolencia"), list(~replace(., is.na(.), 0))) 


# Agregar los PDS a la base 
# Cargar las bases de datos creada en el script pds_summary

# PDS severidad 1
pds_s1 <- read_csv("pds_s1.csv")
base <- left_join(base, pds_s1[,c("CVEGEO", "result_var")], by="CVEGEO") 

base <- base %>% 
  mutate(result_var = ifelse(result_var %in% NA, "Non PDs", "PDs")) %>% 
  rename(pd_s1 = result_var) %>% 
  mutate(pd_s1 = factor(pd_s1, levels = c("Non PDs", "PDs")))
  

# PDS severidad 2
pds_s2 <- read_csv("pds_s2.csv")
pds_s2 <- pds_s2 %>% 
  group_by(CVEGEO) %>% 
  filter(row_number()==1)

base <- left_join(base, pds_s2[,c("CVEGEO", "result_var")], by="CVEGEO") 

base <- base %>% 
  mutate(result_var = ifelse(result_var %in% NA, "Non PDs", "PDs")) %>% 
  rename(pd_s2 = result_var) %>% 
  mutate(pd_s2 = factor(pd_s2, levels = c("Non PDs", "PDs")))

# PDS severidad 3
pds_s3 <- read_csv("pds_s3.csv")
pds_s3 <- pds_s3 %>% 
  group_by(CVEGEO) %>% 
  filter(row_number()==1)

base <- left_join(base, pds_s3[,c("CVEGEO", "result_var")], by="CVEGEO") 

base <- base %>% 
  mutate(result_var = ifelse(result_var %in% NA, "Non PDs", "PDs")) %>% 
  rename(pd_s3 = result_var) %>% 
  mutate(pd_s3 = factor(pd_s3, levels = c("Non PDs", "PDs")))

# PDS todos los niveles de severidad
pds_allcrimes <- read_csv("pds_allcrimes.csv")
pds_allcrimes <- pds_allcrimes %>% 
  group_by(CVEGEO) %>% 
  filter(row_number()==1)

base <- left_join(base, pds_allcrimes[,c("CVEGEO", "result_var")], by="CVEGEO") 

base <- base %>% 
  mutate(result_var = ifelse(result_var %in% NA, "Non PDs", "PDs")) %>% 
  rename(pd_allcrimes = result_var) %>% 
  mutate(pd_allcrimes = factor(pd_allcrimes, levels = c("Non PDs", "PDs")))

# Eliminar las variables que no vamos a utilizar
base <- base %>% 
  dplyr::select (-areaverde_km, -comercio, -serv_financieros, -serv_educativos, -serv_salud, 
          -serv_entretenimiento, -serv_preparacionalimentos, -serv_personales, -viasprimarias,
          -viassecundarias, -paradastrolebus, -paradasrtp, -paradassitis, -total_conboton,
          -total_sinboton, -total_stv, -areaverde_pequena_km)

# Guardar la base
write.csv(base, "base_pdunderstanding.csv", row.names = F)