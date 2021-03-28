rm(list=ls())

#Paquetes
library(readr)
library(rgdal)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)

####Construir las variables sobre infraestructura de las AGEB####
#Abrir la base que creamos en el script cluster
base <- read_csv("Espacios publicos/PD identification/Output/bases/basecluster.csv")

#Areas verdes 
#Abrir el shape de areas verdes
areasverdes <- st_read("Espacios publicos/Índice/inputs/adip/areas_verdes/shp/cdmx_areas_verdes_2017.shp")
st_crs(areasverdes)

#Abrir el marco geoestadistico de la CDMX a nivel ageb
agebs_sf <- st_read("Espacios publicos/Índice/inputs/inegi/marcogeo2020/cdmx/conjunto_de_datos/09a.shp")
agebs_sf = st_transform(agebs_sf, 4326)

#Seleccionar los tipos de area verde que nos interesan
areasverdes <- areasverdes %>% 
  filter(subcat_sed %in% c("Parques", "Jardines públicos", "Arboledas","ANP",
                           "Plazas", "AVA", "Alamedas"))

#Hacer la interseccion entre las areas verdes y las AGEB
interseccion <- st_intersection(agebs_sf, areasverdes)
interseccion$areaverde <- as.numeric(st_area(interseccion$geometry))

#Calcular el area verde en km2 y como porcentaje del area de la AGEB
tempo <- interseccion %>% 
  group_by(CVEGEO) %>% 
  summarise(areaverde_km = as.numeric(sum(areaverde)/1000000), .groups = 'drop') %>% 
  ungroup() %>% 
  st_drop_geometry()

base <- left_join(base, tempo, by="CVEGEO")
base <- base %>% 
  mutate(areaverde_km = ifelse(areaverde_km %in% NA, 0, areaverde_km),
         areaverde_por = areaverde_km/area_agebkm2*100)

#Servicos y comercio
#Abrir el shape del DENUE
denue <- st_read("Espacios publicos/Índice/inputs/inegi/denue/shp/conjunto_de_datos/denue_inegi_09_.shp",
                 options = "ENCODING=LATIN1")
denue  <-  st_transform(denue, 4326)

#Hacer la interseccion entre las unidades economicas y las AGEB
interseccion <- st_intersection(agebs_sf, denue)

#Calcular el numero de unidades de comercio por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^431|^461|^462|^463|^464|^465|^466")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(comercio = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de servicios financieros por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^522")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_financieros = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de servicios educativos por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^611")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_educativos = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de servicios de salud por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^622")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_salud = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de servicios de entretenimiento por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^712|^713")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_entretenimiento = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de preparacion de alimentos y bebidas por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^722")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_preparacionalimentos = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de unidades de servicios personales por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^812")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_personales = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

base <- base %>% 
  dplyr::mutate_at(vars(comercio:serv_personales), list(~replace(., is.na(.), 0)))

#Calcular el numero de las unidades economicas por km2
base <- base %>% 
  mutate_at(vars(comercio: serv_personales), funs(km2= ./area_agebkm2))

#Vialidades
#Abrir la base de ejes viales 
ejesviales <- st_read("Espacios publicos/Índice/inputs/inegi/marcogeo2020/cdmx/conjunto_de_datos/09e.shp",
                      options = "ENCODING=LATIN1")
ejesviales <-  st_transform(ejesviales, 4326)

#Agrupar los tipos de vialidades en primarias y secundarias
ejesviales <- ejesviales %>% 
  mutate(vias = (TIPOVIAL),
         vias = case_when(
           vias == "Avenida"~ "sec",
           vias == "Calle" ~ "sec",
           vias == "Calzada"~ "fast",
           vias == "Prolongación"~ "sec", 
           vias == "Eje Vial"~ "fast",
           vias == "Andador"~ "sec",
           vias == "Cerrada" ~ "sec",
           vias == "Callejón"~ "sec",
           vias == "Corredor" ~ "sec",
           vias == "Circuito"~ "fast",
           vias == "Privada"~ "sec",
           vias == "Peatonal"~ "sec",
           vias == "Continuación"~"sec",
           vias == "Retorno"~ "sec",
           vias == "Periférico"~ "fast",
           vias == "Pasaje"~"sec", 
           vias == "Carretera"~"fast",
           vias == "Boulevard"~ "fast",
           vias == "Autopista"~ "fast",
           vias == "Diagonal"~ "sec", 
           vias == "Ampliación" ~ "sec",
           vias == "Viaducto" ~ "fast",
         ))

interseccion <- st_intersection(agebs_sf, ejesviales)
interseccion$longitud <-  as.numeric(st_length(interseccion))

#Calcular la longitud de los ejes viales en km por AGEB
tempo <- interseccion %>% 
  group_by(CVEGEO, vias) %>% 
  summarise(longitudvial = sum(longitud)/1000, .groups = 'drop') %>% 
  ungroup() %>% 
  st_drop_geometry()

tempo <- tempo %>% 
  spread(vias, longitudvial) %>% 
  mutate(fast = ifelse(fast %in% NA, 0, fast),
         sec = ifelse(sec %in% NA, 0, sec)) %>% 
  rename("viasprimarias" = "fast", "viassecundarias" = "sec")
base <- left_join(base, tempo, by="CVEGEO")

base <- base %>% 
  dplyr::mutate_at(vars(viasprimarias:viassecundarias), funs(replace(., is.na(.), 0)))

#Calcular la longitud de ejes viales por km2
base <- base %>% 
  mutate_at(vars(viasprimarias: viassecundarias), funs(km2= ./area_agebkm2))

#Distancia a la estacion de metro y metrobus mas cercana
#Abrir el shape de las estaciones de metro y metrobus
metro_sf <- st_read("Espacios publicos/Índice/inputs/adip/metro/shp/estaciones-metro.shp")
metrobus_sf <- st_read("Espacios publicos/Índice/inputs/adip/metrobus/shp/estaciones-metrobus.shp")
metro_sf <- st_transform(metro_sf, 4326)
metrobus_sf <- st_transform(metrobus_sf, 4326)

#Unir ambas bases
metrobus_sf <- metrobus_sf %>% 
  mutate(agency_id = "metrobus") %>% 
  dplyr::select("agency_id")
publictrans <- metro_sf %>% 
  dplyr::select("agency_id") %>% 
  rbind(metrobus_sf)

#Calcular el centroide de la AGEB
centrosageb <- st_centroid(agebs_sf)

#Calcular la distancia del centroide de la AGEB a las estaciones de metro y metrobus, y escoger la distancia minima
d_publictrans <- st_distance(centrosageb, publictrans)

tempo <- centrosageb %>%
  mutate(distanciamintransporte = apply(d_publictrans, 1, function(x) min(x)),
         distanciamintransporte_km = distanciamintransporte/1000) %>% 
  st_drop_geometry()

base <- left_join(base, tempo[,c("CVEGEO", "distanciamintransporte_km")], 
                     by="CVEGEO")

#Paradas de otros transportes publicos
#Abrir las bases de las paradas de trolebus, rtp y sitis
trolebus <- read_csv("Espacios publicos/Índice/inputs/adip/paradas de RTP y trolebus/paradas-de-trolebus.csv")
rtp <- read_csv("Espacios publicos/Índice/inputs/adip/paradas de RTP y trolebus/paradas-de-rtp.csv")
sitis <- read_csv("Espacios publicos/Índice/inputs/adip/sitis/puntos-de-arribo-sitis.csv")

trolebus <- st_as_sf(trolebus, coords = c("stop_lon", "stop_lat"), crs = 4326)
rtp <- st_as_sf(rtp, coords = c("stop_lon", "stop_lat"), crs = 4326)
sitis <- st_as_sf(sitis, coords = c("x", "y"), crs= 4326)

#Calcular el numero de paradas de trolebus por AGEB
interseccion <- st_intersection(agebs_sf, trolebus)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradastrolebus = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

#Calcular el numero de paradas de rtp por AGEB
interseccion <- st_intersection(agebs_sf, rtp)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradasrtp = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

#Calcular el numero de los puntos de arribo para los sistemas de transporte individual sustentable (sitis) por AGEB
interseccion <- st_intersection(agebs_sf, sitis)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradassitis = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

#Calcular el numero de paradas por km2
base <- base %>% 
  mutate(paradastrolebus = ifelse(paradastrolebus %in% NA, 0, paradastrolebus),
         paradastrolebus_km2 = paradastrolebus/area_agebkm2,
         paradasrtp = ifelse(paradasrtp %in% NA, 0, paradasrtp),
         paradasrtp_km2 = paradasrtp/area_agebkm2,
         paradassitis = ifelse(paradassitis %in% NA, 0, paradassitis),
         paradassitis_km2 = paradassitis/area_agebkm2)

####Victimas en carpetas de investigacion####
#Abrir la base de victimas
victimas <- read_csv("Espacios publicos/PD identification/Input/victimas/victimas-en-carpetas-de-investigacion-pgj.csv",
                     locale = locale(encoding = "UTF-8"))

#Agrupamos los delitos de genero que pueden ocurrir en el espacio publico, asi como los robos en el transporte publico y a transeuntes con y sin violencia
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

#Agrupamos los delitos por nivel de gravedad
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

#Quedarse solo con las victimas mujeres por delitos de algun nivel de gravedad y en las que se tiene la ubicacion del delito
victimas <- victimas %>% 
  filter(sexo== "Femenino",
         !is.na(gravedad),
         !is.na(longitud))          

#Hacer la interseccion entre la ubicacion del delito y las AGEBS
victimas <- st_as_sf(victimas, coords = c("longitud", "latitud"), crs = 4326)
interseccion <- st_intersection(victimas, agebs_sf)

#Calcular el numero de victimas de delitos gravedad 1 por año
tempo <- interseccion %>% 
  filter(gravedad == "gravedad1") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad1 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad1" & ano_inicio == 2019) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad1_2019 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad1" & ano_inicio == 2020) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad1_2020 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de victimas de delitos gravedad 2 por año
tempo <- interseccion %>% 
  filter(gravedad == "gravedad2") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad2 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad2" & ano_inicio == 2019) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad2_2019 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad2" & ano_inicio == 2020) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad2_2020 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular el numero de victimas de delitos gravedad 3 por año
tempo <- interseccion %>% 
  filter(gravedad == "gravedad3") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad3 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad3" & ano_inicio == 2019) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad3_2019 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

tempo <- interseccion %>% 
  filter(gravedad == "gravedad3" & ano_inicio == 2020) %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(gravedad3_2020 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

#Calcular la tasa de de victimas por AGEB
base <- base %>% 
  mutate_at(vars(gravedad1:gravedad3_2020), funs(replace(., is.na(.), 0))) %>% 
  mutate_at(vars(gravedad1:gravedad3_2020), funs(rate= ./POBTOT*1000))

#Calcular el numero de victimas de todos los niveles de gravedad por AGEB
base <- base %>% 
  mutate(allcrimes19 = gravedad1_2019 + gravedad2_2019 + gravedad3_2019,
         allcrimes20 = gravedad1_2020 + gravedad2_2020 + gravedad3_2020)

#Calcular el logaritmo del numero de victimas por AGEB
base <- base %>% 
  mutate_at(vars(gravedad1: allcrimes20), funs(log= log(.+1)))

#Guardar la base
write.csv(base, "Espacios publicos/PD identification/Output/bases/baseageb.csv", row.names = F)

