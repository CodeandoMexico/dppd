rm(list=ls())

# Paquetes
library(readr)
library(rgdal)
library(dplyr)
library(tidyverse)
library(sf)
library(ggplot2)
library(BAMMtools)
library(leaflet)

#### Construir las variables para la identificación de desviaciones positivas ####

# Abrir la base que creamos en el script clusters
base <- read_csv("basecluster.csv")

# Abrir el marco geoestadistico 2020 de la CDMX a nivel AGEB
# Abrir con readOGR
agebs <- readOGR("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp")
proj4string(agebs)

# Reproyectar con spTransform
agebs <- sp::spTransform(agebs, CRS("+proj=longlat +datum=WGS84 +no_defs"))
proj4string(agebs)

# Convertir a sf y especificar que el sistema de referencia de coordenadas (CRS) sea 4326
agebs <- st_as_sf(agebs)
st_crs(agebs)

st_crs(agebs) = st_crs(4326)
st_crs(agebs)

#### 1) Áreas verdes ####

# Abrir el shape de la base llamada Areas verdes
areasverdes <- st_read("areasverdes/cdmx_areas_verdes_2017.shp")
st_crs(areasverdes)

# Seleccionar los tipos de áreas verdes grandes o no movibles 
areasverdes <- areasverdes %>% 
  filter(subcat_sed %in% c("Parques", "Jardines públicos", "Arboledas","ANP",
                           "Plazas", "AVA", "Alamedas"))

# Hacer la interseccion entre las áreas verdes y las AGEBS
interseccion <- st_intersection(agebs, areasverdes)

# Calcular la superficie en metros cuadrados de las áreas verdes 
interseccion$areaverde <- as.numeric(st_area(interseccion$geometry))

# Transformar la superficie a kilómetros cuadrados y calcular el porcentaje que ocupa en la AGEB
tempo <- interseccion %>% 
  group_by(CVEGEO) %>% 
  summarise(areaverde_km = as.numeric(sum(areaverde)/1000000), .groups = 'drop') %>% 
  ungroup() %>% 
  st_drop_geometry()

# Guardar en la base y poner ceros en las AGEBS que no tienen áreas verdes grandes o no movibles
base <- left_join(base, tempo, by="CVEGEO")
base <- base %>% 
  mutate(areaverde_km = ifelse(areaverde_km %in% NA, 0, areaverde_km),
         areaverde_por = areaverde_km/area_agebkm*100)

#### 2) Unidades económicas ####

# Abrir el shape del DENUE
denue <- st_read("DENUE/denue_inegi_09_.shp", options = "ENCODING=LATIN1")
st_crs(denue)
denue  <-  st_transform(denue, 4326)

# Hacer la intersección entre las unidades económicas y las AGEBS
interseccion <- st_intersection(agebs, denue)

# Calcular el número de unidades comerciales por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^431|^461|^462|^463|^464|^465|^466")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(comercio = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de servicios financieros por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^522")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_financieros = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de servicios educativos por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^611")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_educativos = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de servicios de salud por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^622")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_salud = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de servicios de entretenimiento por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^712|^713")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_entretenimiento = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de preparación de alimentos y bebidas por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^722")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_preparacionalimentos = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el número de unidades de servicios personales por AGEB
tempo <- interseccion %>% 
  filter(str_detect(codigo_act, "^812")) %>% 
  mutate(total = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(serv_personales = sum(total), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

base <- base %>% 
  dplyr::mutate_at(vars(comercio:serv_personales), list(~replace(., is.na(.), 0)))

# Calcular el número total de las unidades económicas por km2
base <- base %>% 
  mutate_at(vars(comercio: serv_personales), funs(km2= ./area_agebkm))

#### 3) Tipos de vialidades  ####

# Abrir el marco geoestadístico 2020 de la CDMX a nivel AGEB
agebs_sinreproj <- st_read("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp")
st_crs(agebs_sinreproj)

ejesviales <- st_read("marco_geoestadistico/cdmx/conjunto_de_datos/09e.shp",
                      options = "ENCODING=LATIN1")
st_crs(ejesviales)

# Agrupar los tipos de vialidades en primarias y secundarias
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

interseccion <- st_intersection(agebs_sinreproj, ejesviales)
interseccion$longitud <-  as.numeric(st_length(interseccion))

# Calcular la longitud de los ejes viales en kilómetros por AGEB
tempo <- interseccion %>% 
  group_by(CVEGEO, vias) %>% 
  summarise(longitudvial = sum(longitud)/1000, .groups = 'drop') %>% 
  ungroup() %>% 
  st_drop_geometry()

# Identificar las AGEBS que no tienen vialidades
tempo <- tempo %>% 
  spread(vias, longitudvial) %>% 
  mutate(fast = ifelse(fast %in% NA, 0, fast),
         sec = ifelse(sec %in% NA, 0, sec)) %>% 
  rename("viasprimarias" = "fast", "viassecundarias" = "sec")
base <- left_join(base, tempo, by="CVEGEO")

base <- base %>% 
  dplyr::mutate_at(vars(viasprimarias:viassecundarias), funs(replace(., is.na(.), 0)))

# Calcular la densidad vial (longitud de ejes viales por kilómetro cuadrado)
base <- base %>% 
  mutate_at(vars(viasprimarias: viassecundarias), funs(km2= ./area_agebkm))

#### 4) Distancia a la estación de metro y metrobús más cercana ####

# Abrir el shape de las estaciones de metro y especificar que el CRS sea 4326
metro_sf <- st_read("metro_metrobus/metro/estaciones-metro.shp")
metro_sf <- st_transform(metro_sf, 4326)

# Abrir el shape de las estaciones de metrobús y especificar que el CRS sea 4326
metrobus_sf <- st_read("metro_metrobus/metrobus/estaciones-metro.shp")
metrobus_sf <- st_transform(metrobus_sf, 4326)

# Unir ambas bases
metrobus_sf <- metrobus_sf %>% 
  mutate(agency_id = "metrobus") %>% 
  dplyr::select("agency_id")
publictrans <- metro_sf %>% 
  dplyr::select("agency_id") %>% 
  rbind(metrobus_sf)

# Calcular el centroide de las AGEBS
centrosageb <- st_centroid(agebs)

# Calcular la distancia del centroide de la AGEB a las estaciones de metro y metrobús, y escoger la distancia mínima
d_publictrans <- st_distance(centrosageb, publictrans)

tempo <- centrosageb %>%
  mutate(distanciamintransporte = apply(d_publictrans, 1, function(x) min(x)),
         distanciamintransporte_km = distanciamintransporte/1000) %>% 
  st_drop_geometry()

# Guardar en la base
base <- left_join(base, tempo[,c("CVEGEO", "distanciamintransporte_km")], 
                     by="CVEGEO")

#### 5) Paradas de otros tipos de transportes públicos (trolebús, RTP y SITIS) ####

# Abrir las bases de las paradas de trolebús, RTP y SITIS
trolebus <- read_csv("trolebus_rtp_sitis/paradas-de-trolebus.csv")
rtp <- read_csv("trolebus_rtp_sitis/paradas-de-rtp.csv")
sitis <- read_csv("trolebus_rtp_sitis/puntos-de-arribo-sitis.csv")

# Especificar las coordenadas
trolebus <- st_as_sf(trolebus, coords = c("stop_lon", "stop_lat"), crs = 4326)
rtp <- st_as_sf(rtp, coords = c("stop_lon", "stop_lat"), crs = 4326)
sitis <- st_as_sf(sitis, coords = c("x", "y"), crs= 4326)

# Calcular el número de paradas de trolebús por AGEB
interseccion <- st_intersection(agebs, trolebus)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradastrolebus = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

# Calcular el número de paradas de RTP por AGEB
interseccion <- st_intersection(agebs, rtp)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradasrtp = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

# Calcular el número de los puntos de arribo para los sistemas de transporte individual sustentable (SITIS) por AGEB
interseccion <- st_intersection(agebs, sitis)
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(paradassitis = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by= "CVEGEO")

# Calcular el numero de paradas por kilómetro cuadrado
base <- base %>% 
  mutate(paradastrolebus = ifelse(paradastrolebus %in% NA, 0, paradastrolebus),
         paradastrolebus_km2 = paradastrolebus/area_agebkm,
         paradasrtp = ifelse(paradasrtp %in% NA, 0, paradasrtp),
         paradasrtp_km2 = paradasrtp/area_agebkm,
         paradassitis = ifelse(paradassitis %in% NA, 0, paradassitis),
         paradassitis_km2 = paradassitis/area_agebkm)

#### 6) Alumbrado público, pavimentación y ambulantaje ####

# Abrir el shape del Inventario Nacional de Viviendas con sf
inv <- st_read("inventario_viviendas/INV_2016/09_Frentes_INV2016.shp")

# Revisar su proyección
st_crs(inv)

# Con base en el identificador de los frentes de manzana, generar el indentificador por AGEB
inv <- inv %>% 
  mutate(CVEGEO_ageb = str_sub(CVEGEO,1, 13))

n_distinct(inv$CVEGEO_ageb)

# Obtener el porcentage de frentes de manzana sin alumbrado público, sin pavimentación y con ambulantaje
inv <- inv %>% 
  mutate(id_frente = 1,
         sinalumbrado = ifelse(ALUMPUB_ == 2, 1, 0),
         sinpavimento = ifelse(RECUCALL_ ==  3, 1, 0),
         ambulantaje = ifelse(PUESAMBU_ == 1, 1, 0)) %>% 
  group_by(CVEGEO_ageb) %>% 
  mutate(frentesmza_total = sum(id_frente),
         sinalumbrado_total = sum(sinalumbrado),
         sinalumbrado_por = sinalumbrado_total*100/frentesmza_total,
         sinpavimento_total = sum(sinpavimento),
         sinpavimento_por = sinpavimento_total*100/frentesmza_total,
         ambulantaje_total = sum(ambulantaje),
         ambulantaje_por = ambulantaje_total*100/frentesmza_total)

# Filtrar para quedarnos unicamente con los valores por AGEB
inv <- inv %>% 
  group_by(CVEGEO_ageb) %>% 
  filter(row_number()==1) %>% 
  st_drop_geometry() %>% 
  as_tibble()

# Guardar en la base 
base <- left_join(base, inv[,c("CVEGEO_ageb", "sinalumbrado_por", "sinpavimento_por", "ambulantaje_por")], by= c("CVEGEO" = "CVEGEO_ageb"))

# Revisar los valores faltantes
sum(is.na(base$sinalumbrado_por))# Hay 4 AGEBS que no tienen valores del inventario, podemos reemplazar los valores faltantes con el valor promedio

variables <- c("sinalumbrado_por", "sinpavimento_por", "ambulantaje_por")
base[,variables] <- lapply(base[,variables], function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

#### 7) Víctimas en carpetas de investigación####

# Abrir la base llamada victimas en carpetas de investigacion pgj
victimas <- read_csv("victimas/victimas-en-carpetas-de-investigacion-pgj.csv",
                     locale = locale(encoding = "UTF-8"))

# Agrupamos los delitos de género que pueden ocurrir en el espacio público, así como los robos en el transporte público y a transeuntes con y sin violencia
victimas <- victimas %>% 
  mutate(delito_agrupado = (delito),
         delito_agrupado = case_when(
           delito_agrupado== "ABUSO SEXUAL" ~"abusosexual",
           delito_agrupado== "VIOLACION" ~ "violacion",
           delito_agrupado== "VIOLACION EQUIPARADA" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA" ~ "violacion",
           delito_agrupado== "VIOLACION TUMULTUARIA EQUIPARADA" ~ "violacion",
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

# Identificar los delitos que ocurrieron antes del inicio de la cuarentena (23 de marzo 2020)
victimas <- victimas %>% 
  mutate(fechahecho = as.Date(fechahecho, "%d/%m/%Y"),
         antes_pandemia = ifelse(fechahecho < "2020-03-24", 1, 0) )

# Quedarse solo con las víctimas mujeres por delitos de algún nivel de gravedad, que sufrieron delitos antes de que inició la cuarentena y con datos sobre la ubicacion del delito
victimas_filtro <- victimas %>% 
  filter(sexo== "Femenino",
         !is.na(gravedad),
         antes_pandemia == 1,
         !is.na(longitud))    

# Especificar las coordenadas en la base de victimas
victimas_filtro <- st_as_sf(victimas_filtro, coords = c("longitud", "latitud"), crs = 4326)

# Hacer la interseccion entre la ubicacion del delito y las AGEBS
interseccion <- st_intersection(victimas_filtro, agebs)

# Calcular el numero de víctimas de delitos de gravedad 1 por AGEB
tempo <- interseccion %>% 
  filter(gravedad == "gravedad1") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(g1 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el numero de víctimas de delitos de gravedad 2 por AGEB
tempo <- interseccion %>% 
  filter(gravedad == "gravedad2") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(g2 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el numero de víctimas de delitos de gravedad 3 por AGEB
tempo <- interseccion %>% 
  filter(gravedad == "gravedad3") %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(g3 = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Calcular el numero de víctimas de delitos de todos los niveles de gravedad por AGEB
tempo <- interseccion %>% 
  mutate(list = 1) %>% 
  group_by(CVEGEO) %>% 
  summarise(allcrimes = sum(list), .groups = 'drop') %>% 
  st_drop_geometry()
base <- left_join(base, tempo, by="CVEGEO")

# Identificar las AGEBS que no registran delitos de víctimas en carpetas de investigación
base <- base %>% 
  mutate_at(vars(g1:allcrimes), list(~replace(., is.na(.), 0))) 

# Transformar las variables dependientes en su logaritmo + 1 para normalizarlas y mantener las observaciones que tienen valor cero
base <- base %>% 
  mutate_at(vars(g1: allcrimes), list(log= ~log(.+1)))

# Guardar la base
write.csv(base, "baseageb.csv", row.names = F)

#### 8) Mapa coropleta del número de víctimas ####

# Para las víctimas de todos los niveles de gravedad

allcrimesjenks <- getJenksBreaks(base$allcrimes, 6)
allcrimesjenks

base <- base %>% 
  mutate(allcrimes_categorica = ifelse(allcrimes >=0 & allcrimes <= 4, "0a4",
                                ifelse(allcrimes >=5 & allcrimes <= 11, "5a11",
                                       ifelse(allcrimes>= 12 & allcrimes<=23, "12a23",
                                              ifelse(allcrimes >=24 & allcrimes<=47, "24a47", "48a136")))))

base$allcrimes_categorica <- factor(base$allcrimes_categorica, ordered= T,
                              levels = c("0a4", "5a11", "12a23", "24a47", "48a136"))

base <- left_join(base, agebs, by="CVEGEO") %>% 
  st_as_sf()

ggplot()+
  geom_sf(data = base, aes(fill= allcrimes_categorica))+
  scale_fill_manual(values= c("#fef0d9","#fdcc8a","#fc8d59", "#e34a33", "#b30000"), 
                    labels = c("0-4", "5-11", "12-23", "24-47", "48-136"),
                    name = "Number of \nfemale victims")+
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=70, face="bold"),
        legend.text = element_text(size=60))

ggsave("mapa_victimas.jpg", width = 35, height = 40)

# Mapa en Leaflet

pal <- colorFactor(
  palette = c("#fef0d9","#fdcc8a","#fc8d59", "#e34a33", "#b30000"),
  domain = base$allcrimes_categorica,
  na.color = "NA"
)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=base,
              color = ~pal(allcrimes_categorica),
              weight = 1,
              opacity = 1,
              fillOpacity = 1)%>% 
  addPolygons(data= agebs,
              fillColor = "transparent",
              color= "black",
              weight = 0.5) %>% 
  addLegend("bottomright", pal = pal, values = base$allcrimes_categorica,
            title = "Number of \nfemale victims",
            opacity = 1)-> mapa

mapa

mapshot(mapa, file = paste0("mapa_victimas_leaflet.jpg"))


