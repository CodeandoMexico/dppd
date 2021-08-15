rm(list=ls())

# Paquetes
library(readr)
library(dplyr)
library(tidyverse)
library(rgdal)
library(sf)
library(sp)
library(ggplot2)
library(rgdal)
library(leaflet)
library(mapview)
library(rgdal)
library(geojsonio)
library(spdplyr)
library(rmapshaper)

#### Hacer la selección de los PDS e identificarlos en imágenes de Leaflet #### 

# Cargar los resultados de los tres modelos
pd_linear <- read_csv("pd_linearreg.csv")
pd_lasso <- read_csv("pd_lasso.csv")
pd_nb <- read_csv("pd_nb.csv")

# Unir bases
pds <- bind_rows(pd_linear, pd_lasso, pd_nb)

# Renombrar variable de resultado
pds <- pds %>% 
  mutate(result_var = ifelse (result_var == "g1" | result_var == "g1_log", "severity1",
                              ifelse(result_var == "g2" | result_var == "g2_log", "severity2", 
                                     ifelse(result_var == "g3" | result_var == "g3_log",
                                            "severity3", ifelse(result_var == "allcrimes_log", 
                                                                 "allcrimes", result_var)))))

table(pds$result_var)

# Separar por nivel de severidad
severity1 <- pds %>% 
  filter(result_var == "severity1")

severity2 <- pds %>% 
  filter(result_var == "severity2")

severity3 <- pds %>% 
  filter(result_var == "severity3")

allcrimes <- pds %>% 
  filter(result_var == "allcrimes")

# Identificar PDs que aparecen en más de un modelo por nivel de severidad
severity1  <-  severity1 %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

severity2  <-  severity2 %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

severity3  <-  severity3 %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

allcrimes  <-  allcrimes %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

# Eliminar los PDs si solo aparecen en un modelo y no es en el de binomial negativa por nivel de severidad
severity1 <- severity1 %>% 
  filter(duplicado > 1  | model == "nb")

severity2 <- severity2 %>% 
  filter(duplicado > 1  | model == "nb")

severity3 <- severity3 %>% 
  filter(duplicado > 1  | model == "nb")

allcrimes <- allcrimes %>% 
  filter(duplicado > 1  | model == "nb")

# Identificar los PDs por nivel de prioridad y iivel de severidad
allcrimes <- allcrimes %>% 
  mutate(priority = ifelse(duplicado == 3, 1, 
                         ifelse(model== "nb" & (duplicado == 2 | duplicado == 1), 2, 
                                ifelse(duplicado == 2 & (model == "linearreg"| model == "lasso"), 3, 0))))

severity1 <- severity1 %>% 
  mutate(priority = ifelse(duplicado == 3, 1, 
                         ifelse(model== "nb" & (duplicado == 2 | duplicado == 1), 2, 
                                ifelse(duplicado == 2 & (model == "linearreg"| model == "lasso"), 3, 0))))

severity2 <- severity2 %>% 
  mutate(priority = ifelse(duplicado == 3, 1, 
                         ifelse(model== "nb" & (duplicado == 2 | duplicado == 1), 2, 
                                ifelse(duplicado == 2 & (model == "linearreg"| model == "lasso"), 3, 0))))

severity3 <- severity3 %>% 
  mutate(priority = ifelse(duplicado == 3, 1, 
                         ifelse(model== "nb" & (duplicado == 2 | duplicado == 1), 2, 
                                ifelse(duplicado == 2 & (model == "linearreg"| model == "lasso"), 3, 0))))

# PDs seleccionados por nivel de severidad
severity1 <- severity1 %>% 
  group_by(CVEGEO, cluster, result_var, priority) %>% 
  summarise(.groups = 'drop')
severity1

severity2 <- severity2 %>% 
  group_by(CVEGEO, cluster, result_var, priority) %>% 
  summarise(.groups = 'drop')
severity2

severity3 <- severity3 %>% 
  group_by(CVEGEO, cluster, result_var, priority) %>% 
  summarise(.groups = 'drop')
severity3 

allcrimes <- allcrimes %>% 
  group_by(CVEGEO, cluster, result_var, priority) %>% 
  summarise(.groups = 'drop')
allcrimes

# Guardar los PDs seleccionados por nivel de severidad
write.csv(severity1, "pds_s1.csv", row.names = F)
write.csv(severity2, "pds_s2.csv", row.names = F)
write.csv(severity3, "pds_s3.csv", row.names = F)
write.csv(allcrimes, "pds_allcrimes.csv", row.names = F)

# Base de PDs seleccionados
pds_select <- bind_rows(severity1, severity2, severity3, allcrimes)
n_distinct(pds_select$CVEGEO)

pds_select  <-  pds_select %>% 
  group_by(CVEGEO) %>% 
  mutate(duplicado = n())

# mapa
pds_mapa <- pds_select %>% 
  mutate(PD = ifelse (duplicado > 1, "duplicate", result_var))

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

# Unir AGEBS con los PD
pd <- left_join(agebs_sf, pds_mapa[,c("CVEGEO", "PD")], by="CVEGEO")

pd <- pd %>% 
  mutate(PD = ifelse (PD %in% NA, "nonPD", PD))

pd$PD <- factor(pd$PD, ordered= TRUE,
                levels = c("severity1", "severity2", "severity3", "allcrimes", "duplicate", "nonPD"))

table(pd$PD)

# Inlcuir la variable cluster
# Cargar la base de datos creada en el script 2.1.performance_measure
base <- read_csv("baseageb.csv")

pd <- left_join(pd, base[,c("CVEGEO", "cluster")], by="CVEGEO")
pd$cluster <- as.factor(pd$cluster)

# Mapa
pd %>% 
  filter(!(cluster %in% NA)) %>% 
  ggplot() +
  geom_sf(aes(color = cluster, fill= PD), size= 1.3)+
  scale_fill_manual(values = c("#ff006e", "#2d00f7", "#006400", "#ffb703", "#001427", "transparent"))+ 
  scale_color_manual(values = c("#ff85a1", "#0ead69", "#4361ee", "#7d8597"))+
  theme(legend.key.size = unit(3, 'cm'),
        legend.title = element_text(size=50),
        legend.text = element_text(size=40))

ggsave("mapa_pds.jpg", width = 30, height = 35)

n_distinct(pds_mapa$CVEGEO)

# Guardar los PDS seleccionados
pds_summary <- pds_mapa %>% 
  filter(row_number()==1)

write.csv(pds_summary, "pds_summary.csv")

#### Imagen de cada PD en Leaflet #### 
agebs_shp <- readOGR("marco_geoestadistico/cdmx/conjunto_de_datos/09a.shp",
                     stringsAsFactors = F)
proj4string(agebs_shp)
agebs_shp <- spTransform(agebs_shp,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

# Severity 1
# cluster 1
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901200012155"),] # Colegio militar

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901200012155.jpg"))

# cluster 2
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900800010387"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900800010387.jpg"))

# Severity 2
# cluster1
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010764"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010764.jpg"))

# cluster2
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900600010202"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900600010202.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900600010359"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900600010359.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700015037"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700015037.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901700010831"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901700010831.jpg"))

# Severity 3
# cluster1
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900300010770"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900300010770.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010957"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010957.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010834"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010834.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500011178"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500011178.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500010837"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500010837.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901400010242"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901400010242.jpg"))

# Cluster 2
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500010663"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500010663.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900600011037"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900600011037.jpg"))

# Cluster 3
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010872"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010872.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901200272032"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901200272032.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901200012206"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901200012206.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900500010807"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900500010807.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600011283"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600011283.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700014715"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700014715.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700012672"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700012672.jpg"))

# All crimes
# Cluster 1

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("090160001100A"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("090160001100A.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500010362"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500010362.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900200011150"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900200011150.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500010235"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500010235.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010533"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010533.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901600010834"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901600010834.jpg"))

#Cluster 2
agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900500011928"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900500011928.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900500010101"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900500010101.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900500013197"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900500013197.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700013755"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700013755.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700014024"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700014024.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700014772"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700014772.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900700015732"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0900700015732.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901000012362"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901000012362.jpg"))

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0901500010663"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "transparent",
              color = "red") -> m 
m

mapshot(m, file = paste0("0901500010663.jpg"))

#### Mapa Leaflet de todos los PDS ####

agebs_pds <- agebs_shp[agebs_shp@data$CVEGEO %in% c("0900800010387",
                                                    "0901200012155",
                                                    "0900600010202",
                                                    "0900600010359",
                                                    "0900700015037",
                                                    "0901600010764",
                                                    "0901700010831",
                                                    "0900300010770",
                                                    "0900500010807",
                                                    "0900600011037",
                                                    "0900700012672",
                                                    "0900700014715",
                                                    "0901400010242",
                                                    "0901500010663",
                                                    "0901500010837",
                                                    "0901500011178",
                                                    "0901600010834",
                                                    "0901600010872",
                                                    "0901600010957",
                                                    "0901600011283",
                                                    "0900200011150",
                                                    "0900500010101",
                                                    "0900500011928",
                                                    "0900500013197",
                                                    "0900700014024",
                                                    "0900700014772",
                                                    "0900700015732",
                                                    "0901000012362",
                                                    "0901500010235",
                                                    "0901500010362",
                                                    "0901600010533",
                                                    "090160001100A"),]

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data=agebs_pds,
              fillColor = "#ff5e5b",
              color = "#ff5e5b",
              fillOpacity = 0.8,
              weight = 1) %>% 
  addPolygons(data= agebs,
              fillColor = "transparent",
              color= "black",
              weight = 1)-> mapa 
mapa

mapshot(mapa, file = paste0("mapa_pds_leaflet.jpg"))
