
# Data Powered Positive Deviance: Espacios públicos seguros para las mujeres en la CDMX

Este proyecto aplica el método de [Desviación Positiva Basada en Datos (Data Powered Positive Deviance)](https://www.undp.org/acceleratorlabs/blog/data-powered-positive-deviance-sustainable-development) para identificar lugares en la CDMX donde las mujeres están más seguras o donde la violencia de género es menor que en otras áreas con atributos similares. 

### Conjuntos de datos utilizados
Para este estudio buscamos bases de datos públicas que nos dieran información sobre dos aspectos relevantes para la problemática: (1) el nivel de seguridad para las mujeres en el espacio público, y (2) elementos que pueden incidir en que un espacio público sea más o menos seguro. También entró en consideración que datos son abiertos y estaban disponibles. A partir de estas 17 bases de datos, se construyeron las variables que se utilizan en este proyecto: 
- Censo de Población y Vivienda 2020
- Encuesta Origen Destino en Hogares de la Zona Metropolitana del Valle de México (EOD) 2017
- Marco geoestadístico 2020
- Directorio Estadístico Nacional de Unidades Económicas (DENUE) 2020/11
- Inventario Nacional de Viviendas 2016 (frente de manzanas)
- Áreas verdes de la Ciudad de México
- Estaciones de Metro
- Estaciones de Metrobús
- Paradas de Trolebús
- Paradas de RTP (Red de Transporte de Pasajeros)
- Puntos de arribo para los sistemas de transporte individual sustentable (SiTIS)
- Víctimas en carpetas de investigación FGJ
- Ubicación de puntos de acceso gratuito a internet WiFi vía infraestructura C5
- Pasos Seguros CDMX 
- Intersecciones seguras
- Distritos del Estudio Origen-Destino de la Zona Metropolitana del Valle de México 2017 para obtener el marco geoestadístico de los distritos de la CDMX - utilizados en la EOD
- La base de datos sobre la ubicación de los Senderos Seguros a la que nos dio acceso la Agencia Digital de Innovación Pública de la CDMX

[Aquí](https://drive.google.com/drive/folders/1PtFnnuCuYEj69Za_8wBCEitDXOW6Y7CF) puedes encontrar las bases de datos necesarias para ejecutar los scripts de este repositorio

### Métodos usados 

- Regresión lasso
- Regressión lineal
- Regressión binomial negativa
- Selección de modelos 
- Geoespacialización
- Identificación de PDs
- PD understanding

### Herramientas de análisis
1. R para el análisis de datos
   - readr
   - rgdal
   - dplyr
   - tidyverse
   - sf
   - ggplot2
   - BAMMtools
   - data.table
   - raster
   - MASS
   - caret
   - ggpubr
   - performance 
   - car
   - lmtest
   - glmnet
   - repr
   - MuMIn
   - effects
   - nlme
   - lmerTest
   - progress
   - fareway
   - pscl
   - boot
   - lme4
   - sandwich
   - AER
   - sjmisc
   - sjPlot
   - cowplot
   - utils
   - easypackages
   - sp
   - lattice
   - latticeExtra
   - ggpubr
2. La paquetería sf (simple features) en R para trabajar con objetos espaciales
3. Kepler para realizar mapas de calor 

## Tabla de contenidos

- [Scripts](#scripts)
- [Mapas](#mapas)
- [Contribuir](#contribuir)
- [Atribuciones](#atribuciones)
- [Licencia](#licencia)

## Scripts
Los scripts están agrupados en cuatro carpetas (de acuerdo con las etapas de la metodología de Data Powered Positive Deviance):

1. Homogeneous groups: sirve para agrupar las AGEBS de la CDMX en grupos con características similares en cuanto a factores no controlables en el mediano y corto plazo.  Se hace un análisis de clusters utilizando K medias. 
2. Performance measure: sirve para construir las variables explicativas y de resultados que se usan para crear los modelos de regresión. 
3. PD identification: se ejecutan modelos de regresión para identificar los valores atípicos positivos en los residuos (desviaciones positivas). Se crearon tres tipos de modelos: regresiones binomiales negativas, regresiones lineales y regresiones LASSO. Para cada tipo de regresión se crearon modelos por variable de resultado y por grupo homogéneo.   
4. PD understanding: después de identificar las desviaciones positivas (PDs) se realiza una comparación de las variables modificables en el corto plazo y que pueden incidir en la seguridad de los espacios públicos entre el grupo de PDs y no PDs. 


### Requerimientos
Los scripts se crearon usando RStudio v3.6.2. 
Los scripts están numerados y deben ejecutarse siguiendo el orden de la numeración. 

### Instalación
Al principio de cada script se enlistan las librerías que deben instalarse e importarse para poder ejecutar los scripts. 

### Uso
Los scripts identifican las AGEBS en la CDMX que son desviaciones positivas. Solo ejecuta siguiendo los comentarios de los scripts.


## Outputs

**Mapas**:

- AGEBS urbanas de la CDMX (script 1.1)
- AGEBS por grupos homogéneos (script 1.1) 
- Número de víctimas mujeres en carpetas de investigación de los delitos de los tres niveles de severidad por AGEB (script 1.2)
- AGEBS que son desviaciones positivas (PDs) (script 3.4) 

**Datasets**:

- Variables para crear los grupos homogéneos (basecluster.csv) (script 1.1).
- Variables para ejecutar los modelos (baseageb.csv) (script 2.1). 
- PDs por tipo de regresión (pd_linearreg.csv; pd_lasso.csv; pd_nb.csv) (scripts 3.1.4, 3.2.4 y 3.3.4).
- Variables para hacer la comparación entre PDs y no PDs (base_pdunderstanding.csv) (script 4.1).

[Aquí](https://drive.google.com/drive/u/1/folders/1HgBpSf1u-Oo_6oaWaX5g3mN24ydkcxIs) encontrarás los datasets creados en los scprits 1.1, 2.1, 3.1.4, 3.2.4, 3.3.4 y 4.1 (outputs) 

## Contribuciones
Para realizar cualquier contribución puedes abrir un issue, hacer un pull request o contactar a acclabmx@undp.org. 

## Referencias
- [Blog 1: Identificando espacios públicos seguros para las mujeres en Ciudad de México](https://www.undp.org/es/mexico/news/pnud-giz-y-semujeres-presentan-recomendaciones-para-crear-espacios-p%C3%BAblicos-m%C3%A1s-seguros-para-las-mujeres-en-la-ciudad-de-m%C3%A9xico)
- [Blog 2: Identificando espacios públicos seguros para las mujeres en Ciudad de México. Parte 2: Del análisis cuantitativo al cualitativo](https://www.undp.org/es/mexico/blog/identificando-espacios-p%C3%BAblicos-m%C3%A1s-seguros-para-las-mujeres-en-ciudad-de-m%C3%A9xico-parte-2-del-an%C3%A1lisis-cuantitativo-al-cualitativo)
- [PNUD, GIZ y SEMUJERES presentan recomendaciones para crear espacios públicos más seguros para las mujeres en la Ciudad de México](https://www.undp.org/es/mexico/news/pnud-giz-y-semujeres-presentan-recomendaciones-para-crear-espacios-p%C3%BAblicos-m%C3%A1s-seguros-para-las-mujeres-en-la-ciudad-de-m%C3%A9xico)
– [The Data Powered Positive Deviance Handbook] (https://www.undp.org/acceleratorlabs/publications/data-powered-positive-deviance-handbook)

## Atribuciones

- Itzel Soto @itzsp 
- Alma Rangel @almarngl
- Ricardo Mirón @ricardomiron
- Nadia Neri Vera @NadiaNeriVera


## Licencia
Los contenidos y productos de este repositorio están publicados bajo el licenciamiento MIT.
