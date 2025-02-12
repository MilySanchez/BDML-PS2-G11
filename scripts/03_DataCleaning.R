
# 01_Cargar Librerias
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(rio)
library(skimr)
library(visdat)


# 02_Definir directorio

setwd("C:/Users/Asus/Documents/01_MAESTRIA/2 Semestre/ML/01_ProblemSet1")

# 02_ Cargar insumos GEIH del webscrapping

cargar_unir_tablas <- function(ruta = "Insumos/", n = 10) {
  # Generar los nombres de los archivos
  archivos <- paste0(ruta, "table_geih_", 1:n, ".csv")
  
  # Leer y combinar los archivos
  tabla_combinada <- archivos %>%
    lapply(read_csv) %>%
    bind_rows()
  
  return(tabla_combinada)
}

# Llamar a la función # 32.177 registros, 178 variables
db_geih <- cargar_unir_tablas()

# Columnas
variable.names(db_geih)
## print data
head(db_geih)
## summary db
skim(db_geih) %>% head()

# Limpieza NAS - 

db_geih_1 <- db_geih %>% select(-c(p550,y_gananciaNetaAgro_m))
skim(db_geih_1)

# Pre filtro columnas, completitud superior al 70% o relevancia ecónomica (42 variables)
db_geih_2 <- db_geih_1 %>% select(c(directorio,secuencia_p,orden,clase,mes,estrato1,sex,age,
                                    p6050,p6090,p6100,p6210,p6210s1,p6240,p7495,
                                    p7500s1a1,p7500s2a1,p7500s3a1,p7505,p7510s1a1,p7510s2a1,p7510s3a1,p7510s5a1,p7510s6a1,p7510s7a1,
                                    pet,iof1,iof2,iof3h,iof3i,iof6,
                                    ingtotob,ingtot,maxEducLevel,regSalud,
                                    wap,ocu,dsi,pea,inac,
                                    totalHoursWorked,formal # relevantes
                                    )) 

skim(db_geih_2)


