
# 01_Cargar Librerias
# library(tidyverse)
# library(readr)
# library(dplyr)
# library(ggplot2)
# library(rio)
# library(skimr)
# library(visdat)
# library(stargazer)
##########################################################
# Title: Data Cleaning.
# Description: This script webscrapes the data from the website
# https://ignaciomsarmiento.github.io/GEIH2018_sample/ the
# objective is to retrive 10 chunks of data from the website
# corresponding to a sample of GEIH 2018. Each one would be
# stored in a individual csv file.
#
# Date: 09/02/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 0. Workspace configuration ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$stores <- file.path(dir$root, "stores", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))


# 02_load inputs GEIH

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


# DATA CLEANING
db_geih_1 <- db_geih

# FILTER (>= 18 YEAR AND LABOR POPULATION) 
db_geih_2 <- db_geih_1 %>% filter(age>=18,dsi==0)

skim(db_geih_2)

# COLUMNS FILTER, COMPLETENESS OF DATA 70% OR ECONOMIC IMPORTANCE 70% (42 variables)
db_geih_3 <- db_geih_2 %>% select(c(directorio,secuencia_p,orden,clase,mes,estrato1,sex,age,
                                    p6050,p6090,p6100,p6210,p6210s1,p6240,p7495,
                                    p7500s1a1,p7500s2a1,p7500s3a1,p7505,p7510s1a1,p7510s2a1,p7510s3a1,p7510s5a1,p7510s6a1,p7510s7a1,
                                    pet,iof1,iof2,iof3h,iof3i,iof6,
                                    ingtotob,ingtot,maxEducLevel,regSalud,
                                    wap,ocu,dsi,pea,inac,
                                    totalHoursWorked,formal # relevantes
)) 

skim(db_geih_3)

# TRANSFORMATION TO FACTOR CATAGORICAL VARIABLES

db_geih_4 <- db_geih_3 %>% mutate(sex=as.factor(sex),
                                  estrato1=as.factor(estrato1),
                                  age=as.factor(age),
                                  p6050=as.factor(p6050),
                                  p6090=as.factor(p6090),
                                  p6100=as.factor(p6100),
                                  p6210=as.factor(p6210),
                                  p6210s1=as.factor(p6210s1),
                                  p6240=as.factor(p6240),
                                  p7495=as.factor(p7495),
                                  p7505=as.factor(p7505),
                                  pet=as.factor(pet),
                                  maxEducLevel=as.factor(maxEducLevel),
                                  regSalud=as.factor(regSalud),
                                  wap=as.factor(wap),
                                  ocu=as.factor(ocu),
                                  dsi=as.factor(dsi),
                                  pea=as.factor(pea),
                                  inac=as.factor(inac),
                                  formal=as.factor(formal)
)


# LAST FILTER WAGE > 0

db_geih_5 <- db_geih_4 %>% filter(ingtot>0)

# AVERAGES-MEANS OF NUMERICAL VARIABLES ON NAS
db_geih_6 <- db_geih_5 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  mutate(age=as.numeric(age))

# SAVE DATA CLEAN
write.csv(db_geih_6, file.path(dir$stores, paste0("data_limpiaGEIH", ".csv")), row.names = F)


## 3

# CONVERT TO HOUR WAGE AND LOG
db_geih_7 <- db_geih_6 %>% mutate(logwage=log(ingtot/totalHoursWorked), age2=age^2)

# REGRESSION
reg1 <- lm(logwage~age+age2, data=db_geih_7)

summary(reg1)

ggplot(db_geih_7, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = TRUE) +
  labs(title = "Regresión",
       x = "Age",
       y = "log(W)") +
  theme_minimal()

