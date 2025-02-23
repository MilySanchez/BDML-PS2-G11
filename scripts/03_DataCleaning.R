
# 01_Cargar Librerias
library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(rio)
library(skimr)
library(visdat)
library(stargazer)


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


# filtro mayores de edad y ocupados y 
db_geih_2 <- db_geih_1 %>% filter(age>=18,dsi==0)

skim(db_geih_2)

# Pre filtro columnas, completitud superior al 70% o relevancia ecónomica (42 variables)
db_geih_3 <- db_geih_2 %>% select(c(directorio,secuencia_p,orden,clase,mes,estrato1,sex,age,
                                    p6050,p6090,p6100,p6210,p6210s1,p6240,p7495,
                                    p7500s1a1,p7500s2a1,p7500s3a1,p7505,p7510s1a1,p7510s2a1,p7510s3a1,p7510s5a1,p7510s6a1,p7510s7a1,
                                    pet,iof1,iof2,iof3h,iof3i,iof6,
                                    ingtotob,ingtot,maxEducLevel,regSalud,
                                    wap,ocu,dsi,pea,inac,
                                    totalHoursWorked,formal # relevantes
)) 

skim(db_geih_3)

# Transformación variables categorias a factor

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


#filtro final (ingresos mayores a 0)

db_geih_5 <- db_geih_4 %>% filter(ingtot>0)

# Inputación Medias de variables numericas
db_geih_6 <- db_geih_5 %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  mutate(age=as.numeric(age))

# guardar data limpia
write.csv(db_geih_6,"data_limpiaGEIH.csv",row.names = F)


## PUNTO 3

# transformación a ingreso por horas laboradas y log
db_geih_7 <- db_geih_6 %>% mutate(logwage=log(ingtot/totalHoursWorked), age2=age^2)

# regresion
reg1 <- lm(logwage~age+age2, data=db_geih_7)

summary(reg1)

ggplot(db_geih_7, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = TRUE) +
  labs(title = "Regresión",
       x = "Age",
       y = "log(W)") +
  theme_minimal()

