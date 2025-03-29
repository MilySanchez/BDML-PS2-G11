##########################################################
# Title: Data cleaning and processing
# Description: This script aims to clean and process the raw
# data. This includes treating missing values, transforming
# existing variables, and creating new relevant variables.
# Date: 18/03/2025
##########################################################

# =========================================================
# 0. Workspace configuration
# =========================================================

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list ()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "stores", "processed")
dir$raw <- file.path(dir$root, "stores", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

### setwd("C:/Users/Juanita.Rojas/OneDrive - insidemedia.net/Documentos")

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

### library(pacman)
### p_load("tidyverse", "data.table", "ggplot2", "dplyr","ggcorrplot",
       "readr", "lubridate", "stringr", "rvest", "rio", "skimr",
       "visdat","stargazer","purrr", "caret", "boot", "glmnet",
       "Mlmetrics", "Metrics")

# Load inputs

train_hogares <- read.csv('train_hogares.csv')
train_personas <- read.csv('train_personas.csv')

# Rename columns personas

train_personas <- train_personas %>%
  rename(
    sexo = P6020,
    edad = P6040,
    parentesco_jefe = P6050,
    afiliado_salud = P6090,
    seg_social_salud = P6100,
    educacion = P6210,
    grado_escolar = P6210s1,
    actividad1 = P6240,
    meses_trabajando = P6426,
    posicion_act1 = P6430,
    horas_extras = P6510,
    primas = P6545,
    bonificaciones = P6580,
    auxilio_alimentacion = P6585s1,
    auxilio_transporte = P6585s2,
    subsidio_familiar = P6585s3,
    subsidio_educativo = P6585s4,
    pago_alimentos = P6590,
    pago_vivienda = P6600,
    transporte_empresa = P6610,
    ingresos_especie = P6620,
    prima_servicios12 = P6630s1,
    prima_navidad12 = P6630s2,
    prima_vacaciones12 = P6630s3,
    viaticos12 = P6630s4,
    bonificaciones_anuales12 = P6630s6,
    horas_sem = P6800,
    personas_trabajo = P6870,
    cotiza_pension = P6920,
    otro_trabajo = P7040,
    horas_actividad2 = P7045,
    posicion_act2 = P7050,
    trabajar_mas = P7090,
    trabajar_mas4 = P7110,
    disp_trabajar_mas = P7120,
    cambiar_trabajo4 = P7150,
    trabajo_1vez = P7310,
    desocup_posicion = P7350,
    desocup_ing = P7422,
    ing_adicional = P7495,
    pension_vejez = P7500s2,
    pension_alimenticia = P7500s3,
    ing_otros = P7505,
    dinero_hogares_residentes = P7510s1,
    dinero_hogares_fuera = P7510s2,
    dinero_instituciones = P7510s3,
    dinero_intereses = P7510s5,
    dinero_cesantias = P7510s6,
    dinero_otros = P7510s7,
    ocupado = Oc,
    desocupado = Des,
    inactivo = Ina)

# Select relevant variables personas

train_personas <- train_personas %>% 
  select(id,
         Orden,
         Clase,
         Dominio,
         sexo,
         edad,
         parentesco_jefe,
         afiliado_salud,
         seg_social_salud,
         educacion,
         grado_escolar,
         actividad1,
         meses_trabajando,
         posicion_act1,
         horas_extras,
         primas,
         bonificaciones,
         auxilio_alimentacion,
         auxilio_transporte,
         subsidio_familiar,
         subsidio_educativo,
         pago_alimentos,
         pago_vivienda,
         transporte_empresa,
         ingresos_especie,
         prima_servicios12,
         prima_navidad12,
         prima_vacaciones12,
         viaticos12,
         bonificaciones_anuales12,
         horas_sem,
         personas_trabajo,
         cotiza_pension,
         otro_trabajo,
         horas_actividad2,
         posicion_act2,
         trabajar_mas,
         trabajar_mas4,
         disp_trabajar_mas,
         cambiar_trabajo4,
         trabajo_1vez,
         desocup_posicion,
         desocup_ing,
         ing_adicional,
         pension_vejez,
         pension_alimenticia,
         ing_otros,
         dinero_hogares_residentes,
         dinero_hogares_fuera,
         dinero_instituciones,
         dinero_intereses,
         dinero_cesantias,
         dinero_otros,
         ocupado,
         desocupado,
         inactivo,
         Pet,
         Fex_c,
         Depto,
         Fex_dpto)

# Rename columns hogares

train_hogares <- train_hogares %>%  
  rename(cuartos = P5000,
         cuartosdormir = P5010,
         tenencia_vivienda = P5090,
         camortizacion = P5100,
         arriendo_est = P5130,
         arriendo_efec = P5140,
         npersonas = Nper,
         npersonas_ug = Npersug,
         lindigencia = Li,
         lpobreza = Lp)

# Select relevant variables hogares

train_hogares <- train_hogares %>%  
  select(id,
         Clase,
         Dominio,
         cuartos,
         cuartosdormir,
         tenencia_vivienda,
         camortizacion,
         arriendo_est,
         arriendo_efec,
         npersonas,
         npersonas_ug,
         lindigencia,
         lpobreza,
         Fex_c,
         Depto,
         Fex_dpto,
         Pobre) %>% 
  mutate(Pobre = factor(Pobre,levels = c(0,1),
                        labels = c("No","Yes")),
         Dominio = factor(Dominio))

# Check number of hogares pobres

table(train_hogares$Pobre)

# Oversampling the minority class

train_hogares <- upSample(x = train_hogares %>% 
                            select(-Pobre),
                          y = train_hogares$Pobre,
                          yname="Pobre")

id_hogares_pobres = train_hogares %>% 
  filter(Pobre==1) %>% 
  select(id)

personas_pobres = train_personas %>% 
  filter(id %in% id_hogares_pobres$id)

train_personas <- train_personas %>% 
  mutate(mujer = ifelse(sexo == 2,1,0),
         jefe_h = ifelse(parentesco_jefe == 1, 1, 0),
         EducLevel = ifelse(educacion == 9,0, educacion),
         ocupado = ifelse(is.na(ocupado),0,1),
         cotiza_pension = ifelse(is.na(cotiza_pension),0,
                                 ifelse(cotiza_pension==1,1,0)),
         prima_servicios12 = ifelse(is.na(prima_servicios12),0,
                                    ifelse(prima_servicios12==1,1,0))) %>% 
  select(id,
         Orden,
         mujer,
         jefe_h,
         EducLevel,
         ocupado)

train_personas_nivel_hogar <- train_personas %>%
  group_by(id) %>% 
  summarize (nocupado=sum(ocupado, na.rm=T),
             maxEducLevel=max(EducLevel,na.rm=TRUE))

train_personas_hogar <- train_personas %>% filter(jefe_h==1) %>%
  select(id, mujer, EducLevel, ocupado) %>%
  rename(jefe_hmujer=mujer, jefe_hedu=EducLevel, jefe_hocupado = ocupado) %>% 
  left_join(train_personas_nivel_hogar)

#TEST

test_hogares <- read.csv("test_hogares.csv")
test_personas <- read.csv("test_personas.csv")

test_personas<- test_personas %>%
  mutate(mujer = ifelse(P6020==2,1,0), 
         jefe_h = ifelse(P6050== 1, 1, 0),
         EducLevel = ifelse(P6210==6,1,0),
         ocupado = ifelse(is.na(Oc),0,1),
         cotiza_pension = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
         prima_servicios12 = ifelse(is.na(P6630s1),0,
                                    ifelse(P6630s1==1,1,0))) %>% 
  select(id,
         Orden,
         mujer,
         jefe_h,
         EducLevel,
         ocupado)

test_personas_nivel_hogar <- test_personas %>%
  group_by(id) %>%
  summarize (nocupado = sum(ocupado, na.rm=T),
    maxEducLevel = max(EducLevel,na.rm=TRUE))

test_personas_hogar <- test_personas %>%
  filter(jefe_h == 1) %>%
  select(id,
         mujer,
         EducLevel,
         ocupado) %>%
  rename(jefe_hmujer= mujer, jefe_hedu=EducLevel, jefe_hocupado = ocupado) %>% 
  left_join(test_personas_nivel_hogar)