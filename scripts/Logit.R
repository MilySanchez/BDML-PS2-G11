rm(list = ls())

# Set up paths

setwd("C:/Users/Juanita.Rojas/OneDrive - insidemedia.net/Documentos")

# Load required libraries

library(pacman)
p_load("tidyverse", "data.table", "ggplot2", "dplyr","ggcorrplot",
       "readr", "lubridate", "stringr", "rvest", "rio", "skimr",
       "visdat","stargazer","purrr", "caret", "boot", "glmnet",
       "Metrics", "car")

#install.packages('MLmetrics')
require(MLmetrics)

# Load inputs

train_hogares <- read.csv('train_hogares.csv')
train_personas <- read.csv('train_personas.csv')

test_hogares <- read.csv('test_hogares.csv')
test_personas <- read.csv('test_personas.csv')

# Rename columns hogares

train_hogares <- train_hogares %>%  
  rename(cuartos_hogar = P5000,
         cuartos_dormir = P5010,
         tenencia_vivienda = P5090,
         camortizacion = P5100,
         arriendo_est = P5130,
         arriendo_efec = P5140,
         npersonas = Nper,
         npersonas_ug = Npersug,
         lindigencia = Li,
         lpobreza = Lp)

test_hogares <- test_hogares %>%  
  rename(cuartos_hogar = P5000,
         cuartos_dormir = P5010,
         tenencia_vivienda = P5090,
         camortizacion = P5100,
         arriendo_est = P5130,
         arriendo_efec = P5140,
         npersonas = Nper,
         npersonas_ug = Npersug,
         lindigencia = Li,
         lpobreza = Lp)

# Select relevant variables personas

train_personas_filtrado <- train_personas %>%
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
    inactivo = Ina) %>% 
  select(id,
         Orden,
         Clase,
         Dominio,
         Estrato1,
         sexo,
         edad,
         afiliado_salud,
         seg_social_salud,
         educacion,
         actividad1,
         meses_trabajando,
         posicion_act1,
         pago_alimentos,
         prima_servicios12,
         horas_sem,
         personas_trabajo,
         cotiza_pension,
         posicion_act2,
         desocup_posicion,
         desocup_ing,
         ing_adicional,
         pension_alimenticia,
         dinero_hogares_fuera,
         dinero_instituciones,
         dinero_intereses,
         dinero_cesantias,
         dinero_otros,
         ocupado,
         Depto)

# rename variables in test personas

test_personas <- test_personas %>%
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

# select variables personas for model

train_personas_m2 <- train_personas_filtrado %>% 
  mutate(
    no_cabecera = ifelse(Clase==2,1,0),
    no_afiliado_salud = ifelse(afiliado_salud==2,1,0),
    reg_subsidiado = ifelse(seg_social_salud==3,1,0),
    educacion = ifelse(educacion==9,0,educacion),
    no_prima = ifelse(prima_servicios12==2,1,0),
    no_cotiza = ifelse(cotiza_pension==2,1,0),
    no_ing_ad = ifelse(ing_adicional==2,1,0),
    ayuda_instituciones = ifelse(dinero_instituciones==1,1,0),
    no_intereses = ifelse(dinero_intereses==2,1,0),
    no_cesantias = ifelse(dinero_cesantias==2,1,0),
    ocupado = ifelse(is.na(ocupado),0,1)
  ) %>% 
  select(
    id,
    Orden,
    no_cabecera,
    no_afiliado_salud,
    reg_subsidiado,
    educacion,
    no_prima,
    no_cotiza,
    no_ing_ad,
    ayuda_instituciones,
    no_intereses,
    no_cesantias,
    ocupado
  )

test_personas_m2 <- test_personas %>% 
  mutate(
    no_cabecera = ifelse(Clase==2,1,0),
    no_afiliado_salud = ifelse(afiliado_salud==2,1,0),
    reg_subsidiado = ifelse(seg_social_salud==3,1,0),
    educacion = ifelse(educacion==9,0,educacion),
    no_prima = ifelse(prima_servicios12==2,1,0),
    no_cotiza = ifelse(cotiza_pension==2,1,0),
    no_ing_ad = ifelse(ing_adicional==2,1,0),
    ayuda_instituciones = ifelse(dinero_instituciones==1,1,0),
    no_intereses = ifelse(dinero_intereses==2,1,0),
    no_cesantias = ifelse(dinero_cesantias==2,1,0),
    ocupado = ifelse(is.na(ocupado),0,1)
  ) %>% 
  select(
    id,
    Orden,
    no_cabecera,
    no_afiliado_salud,
    reg_subsidiado,
    educacion,
    no_prima,
    no_cotiza,
    no_ing_ad,
    ayuda_instituciones,
    no_intereses,
    no_cesantias,
    ocupado
  )

# create variables at household level

train_personas_hogar <- train_personas_m2 %>% 
  group_by(id) %>% 
  summarize(
    num_noafiliados = sum(no_afiliado_salud, na.rm = T),
    num_subsidiado = sum(reg_subsidiado, na.rm = T),
    max_educ = max(educacion, na.rm = T),
    num_noprima = sum(no_prima, na.rm = T),
    num_nocotiza = sum(no_cotiza, na.rm = T),
    num_noing = sum(no_ing_ad, na.rm = T),
    num_ayudainst = sum(ayuda_instituciones, na.rm = T),
    num_nointereses = sum(no_intereses, na.rm = T),
    num_nocesantias = sum(no_cesantias, na.rm = T),
    num_ocupados = sum(ocupado, na.rm = T)
  )

test_personas_hogar <- test_personas_m2 %>% 
  group_by(id) %>% 
  summarize(
    num_noafiliados = sum(no_afiliado_salud, na.rm = T),
    num_subsidiado = sum(reg_subsidiado, na.rm = T),
    max_educ = max(educacion, na.rm = T),
    num_noprima = sum(no_prima, na.rm = T),
    num_nocotiza = sum(no_cotiza, na.rm = T),
    num_noing = sum(no_ing_ad, na.rm = T),
    num_ayudainst = sum(ayuda_instituciones, na.rm = T),
    num_nointereses = sum(no_intereses, na.rm = T),
    num_nocesantias = sum(no_cesantias, na.rm = T),
    num_ocupados = sum(ocupado, na.rm = T)
  )

# select variables hogares for model

train_hogares <- train_hogares %>% 
  mutate(ocupante = ifelse(tenencia_vivienda==5,1,0),
         no_cabecera = ifelse(Clase==2,1,0)) %>% 
  select(
    id,
    no_cabecera,
    cuartos_hogar,
    ocupante,
    Pobre)

test_hogares <- test_hogares %>% 
  mutate(ocupante = ifelse(tenencia_vivienda==5,1,0),
         no_cabecera = ifelse(Clase==2,1,0)) %>% 
  select(
    id,
    no_cabecera,
    cuartos_hogar,
    ocupante)

# join data frames

train <- train_hogares %>% 
  left_join(train_personas_hogar) %>% 
  select(-id)

test <- test_hogares %>% 
  left_join(test_personas_hogar)

# convert categorical variables to factors

train <- train %>% 
  mutate(Pobre = factor(Pobre, levels = c(0,1),
                        labels = c("No", "Yes")),
         no_cabecera = factor(no_cabecera,
                              levels = c(0,1),
                              labels = c("NoCabecera", "Cabecera")),
         ocupante = factor(ocupante, levels = c(0,1),
                           labels = c("Otro", "Ocupante")),
         max_educ = factor(max_educ, levels = c(0:6),
                           labels = c("Ns", "Ninguno", "Preescolar",
                                      "Primaria", "Secundaria",
                                      "Media", "Superior"))) %>% 
  select(no_cabecera,
         cuartos_hogar,
         ocupante,
         num_subsidiado,
         max_educ,
         num_noprima,
         num_nocotiza,
         num_noing,
         num_ayudainst,
         num_ocupados,
         Pobre)

test <- test %>% 
  mutate(no_cabecera = factor(no_cabecera,
                              levels = c(0,1),
                              labels = c("NoCabecera", "Cabecera")),
         ocupante = factor(ocupante, levels = c(0,1),
                           labels = c("Otro", "Ocupante")),
         max_educ = factor(max_educ, levels = c(0:6),
                           labels = c("Ns", "Ninguno", "Preescolar",
                                      "Primaria", "Secundaria",
                                      "Media", "Superior"))) %>% 
  select(id,
         no_cabecera,
         cuartos_hogar,
         ocupante,
         num_subsidiado,
         max_educ,
         num_noprima,
         num_nocotiza,
         num_noing,
         num_ayudainst,
         num_ocupados)

# Check number of hogares pobres

table(train$Pobre)

# Oversampling the minority class

train <- upSample(x = train %>% 
                    select(-Pobre),
                  y = train$Pobre,
                  yname="Pobre")

###############

fiveStats <- function(...) c(prSummary(...))

ctrl <- trainControl(method = "cv",
                     number = 5,
                     classProbs = T,
                     summaryFunction = fiveStats,
                     savePredictions = T)

set.seed(111)

model2 <- train(Pobre~.,
                data = train,
                method = "glm",
                family = binomial,
                metric = "F",
                trControl = ctrl)

model2

##################

predictSample <- test %>% 
  mutate(pobre_lab = predict(model2, newdata = test,
                             type = "raw")) %>% 
  select(id, pobre_lab)

predictSample <- predictSample %>% 
  mutate(pobre = ifelse(pobre_lab == "Yes",1,0)) %>% 
  select(id, pobre)

template <- read.csv('sample_submission.csv')

name <- "Logit.csv"

write.csv(predictSample, name, row.names = F)
