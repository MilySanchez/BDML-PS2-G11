##########################################################
# Title: Data processing and cleaning
# Description: This script aims to clean and process the raw
# data. This includes treating missing values, transforming
# existing variables, and creating new relevant variables.
# Date: 30/03/2025
##########################################################

# =========================================================
# 0. Workspace configuration
# =========================================================

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list ()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "store", "processed")
dir$raw <- file.path(dir$root, "store", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# Load inputs

train_hogares <- read.csv(file.path(dir$raw, "train_hogares.csv"))
train_personas <- read.csv(file.path(dir$raw, "train_personas.csv"))

# Rename columns personas

train_personas <- train_personas %>%
  rename(
    Sexo = P6020,
    Edad = P6040,
    H_Head = P6050,
    AfiliadoSalud = P6090,
    RegSalud = P6100,
    EducLevel = P6210,
    Actividad = P6240,
    TiempoTr = P6426,
    Posicion = P6430,
    IngHoraE = P6510,
    PrimaMesPasado = P6545,
    BonoMesPasado = P6580,
    AuxAlimMesPasado = P6585s1,
    AuxTransMesPasado = P6585s2,
    AuxFamMesPasado = P6585s3,
    AuxEduMesPasado = P6585s4,
    AlimentosMesPasado = P6590,
    ViviendaMesPasado = P6600,
    TranspEmpresa = P6610,
    IngEspecie = P6620,
    Prima = P6630s1,
    PrimaNavidad = P6630s2,
    PrimaVacaciones = P6630s3,
    Viaticos = P6630s4,
    BonoAnual = P6630s6,
    HorasT = P6800,
    TamañoEmp = P6870,
    CotPen = P6920,
    OtroEmpleo = P7040,
    HorasTOtro = P7045,
    PosicionOtro = P7050,
    MasHoras = P7090,
    MasTrabajo = P7110,
    Disponibilidad = P7120,
    DilCambioEmpl = P7150,
    DispCambioEmpl = P7160,
    BusqTrabajo = P7310,
    PosUltTrab = P7350,
    IngTrabDesocu = P7422,
    IngArrPens = P7495,
    IngPension = P7500s2,
    IngPAoDI = P7500s3,
    IngInstDivCes = P7505,
    IngPInterior = P7510s1,
    IngPExterior = P7510s2,
    IngInst = P7510s3,
    IngAhorros = P7510s5,
    IngCes = P7510s6,
    IngOtros = P7510s7,
    Ocupado = Oc,
    Desocupado = Des,
    Inactivo = Ina)

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

# Exploratory data analysis hogares

train_hogares_num <- train_hogares %>%
  select(as.factor('Pobre'),
         cuartos,
         #camortizacion,
         cuartosdormir,
         arriendo_est,
         #arriendo_efec,
         npersonas,
         npersonas_ug)
cor_matrix <- cor(train_hogares_num, use = 'complete.obs')
cor_pobre <- cor_matrix['Pobre', ]
cor_pobre
# pobre = 1 con menor cantidad de cuartos y mayor num de personas en hogar

prop.table(table(train_hogares$tenencia_vivienda, train_hogares$Pobre), 
           margin = 2)
ggplot(train_hogares, aes(x = factor(tenencia_vivienda), 
                          fill = factor(Pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Categorías 5 y 6 mayor % de hogares pobres (posesión sin título u otra)

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
  filter(Pobre== 'Yes') %>% 
  select(id)

train_personas <- train_personas %>% 
  mutate(mujer = ifelse(sexo == 2,1,0),
         jefe_h = ifelse(parentesco_jefe == 1, 1, 0),
         EducLevel = ifelse(educacion == 9,0, educacion),
         ocupado = ifelse(is.na(ocupado),0,1),
         pobre = ifelse(id %in% id_hogares_pobres$id, 1, 0))

# Exploratory data analysis personas

train_personas_num <- train_personas %>%
  select(as.factor('pobre'),
         edad,
         meses_trabajando,
         horas_sem,
         personas_trabajo,
         horas_actividad2)
cor_matrix_p <- cor(train_personas_num, use = 'complete.obs')
cor_pobre_p <- cor_matrix_p['pobre', ]
cor_pobre_p
# num de personas/empleados en el trabajo tiene corr negativa con pobre = 1

prop.table(table(train_personas$Clase, train_personas$pobre), 
           margin = 2)
ggplot(train_personas, aes(x = factor(Clase), 
                          fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres en Clase=2 (no cabeceras)

ggplot(train_personas, aes(x = factor(afiliado_salud), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando afiliado_salud=2,9 (no o no sabe)

ggplot(train_personas, aes(x = factor(seg_social_salud), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando segsocial_salud=3,9 (subsidiado o no sabe)

ggplot(train_personas, aes(x = factor(educacion), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando educ!=5,6 (media y superior)

ggplot(train_personas, aes(x = factor(actividad1), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando act1=2 (buscando trabajo)

ggplot(train_personas, aes(x = factor(posicion_act1), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando posicionact1= 6,7 (trabajadores sin remuneración)

ggplot(train_personas, aes(x = factor(pago_alimentos), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando reciben alimentos como parte de pago

ggplot(train_personas, aes(x = factor(prima_servicios12), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando no recibieron prima de servicios

ggplot(train_personas, aes(x = factor(cotiza_pension), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando cotizapension = 2 (no cotiza)

ggplot(train_personas, aes(x = factor(posicion_act2), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando posicionact2 = 8,9 (jornalero/peón u otro)

ggplot(train_personas, aes(x = factor(desocup_posicion), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando desocposicion = 3,4,6,8 
# (empleado doméstico, trabajador cuenta propia, trabajador sin remuneración, jornalero)

ggplot(train_personas, aes(x = factor(desocup_ing), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando desocup_ing=2 (no recibio ingresos por trabajo)

ggplot(train_personas, aes(x = factor(ing_adicional), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando ingadicional=2
# (no recibio ingresos por arriendos o pensiones)

ggplot(train_personas, aes(x = factor(pension_alimenticia), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando pensionaliment=1
# (recibio pagos por pension aliment por paternidad, divorcio, separación)

ggplot(train_personas, aes(x = factor(dinero_hogares_fuera), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando dinhogfuera=2 (no recibio dinero de fuera)

ggplot(train_personas, aes(x = factor(dinero_instituciones), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando dininstitu=1 (si recibio ayudas de insti..)

ggplot(train_personas, aes(x = factor(dinero_intereses), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando dinintere=2 (no recibio dinero por intereses)

ggplot(train_personas, aes(x = factor(dinero_cesantias), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando dinces=2 (no recibio dinero por cesantías)

ggplot(train_personas, aes(x = factor(dinero_otros), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando dinotros=2,9 (no recibio o no sabe/informa)

ggplot(train_personas, aes(x = factor(ocupado), 
                           fill = factor(pobre))) +
  geom_bar(position = 'fill') +
  theme_minimal()
# Mayor % de personas pobres cuando ocupado=0


# Select relevant variables personas

train_personas <- train_personas %>% 
  select(id,
         Orden,
         Clase,
         Dominio,
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


##-----------------------------------------------------------------------------
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
