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
test_hogares <- read.csv(file.path(dir$raw, "test_hogares.csv"))
test_personas <- read.csv(file.path(dir$raw, "test_personas.csv"))

# Rename columns train personas

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

# Rename columns train hogares

train_hogares <- train_hogares %>%  
  rename(Cuartos = P5000,
         CuartosDormir = P5010,
         TenenciaVivienda = P5090,
         CuotaAmortizacion = P5100,
         ArriendoEst = P5130,
         ArriendoEfec = P5140,
         Npersonas = Nper,
         NpersonasUG = Npersug,
         Lindigencia = Li,
         Lpobreza = Lp)

# Select relevant variables train personas and create new ones

train_personas <- train_personas %>% 
  mutate(Mujer = ifelse(Sexo==2,1,0),
         H_Head = ifelse(H_Head== 1, 1, 0),
         AfiliadoSalud = ifelse(is.na(AfiliadoSalud)|AfiliadoSalud==9|AfiliadoSalud==2, 0, 1),
         RegSalud = ifelse(is.na(RegSalud)|RegSalud==4,0,RegSalud),
         EducLevel = ifelse(EducLevel==9,0,EducLevel),
         Actividad = ifelse(is.na(Actividad),6,Actividad),
         TiempoTr = ifelse(is.na(TiempoTr), 0, TiempoTr),
         Posicion = ifelse(is.na(Posicion)|Posicion==9, 0, Posicion),
         IngHoraE = ifelse(is.na(IngHoraE)|IngHoraE==9|IngHoraE==2,0,1),
         PrimaMesPasado = ifelse(is.na(PrimaMesPasado)|PrimaMesPasado==9|PrimaMesPasado==2,0,1),
         BonoMesPasado = ifelse(is.na(BonoMesPasado)|BonoMesPasado==9|BonoMesPasado==2,0,1),
         AuxAlimMesPasado = ifelse(is.na(AuxAlimMesPasado)|AuxAlimMesPasado==9|AuxAlimMesPasado==2,0,1),
         AuxTransMesPasado = ifelse(is.na(AuxTransMesPasado)|AuxTransMesPasado==9|AuxTransMesPasado==2,0,1),
         AuxFamMesPasado = ifelse(is.na(AuxFamMesPasado)|AuxFamMesPasado==9|AuxFamMesPasado==2,0,1),
         AuxEduMesPasado = ifelse(is.na(AuxEduMesPasado)|AuxEduMesPasado==9|AuxEduMesPasado==2,0,1),
         AlimentosMesPasado = ifelse(is.na(AlimentosMesPasado)|AlimentosMesPasado==9|AlimentosMesPasado==2,0,1),
         ViviendaMesPasado = ifelse(is.na(ViviendaMesPasado)|ViviendaMesPasado==9|ViviendaMesPasado==2,0,1),
         TranspEmpresa = ifelse(is.na(TranspEmpresa)|TranspEmpresa==9|TranspEmpresa==2,0,1),
         IngEspecie = ifelse(is.na(IngEspecie)|IngEspecie==9|IngEspecie==2,0,1),
         Prima = ifelse(is.na(Prima),0,ifelse(Prima==1,1,0)),
         PrimaNavidad = ifelse(is.na(PrimaNavidad),0,ifelse(PrimaNavidad==1,1,0)),
         PrimaVacaciones = ifelse(is.na(PrimaVacaciones),0,ifelse(PrimaVacaciones==1,1,0)),
         Viaticos = ifelse(is.na(Viaticos),0,ifelse(Viaticos==1,1,0)),
         BonoAnual = ifelse(is.na(BonoAnual),0,ifelse(BonoAnual==1,1,0)),
         HorasT = ifelse(is.na(HorasT),0,HorasT),
         TamañoEmp = ifelse(is.na(TamañoEmp),0,TamañoEmp),
         CotPen = ifelse(is.na(CotPen),0,ifelse(CotPen==1,1,0)),
         OtroEmpleo = ifelse(is.na(OtroEmpleo),0,ifelse(OtroEmpleo==1,1,0)),
         HorasTOtro = ifelse(is.na(HorasTOtro),0,HorasTOtro),
         PosicionOtro = ifelse(is.na(PosicionOtro)|PosicionOtro==9, 0, PosicionOtro),
         MasHoras = ifelse(is.na(MasHoras),0,ifelse(MasHoras==1,1,0)),
         MasTrabajo = ifelse(is.na(MasTrabajo),0,ifelse(MasTrabajo==1,1,0)),
         Disponibilidad = ifelse(is.na(Disponibilidad),0,ifelse(Disponibilidad==1,1,0)),
         DilCambioEmpl = ifelse(is.na(DilCambioEmpl),0,ifelse(DilCambioEmpl==1,1,0)),
         DispCambioEmpl = ifelse(is.na(DispCambioEmpl),0,ifelse(DispCambioEmpl==1,1,0)),
         BusqTrabajo = ifelse(is.na(BusqTrabajo),0,ifelse(BusqTrabajo==1,1,2)),
         PosUltTrab = ifelse(is.na(PosUltTrab)|PosUltTrab==9, 0, PosUltTrab),
         IngTrabDesocu = ifelse(is.na(IngTrabDesocu),0,ifelse(IngTrabDesocu==1,1,0)),
         IngArrPens = ifelse(is.na(IngArrPens),0,ifelse(IngArrPens==1,1,0)),
         IngPension = ifelse(is.na(IngPension),0,ifelse(IngPension==1,1,0)),
         IngPAoDI = ifelse(is.na(IngPAoDI)|IngPAoDI==9|IngPAoDI==2,0,1),
         IngInstDivCes = ifelse(is.na(IngInstDivCes),0,ifelse(IngInstDivCes==1,1,0)),
         IngPInterior = ifelse(is.na(IngPInterior)|IngPInterior==9|IngPInterior==2,0,1),
         IngPExterior = ifelse(is.na(IngPExterior)|IngPExterior==9|IngPExterior==2,0,1),
         IngInst = ifelse(is.na(IngInst)|IngInst==9|IngInst==2,0,1),
         IngAhorros = ifelse(is.na(IngAhorros)|IngAhorros==9|IngAhorros==2,0,1),
         IngCes = ifelse(is.na(IngCes)|IngCes==9|IngCes==2,0,1),
         IngOtros = ifelse(is.na(IngOtros)|IngOtros==9|IngOtros==2,0,1),
         Ocupado = ifelse(is.na(Ocupado),0,1)) %>%
  select(id, Orden, Clase, Dominio, Mujer, Edad, H_Head, AfiliadoSalud, RegSalud,
       EducLevel, Actividad, TiempoTr, Posicion, IngHoraE, PrimaMesPasado,
       BonoMesPasado, AuxAlimMesPasado, AuxTransMesPasado, AuxFamMesPasado,
       AuxEduMesPasado, AlimentosMesPasado, ViviendaMesPasado, TranspEmpresa,
       IngEspecie, Prima, PrimaNavidad, PrimaVacaciones, Viaticos, BonoAnual,
       HorasT, TamañoEmp, CotPen, OtroEmpleo, HorasTOtro, PosicionOtro, MasHoras, 
       MasTrabajo, Disponibilidad, DilCambioEmpl, DispCambioEmpl, BusqTrabajo, 
       PosUltTrab, IngTrabDesocu, IngArrPens, IngPension, IngPAoDI, IngInstDivCes, 
       IngPInterior, IngPExterior, IngInst, IngAhorros, IngCes, IngOtros, Ocupado)

# Select relevant variables train hogares and create new ones

train_hogares <- train_hogares %>%
  select(id, Clase, Dominio, Cuartos, CuartosDormir, TenenciaVivienda,
         CuotaAmortizacion, ArriendoEst, ArriendoEfec, Npersonas, NpersonasUG,
         Lindigencia, Lpobreza, Fex_c, Depto, Fex_dpto, Pobre) %>%  
  mutate(Pobre = factor(Pobre,levels=c(0,1),labels=c("No","Yes")),
         Dominio=factor(Dominio),
         arriendo=ifelse(!is.na(ArriendoEfec),ArriendoEfec,
                         ifelse(!is.na(CuotaAmortizacion),CuotaAmortizacion,
                                ArriendoEst)),
         CuotaAmortizacion=ifelse(is.na(CuotaAmortizacion), 0, CuotaAmortizacion),
         ArriendoEst = ifelse(is.na(ArriendoEst), 0, ArriendoEst),
         ArriendoEfec = ifelse(is.na(ArriendoEfec), 0, ArriendoEfec))

# Create new data frame with vars personas transformed to level hogares

train_personas_nivel_hogar <- train_personas %>% 
  group_by(id) %>% 
  summarize (nMujeres = sum(Mujer, na.rm=TRUE),
             nAfiliados = sum(AfiliadoSalud, na.rm=TRUE),
             nSubsidiado = sum(RegSalud==3, na.rm=TRUE),
             maxEducLevel=max(EducLevel,na.rm=TRUE),
             nTrabajadores = sum(Actividad==1, na.rm=TRUE),
             maxTiempoTr = max(TiempoTr, na.rm=TRUE),
             nEmpleado = sum(Posicion==1|Posicion==2, na.rm=TRUE),
             nJefe = sum(Posicion==5, na.rm=TRUE),
             nCuentaPropia = sum(Posicion==4, na.rm=TRUE),
             nMenorRango = sum(Posicion %in% c(3,6,7,8,9), na.rm=TRUE),
             nIngHoraT = sum(IngHoraE, na.rm=TRUE),
             nPrimaMesPasado = sum(PrimaMesPasado, na.rm = TRUE),
             nBonoMesPasado = sum(BonoMesPasado, na.rm = TRUE),
             nAuxAlimMesPasado = sum(AuxAlimMesPasado, na.rm = TRUE),
             nAuxTransMesPasado = sum(AuxTransMesPasado, na.rm = TRUE),
             nAuxFamMesPasado = sum(AuxFamMesPasado, na.rm = TRUE),
             nAuxEduMesPasado = sum(AuxEduMesPasado, na.rm = TRUE),
             nAlimentosMesPasado = sum(AlimentosMesPasado, na.rm = TRUE),
             nViviendaMesPasado = sum(ViviendaMesPasado, na.rm = TRUE),
             nTranspEmpresa = sum(TranspEmpresa, na.rm = TRUE),
             nIngEspecie = sum(IngEspecie, na.rm = TRUE),
             nPrima = sum(Prima, na.rm = TRUE),
             nPrimaNavidad = sum(PrimaNavidad, na.rm = TRUE),
             nPrimaVacaciones = sum(PrimaVacaciones, na.rm = TRUE),
             nViaticos = sum(Viaticos, na.rm = TRUE),
             nBonoAnual = sum(BonoAnual, na.rm = TRUE),
             nTiempoCompleto = sum(HorasT>=40, na.rm=TRUE),
             maxTamañoEmp = max(TamañoEmp, na.rm=TRUE),
             nCotPen = sum(CotPen, na.rm = TRUE),
             nOtroEmpleo = sum(OtroEmpleo, na.rm = TRUE),
             nMasHoras = sum(MasHoras, na.rm = TRUE),
             nMasTrabajo = sum(MasTrabajo, na.rm = TRUE),
             nDisponibilidad = sum(Disponibilidad, na.rm = TRUE),
             nDilCambioEmpl = sum(DilCambioEmpl, na.rm = TRUE),
             nDispCambioEmpl = sum(DispCambioEmpl, na.rm = TRUE),
             nBusqTrabajo = sum(BusqTrabajo, na.rm = TRUE),
             nIngTrabDesocu = sum(IngTrabDesocu, na.rm = TRUE),
             nIngArrPens = sum(IngArrPens, na.rm = TRUE),
             nIngPension = sum(IngPension, na.rm = TRUE),
             nIngPAoDI = sum(IngPAoDI, na.rm = TRUE),
             nIngInstDivCes = sum(IngInstDivCes, na.rm = TRUE),
             nIngPInterior = sum(IngPInterior, na.rm = TRUE),
             nIngPExterior = sum(IngPExterior, na.rm = TRUE),
             nIngInst = sum(IngInst, na.rm = TRUE),
             nIngAhorros = sum(IngAhorros, na.rm = TRUE),
             nIngCes = sum(IngCes, na.rm = TRUE),
             nIngOtros = sum(IngOtros, na.rm = TRUE),
             nOcupado = sum(Ocupado, na.rm = TRUE))

# Rename columns test personas

test_personas <- test_personas %>%
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

# Rename columns test hogares

test_hogares <- test_hogares %>%  
  rename(Cuartos = P5000,
         CuartosDormir = P5010,
         TenenciaVivienda = P5090,
         CuotaAmortizacion = P5100,
         ArriendoEst = P5130,
         ArriendoEfec = P5140,
         Npersonas = Nper,
         NpersonasUG = Npersug,
         Lindigencia = Li,
         Lpobreza = Lp)
