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

# Select relevant variables personas and create new ones



# Select relevant variables hogares and create new ones


