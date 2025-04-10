##########################################################
# Title: XGBoost training, prediction and importance
# variables
# Date: 5/04/2025
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
dir$model <- file.path(dir$root, "store", "models")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries
source(file.path(dir$scripts, "00_load_requierments.R"))

# =========================================================
# 1. Load inputs  and data transformation
# =========================================================

train_hogares<-read.csv(file.path(dir$raw, "train_hogares.csv"))
train_personas<-read.csv(file.path(dir$raw, "train_personas.csv"))

train_personas <- train_personas |> 
  select(id, Orden, Clase, Dominio, P6020, P6040, P6050, P6090, P6100, P6210, 
         P6210s1, P6240, Oficio, P6426, P6430, P6510, P6545, P6580, P6585s1, 
         P6585s2, P6585s3, P6585s4, P6590, P6600, P6610, P6620, P6630s1, 
         P6630s2, P6630s3, P6630s4, P6630s6, P6800, P6870, P6920, P7040, 
         P7045, P7050, P7090, P7110, P7120, P7150, P7160, P7310, P7350, 
         P7422, P7472, P7495, P7500s2, P7500s3, P7505, P7510s1, P7510s2, 
         P7510s3, P7510s5, P7510s6, P7510s7, Pet, Oc, Des, Ina, Fex_c, 
         Depto, Fex_dpto)

train_hogares <- train_hogares |> 
  select(id, Clase, Dominio, P5000, P5010, P5090, P5100, P5130, P5140, Nper, 
         Npersug, Li, Lp, Fex_c, Depto, Fex_dpto, Pobre) |> 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),Dominio=factor(Dominio)) |>
  mutate(arriendo=ifelse(!is.na(P5140),P5140,ifelse(!is.na(P5100),P5100,P5130)),
         P5100=ifelse(is.na(P5100), 0, P5100),
         P5130=ifelse(is.na(P5130), 0, P5130),
         P5140=ifelse(is.na(P5140), 0, P5140))

train_hogares <- downSample(x = train_hogares |> select(-Pobre), y = train_hogares$Pobre, yname="Pobre")

train_personas<- train_personas %>% mutate(
  Mujer = ifelse(P6020==2,1,0), 
  Edad = P6040,
  H_Head = ifelse(P6050== 1, 1, 0),
  AfiliadoSalud = ifelse(is.na(P6090)|P6090==9|P6090==2, 0, 1),
  RegSalud = ifelse(is.na(P6100)|P6100==4,0,P6100),
  EducLevel = ifelse(P6210==9,0,P6210),
  Actividad = ifelse(is.na(P6240),6,P6240),
  TiempoTr = ifelse(is.na(P6426), 0, P6426),
  Posicion = ifelse(is.na(P6430)|P6430==9, 0, P6430),
  IngHoraE = ifelse(is.na(P6510)|P6510==9|P6510==2,0,1),
  PrimaMesPasado = ifelse(is.na(P6545)|P6545==9|P6545==2,0,1),
  BonoMesPasado = ifelse(is.na(P6580)|P6580==9|P6580==2,0,1),
  AuxAlimMesPasado = ifelse(is.na(P6585s1)|P6585s1==9|P6585s1==2,0,1),
  AuxTransMesPasado = ifelse(is.na(P6585s2)|P6585s2==9|P6585s2==2,0,1),
  AuxFamMesPasado = ifelse(is.na(P6585s3)|P6585s3==9|P6585s3==2,0,1),
  AuxEduMesPasado = ifelse(is.na(P6585s4)|P6585s4==9|P6585s4==2,0,1),
  AlimentosMesPasado = ifelse(is.na(P6590)|P6590==9|P6590==2,0,1),
  ViviendaMesPasado = ifelse(is.na(P6600)|P6600==9|P6600==2,0,1),
  TranspEmpresa = ifelse(is.na(P6610)|P6610==9|P6610==2,0,1),
  IngEspecie = ifelse(is.na(P6620)|P6620==9|P6620==2,0,1),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0)),
  PrimaNavidad = ifelse(is.na(P6630s2),0,ifelse(P6630s2==1,1,0)),
  PrimaVacaciones = ifelse(is.na(P6630s3),0,ifelse(P6630s3==1,1,0)),
  Viaticos = ifelse(is.na(P6630s4),0,ifelse(P6630s4==1,1,0)),
  BonoAnual = ifelse(is.na(P6630s6),0,ifelse(P6630s6==1,1,0)),
  HorasT = ifelse(is.na(P6800),0,P6800),
  TamañoEmp = ifelse(is.na(P6870),0,P6870),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  OtroEmpleo = ifelse(is.na(P7040),0,ifelse(P7040==1,1,0)),
  HorasTOtro = ifelse(is.na(P7045),0,P7045),
  PosicionOtro = ifelse(is.na(P7050)|P7050==9, 0, P7050),
  MasHoras = ifelse(is.na(P7090),0,ifelse(P7090==1,1,0)),
  MasTrabajo = ifelse(is.na(P7110),0,ifelse(P7110==1,1,0)),
  Disponibilidad = ifelse(is.na(P7120),0,ifelse(P7120==1,1,0)),
  DilCambioEmpl = ifelse(is.na(P7150),0,ifelse(P7150==1,1,0)),
  DispCambioEmpl = ifelse(is.na(P7160),0,ifelse(P7160==1,1,0)),
  BusqTrabajo = ifelse(is.na(P7310),0,ifelse(P7310==1,1,2)),
  PosUltTrab = ifelse(is.na(P7350)|P7350==9, 0, P7350),
  IngTrabDesocu = ifelse(is.na(P7422),0,ifelse(P7422==1,1,0)),
  IngArrPens = ifelse(is.na(P7495),0,ifelse(P7495==1,1,0)),
  IngPension = ifelse(is.na(P7500s2),0,ifelse(P7500s2==1,1,0)),
  IngPAoDI = ifelse(is.na(P7500s3)|P7500s3==9|P7500s3==2,0,1),
  IngInstDivCes = ifelse(is.na(P7505),0,ifelse(P7505==1,1,0)),
  IngPInterior = ifelse(is.na(P7510s1)|P7510s1==9|P7510s1==2,0,1),
  IngPExterior = ifelse(is.na(P7510s2)|P7510s2==9|P7510s2==2,0,1),
  IngInst = ifelse(is.na(P7510s3)|P7510s3==9|P7510s3==2,0,1),
  IngAhorros = ifelse(is.na(P7510s5)|P7510s5==9|P7510s5==2,0,1),
  IngCes = ifelse(is.na(P7510s6)|P7510s6==9|P7510s6==2,0,1),
  IngOtros = ifelse(is.na(P7510s7)|P7510s7==9|P7510s7==2,0,1),
  Ocupado = ifelse(is.na(Oc),0,1)
) |> select(id, Orden, Mujer, Edad, H_Head, AfiliadoSalud, RegSalud, EducLevel, 
            Actividad, TiempoTr, Posicion, IngHoraE, PrimaMesPasado, BonoMesPasado,
            AuxAlimMesPasado, AuxTransMesPasado, AuxFamMesPasado, AuxEduMesPasado, 
            AlimentosMesPasado, ViviendaMesPasado, TranspEmpresa, IngEspecie, 
            Prima, PrimaNavidad, PrimaVacaciones, Viaticos, BonoAnual, HorasT, 
            TamañoEmp, CotPen, OtroEmpleo, HorasTOtro, PosicionOtro, MasHoras, 
            MasTrabajo, Disponibilidad, DilCambioEmpl, DispCambioEmpl, BusqTrabajo, 
            PosUltTrab, IngTrabDesocu, IngArrPens, IngPension, IngPAoDI, IngInstDivCes, 
            IngPInterior, IngPExterior, IngInst, IngAhorros, IngCes, IngOtros, Ocupado)


train_personas_nivel_hogar <- train_personas |>
  group_by(id) |> summarize (
    nMujeres = sum(Mujer, na.rm=TRUE),
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
    nOcupado = sum(Ocupado, na.rm = TRUE)
  )

train_personas_hogar <- train_personas |> filter(H_Head==1) |>
  select(-c(Orden, H_Head)) |>
  rename(H_Head_Mujer = Mujer, H_Head_Edad = Edad,H_Head_AfiliadoSalud = AfiliadoSalud, 
         H_Head_RegSalud = RegSalud,H_Head_EducLevel = EducLevel,H_Head_Actividad = Actividad,H_Head_TiempoTr = TiempoTr,
         H_Head_Posicion = Posicion,H_Head_IngHoraE = IngHoraE,H_Head_PrimaMesPasado = PrimaMesPasado,
         H_Head_BonoMesPasado = BonoMesPasado,H_Head_AuxAlimMesPasado = AuxAlimMesPasado,H_Head_AuxTransMesPasado = AuxTransMesPasado,
         H_Head_AuxFamMesPasado = AuxFamMesPasado,H_Head_AuxEduMesPasado = AuxEduMesPasado,H_Head_AlimentosMesPasado = AlimentosMesPasado,
         H_Head_ViviendaMesPasado = ViviendaMesPasado,H_Head_TranspEmpresa = TranspEmpresa,H_Head_IngEspecie = IngEspecie,
         H_Head_Prima = Prima,H_Head_PrimaNavidad = PrimaNavidad,H_Head_PrimaVacaciones = PrimaVacaciones,
         H_Head_Viaticos = Viaticos,H_Head_BonoAnual = BonoAnual,H_Head_HorasT = HorasT,H_Head_TamañoEmp = TamañoEmp,
         H_Head_CotPen = CotPen,H_Head_OtroEmpleo = OtroEmpleo,H_Head_HorasTOtro = HorasTOtro,H_Head_PosicionOtro = PosicionOtro,
         H_Head_MasHoras = MasHoras,H_Head_MasTrabajo = MasTrabajo,H_Head_Disponibilidad = Disponibilidad,
         H_Head_DilCambioEmpl = DilCambioEmpl,H_Head_DispCambioEmpl = DispCambioEmpl,H_Head_BusqTrabajo = BusqTrabajo,
         H_Head_PosUltTrab = PosUltTrab,H_Head_IngTrabDesocu = IngTrabDesocu,H_Head_IngArrPens = IngArrPens,
         H_Head_IngPension = IngPension,H_Head_IngPAoDI = IngPAoDI,H_Head_IngInstDivCes = IngInstDivCes,H_Head_IngPInterior = IngPInterior,
         H_Head_IngPExterior = IngPExterior,H_Head_IngInst = IngInst,H_Head_IngAhorros = IngAhorros,H_Head_IngCes = IngCes,
         H_Head_IngOtros = IngOtros,H_Head_Ocupado = Ocupado
  ) |> 
  left_join(train_personas_nivel_hogar)

#TEST

test_hogares<-read.csv(file.path(dir$raw, "test_hogares.csv"))
test_personas<-read.csv(file.path(dir$raw, "test_personas.csv"))

test_hogares <- test_hogares |>
  mutate(arriendo=ifelse(!is.na(P5140),P5140,ifelse(!is.na(P5100),P5100,P5130)),
         P5100=ifelse(is.na(P5100), 0, P5100),
         P5130=ifelse(is.na(P5130), 0, P5130),
         P5140=ifelse(is.na(P5140), 0, P5140))

test_personas<- test_personas %>% mutate(
  Mujer = ifelse(P6020==2,1,0), 
  Edad = P6040,
  H_Head = ifelse(P6050== 1, 1, 0),
  AfiliadoSalud = ifelse(is.na(P6090)|P6090==9|P6090==2, 0, 1),
  RegSalud = ifelse(is.na(P6100)|P6100==4,0,P6100),
  EducLevel = ifelse(P6210==9,0,P6210),
  Actividad = ifelse(is.na(P6240),6,P6240),
  TiempoTr = ifelse(is.na(P6426), 0, P6426),
  Posicion = ifelse(is.na(P6430)|P6430==9, 0, P6430),
  IngHoraE = ifelse(is.na(P6510)|P6510==9|P6510==2,0,1),
  PrimaMesPasado = ifelse(is.na(P6545)|P6545==9|P6545==2,0,1),
  BonoMesPasado = ifelse(is.na(P6580)|P6580==9|P6580==2,0,1),
  AuxAlimMesPasado = ifelse(is.na(P6585s1)|P6585s1==9|P6585s1==2,0,1),
  AuxTransMesPasado = ifelse(is.na(P6585s2)|P6585s2==9|P6585s2==2,0,1),
  AuxFamMesPasado = ifelse(is.na(P6585s3)|P6585s3==9|P6585s3==2,0,1),
  AuxEduMesPasado = ifelse(is.na(P6585s4)|P6585s4==9|P6585s4==2,0,1),
  AlimentosMesPasado = ifelse(is.na(P6590)|P6590==9|P6590==2,0,1),
  ViviendaMesPasado = ifelse(is.na(P6600)|P6600==9|P6600==2,0,1),
  TranspEmpresa = ifelse(is.na(P6610)|P6610==9|P6610==2,0,1),
  IngEspecie = ifelse(is.na(P6620)|P6620==9|P6620==2,0,1),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0)),
  PrimaNavidad = ifelse(is.na(P6630s2),0,ifelse(P6630s2==1,1,0)),
  PrimaVacaciones = ifelse(is.na(P6630s3),0,ifelse(P6630s3==1,1,0)),
  Viaticos = ifelse(is.na(P6630s4),0,ifelse(P6630s4==1,1,0)),
  BonoAnual = ifelse(is.na(P6630s6),0,ifelse(P6630s6==1,1,0)),
  HorasT = ifelse(is.na(P6800),0,P6800),
  TamañoEmp = ifelse(is.na(P6870),0,P6870),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  OtroEmpleo = ifelse(is.na(P7040),0,ifelse(P7040==1,1,0)),
  HorasTOtro = ifelse(is.na(P7045),0,P7045),
  PosicionOtro = ifelse(is.na(P7050)|P7050==9, 0, P7050),
  MasHoras = ifelse(is.na(P7090),0,ifelse(P7090==1,1,0)),
  MasTrabajo = ifelse(is.na(P7110),0,ifelse(P7110==1,1,0)),
  Disponibilidad = ifelse(is.na(P7120),0,ifelse(P7120==1,1,0)),
  DilCambioEmpl = ifelse(is.na(P7150),0,ifelse(P7150==1,1,0)),
  DispCambioEmpl = ifelse(is.na(P7160),0,ifelse(P7160==1,1,0)),
  BusqTrabajo = ifelse(is.na(P7310),0,ifelse(P7310==1,1,2)),
  PosUltTrab = ifelse(is.na(P7350)|P7350==9, 0, P7350),
  IngTrabDesocu = ifelse(is.na(P7422),0,ifelse(P7422==1,1,0)),
  IngArrPens = ifelse(is.na(P7495),0,ifelse(P7495==1,1,0)),
  IngPension = ifelse(is.na(P7500s2),0,ifelse(P7500s2==1,1,0)),
  IngPAoDI = ifelse(is.na(P7500s3)|P7500s3==9|P7500s3==2,0,1),
  IngInstDivCes = ifelse(is.na(P7505),0,ifelse(P7505==1,1,0)),
  IngPInterior = ifelse(is.na(P7510s1)|P7510s1==9|P7510s1==2,0,1),
  IngPExterior = ifelse(is.na(P7510s2)|P7510s2==9|P7510s2==2,0,1),
  IngInst = ifelse(is.na(P7510s3)|P7510s3==9|P7510s3==2,0,1),
  IngAhorros = ifelse(is.na(P7510s5)|P7510s5==9|P7510s5==2,0,1),
  IngCes = ifelse(is.na(P7510s6)|P7510s6==9|P7510s6==2,0,1),
  IngOtros = ifelse(is.na(P7510s7)|P7510s7==9|P7510s7==2,0,1),
  Ocupado = ifelse(is.na(Oc),0,1)
) |> select(id, Orden, Mujer, Edad, H_Head, AfiliadoSalud, RegSalud, EducLevel, 
            Actividad, TiempoTr, Posicion, IngHoraE, PrimaMesPasado, BonoMesPasado,
            AuxAlimMesPasado, AuxTransMesPasado, AuxFamMesPasado, AuxEduMesPasado, 
            AlimentosMesPasado, ViviendaMesPasado, TranspEmpresa, IngEspecie, 
            Prima, PrimaNavidad, PrimaVacaciones, Viaticos, BonoAnual, HorasT, 
            TamañoEmp, CotPen, OtroEmpleo, HorasTOtro, PosicionOtro, MasHoras, 
            MasTrabajo, Disponibilidad, DilCambioEmpl, DispCambioEmpl, BusqTrabajo, 
            PosUltTrab, IngTrabDesocu, IngArrPens, IngPension, IngPAoDI, IngInstDivCes, 
            IngPInterior, IngPExterior, IngInst, IngAhorros, IngCes, IngOtros, Ocupado)

test_personas_nivel_hogar <- test_personas |>
  group_by(id) |> summarize (
    nMujeres = sum(Mujer, na.rm=TRUE),
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
    nOcupado = sum(Ocupado, na.rm = TRUE)
  )

test_personas_hogar <- test_personas |> filter(H_Head==1) |>
  select(-c(Orden, H_Head)) |>
  rename(H_Head_Mujer = Mujer, H_Head_Edad = Edad,H_Head_AfiliadoSalud = AfiliadoSalud, 
         H_Head_RegSalud = RegSalud,H_Head_EducLevel = EducLevel,H_Head_Actividad = Actividad,H_Head_TiempoTr = TiempoTr,
         H_Head_Posicion = Posicion,H_Head_IngHoraE = IngHoraE,H_Head_PrimaMesPasado = PrimaMesPasado,
         H_Head_BonoMesPasado = BonoMesPasado,H_Head_AuxAlimMesPasado = AuxAlimMesPasado,H_Head_AuxTransMesPasado = AuxTransMesPasado,
         H_Head_AuxFamMesPasado = AuxFamMesPasado,H_Head_AuxEduMesPasado = AuxEduMesPasado,H_Head_AlimentosMesPasado = AlimentosMesPasado,
         H_Head_ViviendaMesPasado = ViviendaMesPasado,H_Head_TranspEmpresa = TranspEmpresa,H_Head_IngEspecie = IngEspecie,
         H_Head_Prima = Prima,H_Head_PrimaNavidad = PrimaNavidad,H_Head_PrimaVacaciones = PrimaVacaciones,
         H_Head_Viaticos = Viaticos,H_Head_BonoAnual = BonoAnual,H_Head_HorasT = HorasT,H_Head_TamañoEmp = TamañoEmp,
         H_Head_CotPen = CotPen,H_Head_OtroEmpleo = OtroEmpleo,H_Head_HorasTOtro = HorasTOtro,H_Head_PosicionOtro = PosicionOtro,
         H_Head_MasHoras = MasHoras,H_Head_MasTrabajo = MasTrabajo,H_Head_Disponibilidad = Disponibilidad,
         H_Head_DilCambioEmpl = DilCambioEmpl,H_Head_DispCambioEmpl = DispCambioEmpl,H_Head_BusqTrabajo = BusqTrabajo,
         H_Head_PosUltTrab = PosUltTrab,H_Head_IngTrabDesocu = IngTrabDesocu,H_Head_IngArrPens = IngArrPens,
         H_Head_IngPension = IngPension,H_Head_IngPAoDI = IngPAoDI,H_Head_IngInstDivCes = IngInstDivCes,H_Head_IngPInterior = IngPInterior,
         H_Head_IngPExterior = IngPExterior,H_Head_IngInst = IngInst,H_Head_IngAhorros = IngAhorros,H_Head_IngCes = IngCes,
         H_Head_IngOtros = IngOtros,H_Head_Ocupado = Ocupado
  ) |> 
  left_join(test_personas_nivel_hogar)

#Data para el modelo

train <- train_hogares |> left_join(train_personas_hogar) 
test <- test_hogares |> left_join(test_personas_hogar) |> 
  mutate(Dominio=factor(Dominio))

rm(train_hogares, train_personas, train_personas_hogar,train_personas_nivel_hogar, test_personas_nivel_hogar, test_hogares, test_personas, test_personas_hogar)
# =========================================================
# 3. Modeling - XGBoost with caret
# =========================================================

set.seed(98063)

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  savePredictions = TRUE
)

# Create tuning grid for xgboost
tuneGrid <- expand.grid(
  nrounds = c(100, 200),
  eta = c(0.1, 0.3),
  max_depth = c(3, 6),
  gamma = 0,
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

model_xgb <- train(
  Pobre ~ nCotPen  + Npersug + nIngArrPens+P5130+nSubsidiado+Nper+nTiempoCompleto+nIngPension+nOcupado+nTrabajadores+arriendo+nEmpleado+P5090+maxTiempoTr+Li,
  data = train,
  method = "xgbTree",
  trControl = ctrl,
  tuneGrid = tuneGrid,
  verbose = FALSE
)

# =========================================================
# 4. Prediction and submission
# =========================================================

predictSample <- test |> 
  mutate(pobre_lab = predict(model_xgb, newdata = test, type = "raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre)

depth_str <- paste0("depth_", model_xgb$bestTune$max_depth)
eta_str <- paste0("eta_", model_xgb$bestTune$eta)
rounds_str <- paste0("rounds_", model_xgb$bestTune$nrounds)

name <- paste0("XGB_", depth_str, "_", rounds_str,"_",eta_str, ".csv")

write.csv(predictSample, file.path(dir$model, name), row.names = FALSE)