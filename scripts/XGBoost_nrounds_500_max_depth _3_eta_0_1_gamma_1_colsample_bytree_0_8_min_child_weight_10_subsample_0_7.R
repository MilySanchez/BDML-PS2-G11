##########################################################
# Title: XGBoost_nrounds_500_max_depth _3_eta_0_1_gamma_1_colsample_bytree_0_8_min_child_weight_10_subsample_0_7
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 0. Workspace configuration ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# Clear workspace


rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$raw <- file.path(dir$root, "store", "raw")
dir$processed <- file.path(dir$root, "store", "processed")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
dir$results <- file.path(dir$root, "results")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# Load train and test files

train <- read.csv(file.path(dir$processed, "train.csv")) 

test <- read.csv(file.path(dir$processed, "test.csv")) 

# Downsapling
train$Pobre <- as.factor(train$Pobre)

train <- downSample(x = train |> select(-Pobre),y = train$Pobre,yname = "Pobre")

# BOOSTING

set.seed(123)

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...),
    caret::defaultSummary(...)
  )
}


ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE, 
                    verbose=FALSE,
                    savePredictions = T)

grid_xgboost <- expand.grid(
  nrounds = c(250, 500),
  max_depth = c(2, 3),
  eta = c(0.1, 0.05), 
  gamma = c(0, 1),
  min_child_weight = c(10), 
  colsample_bytree = c(0.6, 0.8),
  subsample = c(0.7)
)


Xgboost_tree <- train(Pobre ~ Cuartos + CuartosDormir + TenenciaVivienda + CuotaAmortizacion + ArriendoEst + ArriendoEfec + Npersonas + NpersonasUG + 
                        Lindigencia + Lpobreza + Fex_c + Depto + Fex_dpto + arriendo + H_Head_Mujer + H_Head_Edad + 
                        H_Head_AfiliadoSalud + H_Head_RegSalud + H_Head_EducLevel + H_Head_Actividad + H_Head_TiempoTr + 
                        H_Head_Posicion + H_Head_IngHoraE + H_Head_PrimaMesPasado + H_Head_BonoMesPasado + 
                        H_Head_AuxAlimMesPasado + H_Head_AuxTransMesPasado + H_Head_AuxFamMesPasado + 
                        H_Head_AuxEduMesPasado + H_Head_AlimentosMesPasado + H_Head_ViviendaMesPasado + 
                        H_Head_TranspEmpresa + H_Head_IngEspecie + H_Head_Prima + H_Head_PrimaNavidad + 
                        H_Head_PrimaVacaciones + H_Head_Viaticos + H_Head_BonoAnual + H_Head_HorasT + 
                        H_Head_TamañoEmp + H_Head_CotPen + H_Head_OtroEmpleo + H_Head_HorasTOtro + 
                        H_Head_PosicionOtro + H_Head_MasHoras + H_Head_MasTrabajo + H_Head_Disponibilidad + 
                        H_Head_DilCambioEmpl + H_Head_DispCambioEmpl + H_Head_BusqTrabajo + H_Head_PosUltTrab + 
                        H_Head_IngTrabDesocu + H_Head_IngArrPens + H_Head_IngPension + H_Head_IngPAoDI + 
                        H_Head_IngInstDivCes + H_Head_IngPInterior + H_Head_IngPExterior + H_Head_IngInst + 
                        H_Head_IngAhorros + H_Head_IngCes + H_Head_IngOtros + H_Head_Ocupado + nMujeres + 
                        nAfiliados + nSubsidiado + maxEducLevel + nTrabajadores + maxTiempoTr + nEmpleado + 
                        nJefe + nCuentaPropia + nMenorRango + nIngHoraT + nPrimaMesPasado + nBonoMesPasado + 
                        nAuxAlimMesPasado + nAuxTransMesPasado + nAuxFamMesPasado + nAuxEduMesPasado + 
                        nAlimentosMesPasado + nViviendaMesPasado + nTranspEmpresa + nIngEspecie + nPrima + 
                        nPrimaNavidad + nPrimaVacaciones + nViaticos + nBonoAnual + nTiempoCompleto + maxTamañoEmp + 
                        nCotPen + nOtroEmpleo + nMasHoras + nMasTrabajo + nDisponibilidad + nDilCambioEmpl + 
                        nDispCambioEmpl + nBusqTrabajo + nIngTrabDesocu + nIngArrPens + nIngPension + nIngPAoDI + 
                        nIngInstDivCes + nIngPInterior + nIngPExterior + nIngInst + nIngAhorros + nIngCes + nIngOtros + nOcupado,
                      data = train, 
                      method = "xgbTree", 
                      trControl = ctrl,
                      tuneGrid=grid_xgboost,
                      metric = "F1",
                      verbosity = 0
)   

# Prediction

predictSample <- test |> 
  mutate(pobre_lab=predict(Xgboost_tree, newdata=test, type="raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)

# Final file

nrounds <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$nrounds))

max_depth <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$max_depth))

eta <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$eta))

gamma <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$gamma))

colsample_bytree <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$colsample_bytree))

min_child_weight <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$min_child_weight))

subsample <- gsub("\\.","_", as.character(Xgboost_tree$bestTune$subsample))

name <- paste0(
  "XGBoost_nrounds_", nrounds,
  "_max_depth _", max_depth,
  "_eta_", eta,
  "_gamma_", gamma,
  "_colsample_bytree_", colsample_bytree,
  "_min_child_weight_", min_child_weight,
  "_subsample_", subsample,
  ".csv")


write.csv(predictSample, file.path(dir$views, "Models",name), row.names=F)