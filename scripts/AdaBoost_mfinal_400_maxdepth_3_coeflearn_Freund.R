##########################################################
# Title: AdaBoost_mfinal_400_maxdepth_3_coeflearn_Freund
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

adagrid <- expand.grid(
  mfinal = c(50, 200 ,400),
  maxdepth = c(1,3),
  coeflearn = c('Breiman','Freund'))


adaboost_tree <- train(Pobre ~ Cuartos + CuartosDormir + TenenciaVivienda + CuotaAmortizacion + ArriendoEst + ArriendoEfec + Npersonas + NpersonasUG + 
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
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "F1",
                       tuneGrid=adagrid
)

# Prediction

predictSample <- test |> 
  mutate(pobre_lab=predict(adaboost_tree, newdata=test, type="raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)

# Final file

mfinal <- gsub("\\.","_", as.character(adaboost_tree$bestTune$mfinal))

maxdepth <- gsub("\\.","_", as.character(adaboost_tree$bestTune$maxdepth))

coeflearn <- gsub("\\.","_", as.character(adaboost_tree$bestTune$coeflearn))

name <- paste0(
  "AdaBoost_mfinal_", mfinal,
  "_maxdepth_", maxdepth ,
  "_coeflearn_", coeflearn,
  ".csv")


write.csv(predictSample, file.path(dir$views, "Models",name), row.names=F)

# Getting importance
importancia <- varImp(adaboost_tree)

# Turning into a data.frame
importancia_df <- importancia$importance
importancia_df$Variable <- rownames(importancia_df) 

# Order columns
importancia_df <- importancia_df[, c("Variable", setdiff(names(importancia_df), "Variable"))]

# Keep CSV
write.csv(importancia_df, "importancia_variables_adaboost.csv", row.names = FALSE)

