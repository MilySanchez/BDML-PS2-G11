##########################################################
# Title: EN_lambda_0_001_alpha_0_1 - V2
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

train <- read.csv(file.path(dir$processed, "train.csv")) |> select(c(Clase, Dominio, Cuartos, CuartosDormir, TenenciaVivienda, 
                                                                     Npersonas, NpersonasUG, Lindigencia, Lpobreza, Fex_c, Depto, Fex_dpto, 
                                                                     Pobre, arriendo, H_Head_Mujer, H_Head_EducLevel, 
                                                                     H_Head_Ocupado, H_Head_CotPen, H_Head_Prima, 
                                                                     nOcupado, maxEducLevel, nCotPen, nPrima))

test <- read.csv(file.path(dir$processed, "test.csv")) |> select(c(Clase, Dominio, Cuartos, CuartosDormir, TenenciaVivienda, 
                                                                      Npersonas, NpersonasUG, Lindigencia, Lpobreza, Fex_c, Depto, Fex_dpto, 
                                                                      arriendo, H_Head_Mujer, H_Head_EducLevel, 
                                                                      H_Head_Ocupado, H_Head_CotPen, H_Head_Prima, 
                                                                      nOcupado, maxEducLevel, nCotPen, nPrima))

# Model
ctrl<- trainControl(
  method="cv",
  number=10,
  classProbs=TRUE,
  savePredictions = T
)
set.seed(098063)

model1 <- train(
  Pobre~.,
  data=train,
  metric="F1",
  method="glmnet",
  trControl=ctrl,
  tuneGrid=expand.grid(
    alpha=seq(0,1,by=0.1),
    lambda=10^seq(10,-3,length=10)
  )
)

# Prediction

predictSample <- test |> 
  mutate(pobre_lab=predict(model1, newdata=test, type="raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)

# Final file

lambda_str <- gsub(
  "\\.","_",
  as.character(round(model1$bestTune$lambda,4))
)

alpha_str <- gsub("\\.","_", as.character(model1$bestTune$alpha))

name <- paste0(
  "EN_lambda_", lambda_str,
  "_alpha_", alpha_str,
  ".csv")


write.csv(predictSample, file.path(dir$views, "Models",name), row.names=F)
