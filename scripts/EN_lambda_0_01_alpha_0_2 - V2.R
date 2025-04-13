# Este modelo integra las anteriores variables de Pensión y Prima, junto con
# una nueva variable de arriendo, que toma valor dependiendo de si la persona
# paga arriendo, amortización o el valor hipotético. El caso en el que no pagan
# arriendo por tener casa propia igual se toma como si el caso hipotético fuese
# lo que pagasen. Puntuación 0.4665
# Clear workspace

# variables a tener en cuenta P6040 edad, P6090 salud, P6100 régimen de salud, P6426 tiempo en la empresa, 
# P6430 posición en el trabajo, P6585s2 recibió auxilio de transorte, P6800 horas que trabaja 


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

train <- read.csv(file.path(dir$processed, "train.csv")) |> select(c(Clase, Dominio, Cuartos, CuartosDormir, TenenciaVivienda, 
                                                                     Npersonas, NpersonasUG, Lindigencia, Lpobreza, Fex_c, Depto, Fex_dpto, 
                                                                     Pobre, arriendo, H_Head_Mujer, H_Head_EducLevel, 
                                                                     H_Head_Ocupado, H_Head_CotPen, H_Head_Prima, H_Head_RegSalud,H_Head_Edad, H_Head_HorasT,
                                                                     nOcupado, maxEducLevel, nCotPen, nPrima, nSubsidiado))

test <- read.csv(file.path(dir$processed, "test.csv")) |> select(c(Clase, Dominio, Cuartos, CuartosDormir, TenenciaVivienda, 
                                                                   Npersonas, NpersonasUG, Lindigencia, Lpobreza, Fex_c, Depto, Fex_dpto, 
                                                                   arriendo, H_Head_Mujer, H_Head_EducLevel, 
                                                                   H_Head_Ocupado, H_Head_CotPen, H_Head_Prima, H_Head_RegSalud,H_Head_Edad, H_Head_HorasT,
                                                                   nOcupado, maxEducLevel, nCotPen, nPrima, nSubsidiado))


#MODELO

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
    alpha=seq(0,1,by=0.2),
    lambda=10^seq(10,-2,length=10)
  )
)
#Predicción

predictSample <- test |> 
  mutate(pobre_lab=predict(model1, newdata=test, type="raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> mutate(pobre=ifelse(pobre_lab=="Yes",1,0)) |>
  select(id, pobre)

#Archivo final

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
