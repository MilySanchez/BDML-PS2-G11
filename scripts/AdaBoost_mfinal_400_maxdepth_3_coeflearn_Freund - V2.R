##########################################################
# Title: AdaBoost_mfinal_400_maxdepth_3_coeflearn_Freund - V2
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


adaboost_tree <- train(Pobre ~ nCotPen + NpersonasUG + nIngArrPens + ArriendoEst + nSubsidiado + Npersonas +nTiempoCompleto,
                       data = train, 
                       method = "AdaBoost.M1", 
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
