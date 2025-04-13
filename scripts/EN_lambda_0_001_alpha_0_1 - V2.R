# Este modelo integra las anteriores variables de Pensión y Prima, junto con
# una nueva variable de arriendo, que toma valor dependiendo de si la persona
# paga arriendo, amortización o tiene casa propia. El caso en el que tiene casa
# propia la variable toma valor de 0.
# Puntuación 0.6050
# Clear workspace

rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$stores <- file.path(dir$root, "stores", "raw")
dir$processed <- file.path(dir$root, "stores", "processed")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
dir$results <- file.path(dir$root, "results")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

train_hogares<-read.csv(file.path(dir$stores, "train_hogares.csv"))
train_personas<-read.csv(file.path(dir$stores, "train_personas.csv"))

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
  mutate(arriendo=ifelse(!is.na(P5140),P5140,ifelse(!is.na(P5100),P5100,0)))


train_personas<- train_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), 
  H_Head = ifelse(P6050== 1, 1, 0),
  EducLevel = ifelse(P6210==9,0,P6210),
  ocupado = ifelse(is.na(Oc),0,1),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0))
) |> select(id, Orden, mujer, H_Head, EducLevel, ocupado, CotPen, Prima)



train_personas_nivel_hogar <- train_personas |>
  group_by(id) |> summarize (
    nocupado=sum(ocupado, na.rm=T),
    maxEducLevel=max(EducLevel,na.rm=TRUE),
    ncotpen=sum(CotPen, na.rm=TRUE),
    nprima=sum(Prima, na.rm=TRUE)
  )

train_personas_hogar <- train_personas |> filter(H_Head==1) |>
  select(id, mujer, EducLevel, ocupado, CotPen, Prima) |>
  rename(H_Head_mujer=mujer, H_Head_Edu=EducLevel, H_Head_ocupado = ocupado,
         H_Head_cotpen=CotPen, H_Head_prima=Prima) |> 
  left_join(train_personas_nivel_hogar)

#TEST

test_hogares<-read.csv(file.path(dir$stores, "test_hogares.csv"))
test_personas<-read.csv(file.path(dir$stores, "test_personas.csv"))

test_hogares <- test_hogares |>
  mutate(arriendo=ifelse(!is.na(P5140),P5140,ifelse(!is.na(P5100),P5100,0)))

test_personas<- test_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), 
  H_Head = ifelse(P6050== 1, 1, 0),
  EducLevel = ifelse(P6210==6,1,0),
  ocupado = ifelse(is.na(Oc),0,1),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0))
) |> select(id, Orden, mujer, H_Head, EducLevel, ocupado, CotPen, Prima)

test_personas_nivel_hogar <- test_personas |>
  group_by(id) |> summarize (
    nocupado=sum(ocupado, na.rm=T),
    maxEducLevel=max(EducLevel,na.rm=TRUE),
    ncotpen=sum(CotPen, na.rm=TRUE),
    nprima=sum(Prima, na.rm=TRUE)
  )

test_personas_hogar <- test_personas |> filter(H_Head==1) |>
  select(id, mujer, EducLevel, ocupado, CotPen, Prima) |>
  rename(H_Head_mujer=mujer, H_Head_Edu=EducLevel, H_Head_ocupado = ocupado,
         H_Head_cotpen=CotPen, H_Head_prima=Prima) |> 
  left_join(test_personas_nivel_hogar)

#Data para el modelo

train <- train_hogares |> left_join(train_personas_hogar) |> select(-c(id, P5100, P5130, P5140))
test <- test_hogares |> left_join(test_personas_hogar) |> 
  mutate(Dominio=factor(Dominio)) |> select(-c(P5100, P5130, P5140))


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
    alpha=seq(0,1,by=0.1),
    lambda=10^seq(10,-3,length=10)
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
