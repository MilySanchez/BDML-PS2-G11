require("pacman")
p_load(tidyverse, # tidy-data
       glmnet, # To implement regularization algorithms. 
       caret, # creating predictive models
       Mlmetrics, # To calculate metrics
       Metrics
)

rm(list=ls())

train_hogares<-read.csv("Mily/BDML/T2/train_hogares.csv")
train_personas<-read.csv("Mily/BDML/T2/train_personas.csv")

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
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")),Dominio=factor(Dominio))

#train_hogares2 <- train_hogares |> rbind(train_hogares |> filter(Pobre==1)) |> 
 # rbind(train_hogares |> filter(Pobre==1)) 

train_hogares <- upSample(x = train_hogares |> select(-Pobre), y = train_hogares$Pobre, yname="Pobre")

id_hogares_pobres = train_hogares |> filter(Pobre==1) |> select(id)

personas_pobres = train_personas |> filter(id %in% id_hogares_pobres$id)

train_personas<- train_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), 
  H_Head = ifelse(P6050== 1, 1, 0),
  EducLevel = ifelse(P6210==9,0,P6210),
  ocupado = ifelse(is.na(Oc),0,1),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0))
) |> select(id, Orden, mujer, H_Head, EducLevel, ocupado)



train_personas_nivel_hogar <- train_personas |>
  group_by(id) |> summarize (
    nocupado=sum(ocupado, na.rm=T),
    maxEducLevel=max(EducLevel,na.rm=TRUE)
  )

train_personas_hogar <- train_personas |> filter(H_Head==1) |>
  select(id, mujer, EducLevel, ocupado) |>
  rename(H_Head_mujer=mujer, H_Head_Edu=EducLevel, H_Head_ocupado = ocupado) |> 
  left_join(train_personas_nivel_hogar)

#TEST

test_hogares<-read.csv("Mily/BDML/T2/test_hogares.csv")
test_personas<-read.csv("Mily/BDML/T2/test_personas.csv")

test_personas<- test_personas %>% mutate(
  mujer = ifelse(P6020==2,1,0), 
  H_Head = ifelse(P6050== 1, 1, 0),
  EducLevel = ifelse(P6210==6,1,0),
  ocupado = ifelse(is.na(Oc),0,1),
  CotPen = ifelse(is.na(P6920),0,ifelse(P6920==1,1,0)),
  Prima = ifelse(is.na(P6630s1),0,ifelse(P6630s1==1,1,0))
) |> select(id, Orden, mujer, H_Head, EducLevel, ocupado)

test_personas_nivel_hogar <- test_personas |>
  group_by(id) |> summarize (
    nocupado=sum(ocupado, na.rm=T),
    maxEducLevel=max(EducLevel,na.rm=TRUE)
  )

test_personas_hogar <- test_personas |> filter(H_Head==1) |>
  select(id, mujer, EducLevel, ocupado) |>
  rename(H_Head_mujer=mujer, H_Head_Edu=EducLevel, H_Head_ocupado = ocupado) |> 
  left_join(test_personas_nivel_hogar)

#Data para el modelo

train <- train_hogares |> left_join(train_personas_hogar) |> select(-c(id)) |> 
  mutate(P5100=ifelse(is.na(P5100),0,P5100),P5130=ifelse(is.na(P5130),0,P5130),P5140=ifelse(is.na(P5140),0,P5140))

test <- test_hogares |> left_join(test_personas_hogar) |> 
  mutate(P5100=ifelse(is.na(P5100),0,P5100),P5130=ifelse(is.na(P5130),0,P5130),P5140=ifelse(is.na(P5140),0,P5140)) |> 
  mutate(Dominio=factor(Dominio))


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

write.csv(predictSample, name, row.names=F)

