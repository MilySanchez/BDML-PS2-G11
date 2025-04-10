##########################################################
# Title: Random Forest training, prediction and importance
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
# 1. Load inputs
# =========================================================

train_hogares <- read.csv(file.path(dir$raw, "train_hogares.csv"))
train_personas <- read.csv(file.path(dir$raw, "train_personas.csv"))
test_hogares <- read.csv(file.path(dir$raw, "test_hogares.csv"))
test_personas <- read.csv(file.path(dir$raw, "test_personas.csv"))

# =========================================================
# 2. Data transformation
# =========================================================

train_personas <- train_personas |> 
  select(id, Orden, Clase, Dominio, P6020, P6040, P6050, P6090, P6100, P6210, 
         P6210s1, P6240, Oficio, P6426, P6430, P6510, P6545, P6580, P6585s1, 
         P6585s2, P6585s3, P6585s4, P6590, P6600, P6610, P6620, P6630s1, 
         P6630s2, P6630s3, P6630s4, P6630s6, P6800, P6870, P6920, P7040, 
         P7045, P7050, P7090, P7110, P7120, P7150, P7160, P7310, P7350, 
         P7422, P7472, P7495, P7500s2, P7500s3, P7505, P7510s1, P7510s2, 
         P7510s3, P7510s5, P7510s6, P7510s7, Pet, Oc, Des, Ina, Fex_c, 
         Depto, Fex_dpto) |> 
  mutate(
    mujer = ifelse(P6020 == 2, 1, 0), 
    H_Head = ifelse(P6050 == 1, 1, 0),
    EducLevel = ifelse(P6210 == 9, 0, P6210),
    ocupado = ifelse(is.na(Oc), 0, 1)
  ) |> 
  select(id, Orden, mujer, H_Head, EducLevel, ocupado)

train_personas_nivel_hogar <- train_personas |>
  group_by(id) |> 
  summarize(
    nocupado = sum(ocupado, na.rm = TRUE),
    maxEducLevel = max(EducLevel, na.rm = TRUE)
  )

train_personas_hogar <- train_personas |> 
  filter(H_Head == 1) |>
  select(id, mujer, EducLevel, ocupado) |>
  rename(
    H_Head_mujer = mujer,
    H_Head_Edu = EducLevel,
    H_Head_ocupado = ocupado
  ) |> 
  left_join(train_personas_nivel_hogar, by = "id")

train_hogares <- train_hogares |> 
  select(id, Clase, Dominio, P5000, P5010, P5090, P5100, P5130, P5140, Nper, 
         Npersug, Li, Lp, Fex_c, Depto, Fex_dpto, Pobre) |> 
  mutate(
    Pobre = factor(Pobre, levels = c(0, 1), labels = c("No", "Yes")),
    Dominio = factor(Dominio)
  )

# Up-sample to balance classes
train_hogares <- upSample(
  x = train_hogares |> select(-Pobre),
  y = train_hogares$Pobre,
  yname = "Pobre"
)

# Join datasets for modeling
train <- train_hogares |> 
  left_join(train_personas_hogar, by = "id") |> 
  select(-id) |> 
  mutate(
    P5100 = ifelse(is.na(P5100), 0, P5100),
    P5130 = ifelse(is.na(P5130), 0, P5130),
    P5140 = ifelse(is.na(P5140), 0, P5140)
  )


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


test <- test_hogares |> left_join(test_personas_hogar) |> 
  mutate(P5100=ifelse(is.na(P5100),0,P5100),P5130=ifelse(is.na(P5130),0,P5130),P5140=ifelse(is.na(P5140),0,P5140)) |> 
  mutate(Dominio=factor(Dominio))


# =========================================================
# 3. Modeling - Tuning Random Forest Hyperparameters with ranger
# =========================================================

set.seed(98063)

ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  savePredictions = TRUE
)

n_features <- ncol(train) - 1  

tuneGrid <- expand.grid(
  mtry = c(2, floor(sqrt(n_features)), floor(n_features/2)),
  splitrule = "gini",
  min.node.size = c(1, 5, 10)
)

model_ranger <- train(
  Pobre ~ .,
  data = train,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = tuneGrid,
  importance = "impurity",
  num.trees = 500  
)

# =========================================================
# 4. Variable Importance Plot
# =========================================================

importance_ranger <- varImp(model_ranger, scale = TRUE)
importance_df <- as.data.frame(importance_ranger$importance)
importance_df$Variable <- rownames(importance_df)

top_vars <- importance_df |>
  arrange(desc(Overall)) |>
  filter(Variable != "Fex_c" & Variable != "Fex_dpto") |>
  head(20)

ggplot(top_vars, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 20 Variable Importances (Ranger - Random Forest)",
    x = "Variables",
    y = "Importance (Impurity)"
  ) 

# =========================================================
# 5. Prediction and submission
# =========================================================

predictSample <- test |> 
  mutate(pobre_lab = predict(model_ranger, newdata = test, type = "raw")) |>
  select(id, pobre_lab)

predictSample <- predictSample |> 
  mutate(pobre = ifelse(pobre_lab == "Yes", 1, 0)) |>
  select(id, pobre)

mtry_str <- paste0("mtry_", model_ranger$bestTune$mtry)
node_str <- paste0("nodesize_", model_ranger$bestTune$min.node.size)

name <- paste0("RF_", mtry_str,"_", node_str, ".csv")

write.csv(predictSample, file.path(dir$model,name), row.names = FALSE)