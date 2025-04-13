##########################################################
# Title: Exploratory Analysis and Descriptive Statistics
# Description: This script creates descriptive statistics tables, and 
# a Correlation graph of all existing variables after Data Cleaning.
# This must stored a latex table and an image of the corr graph.
#
# Date: 10/04/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 0. Workspace configuration ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "store", "processed")
dir$raw <- file.path(dir$root, "store", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 1. Descriptive Data ===========================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# Load data

train <- read.csv(file.path(dir$processed, "train.csv"))

# Transform variable types

train <- train %>% 
  mutate(across(c(Pobre, Dominio, Clase, TenenciaVivienda, Depto,
                  H_Head_Mujer, H_Head_AfiliadoSalud,
                  H_Head_RegSalud, H_Head_EducLevel,
                  H_Head_Actividad, H_Head_Posicion,
                  H_Head_IngHoraE, H_Head_PrimaMesPasado,
                  H_Head_BonoMesPasado, H_Head_AuxAlimMesPasado,
                  H_Head_AuxTransMesPasado, H_Head_AuxFamMesPasado,
                  H_Head_AuxEduMesPasado, H_Head_AlimentosMesPasado,
                  H_Head_ViviendaMesPasado, H_Head_TranspEmpresa,
                  H_Head_IngEspecie, H_Head_Prima,
                  H_Head_PrimaNavidad, H_Head_PrimaVacaciones,
                  H_Head_Viaticos, H_Head_BonoAnual, H_Head_CotPen,
                  H_Head_OtroEmpleo, H_Head_PosicionOtro,
                  H_Head_MasHoras, H_Head_MasTrabajo, H_Head_Disponibilidad,
                  H_Head_DilCambioEmpl, H_Head_DispCambioEmpl,
                  H_Head_BusqTrabajo, H_Head_PosUltTrab,
                  H_Head_IngTrabDesocu, H_Head_IngArrPens,
                  H_Head_IngPension, H_Head_IngPAoDI, H_Head_IngInstDivCes,
                  H_Head_IngPInterior, H_Head_IngPExterior, H_Head_IngInst,
                  H_Head_IngAhorros, H_Head_IngCes, H_Head_IngOtros,
                  H_Head_Ocupado, maxEducLevel),
                as.factor))

# Initial exploration

skimtrain <- skim(train)

# Descriptive statistics for numeric variables

train %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    media = mean,
    sd = sd,
    min = min,
    max = max,
    q25 = ~quantile(., 0.25),
    q75 = ~quantile(., 0.75),
    skewness = ~e1071::skewness(.)),
    na.rm = TRUE))

# Descriptive statistics for categorical variables

table(train$Pobre)
prop.table(table(train$Pobre))*100

train %>% 
  select(where(is.factor)) %>% 
  summary()

# Distribution of Pobre

ggplot(train, aes(x = factor(Pobre))) +
  geom_bar(fill = c('blue', 'red')) +
  labs(title = 'Distribución de hogares pobres vs no pobres',
       x = 'Pobre (1) vs No Pobre (0)',
       y = 'Frecuencia')

# Correlation matrix for numeric variables

cor_matrix <- train %>% 
  select(where(is.numeric)) %>% 
  cor(use = 'complete.obs')

corrplot(cor_matrix, method = 'shade')

#  Bivariate analysis

# train %>% 
#   group_by(Pobre) %>% 
#   summarise()

table(train$Dominio, train$Pobre) %>% 
  chisq.test()

