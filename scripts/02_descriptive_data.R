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

# Transform variables to factor

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

skimtrain <- skimtrain %>%
  rename_with(~ gsub("numeric\\.", "", .x)) %>% 
  rename(variable = skim_variable, c_rate = complete_rate, type = skim_type) %>% 
  mutate(across(where(is.numeric), ~ format(.x, scientific = TRUE))) %>% 
  arrange(variable)

# Export results to text files

stargazer(as.data.frame(skimtrain), summary = F, type = 'text',
          out = file.path(dir$views, 'data_description_total.txt'))

# Generate statistics for categorical variables

skim_summary_factor <- skim(train %>%
                       select(c(Pobre, TenenciaVivienda, H_Head_EducLevel,
                                maxEducLevel, H_Head_Posicion,
                                H_Head_ViviendaMesPasado, Depto)))

skim_summary_factor <- skim_summary_factor %>% 
  select(-factor.ordered, -skim_type, -n_missing) %>% 
  rename_with(~ gsub("factor\\.", "", .x)) %>% 
  rename(variable = skim_variable) %>% 
  arrange(variable)

stargazer(as.data.frame(skim_summary_factor),
          summary=F,
          type="text",
          out = file.path(dir$views,'description_summary_factor.txt'))

# Generate statistics for numeric vars
skim_summary_num <- skim(train %>%
                              select(c(nCotPen, NpersonasUG, nIngArrPens,
                                       arriendo, nSubsidiado, nTiempoCompleto,
                                       nIngPension, nOcupado, nTrabajadores,
                                       nEmpleado, maxTiempoTr, nPrima,
                                       nIngTrabDesocu, nAfiliados, ArriendoEst,
                                       ArriendoEfec, nIngPExterior,
                                       nIngPInterior, nAlimentosMesPasado,
                                       H_Head_HorasT)))

skim_summary_num <- skim_summary_num %>% 
  select(-numeric.hist, -skim_type, -n_missing) %>% 
  rename_with(~ gsub("numeric\\.", "", .x)) %>% 
  rename(variable = skim_variable) %>% 
  arrange(variable)

stargazer(as.data.frame(skim_summary_num),
          summary=F,
          type="text",
          out = file.path(dir$views,'description_summary_num.txt'))

# Create frequency table of Pobre

table(train$Pobre)
prop_pobre <- prop.table(table(train$Pobre))*100

# Convert to df to plot

df_pobre <- data.frame(Pobre = names(prop_pobre),
                       Porcentaje = as.numeric(prop_pobre))

pobre_plot <- ggplot(df_pobre, aes(x = Pobre, y = Porcentaje, fill = Pobre)) +
  geom_col(width = 0.6, alpha = 0.8) +
  labs(x = 'Pobre',
       y = 'Porcentaje (%)',
       caption = paste('Total de hogares:', nrow(train))) +
  theme_minimal() + 
  theme(legend.position = 'none',
        plot.caption = element_text(face = 'italic'),
        axis.text = element_text(size = 10),
        panel.grid.major.x = element_blank())

ggsave(filename = file.path(dir$views, 'distribucion_pobreza.png'),
       plot = pobre_plot,
       width = 7,
       height = 5,
       dpi = 300)

# Summarize factor vars

train %>% 
  select(where(is.factor)) %>% 
  summary()

