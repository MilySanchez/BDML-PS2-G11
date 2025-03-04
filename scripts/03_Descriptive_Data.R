##########################################################
# Title: Descriptive Data
# Description: This script creates descriptive statistics tables, and 
# a Correlation graph of all existing variables after Data Cleaning.
# This must stored a latex table and an image of the corr graph.
#
# Date: 09/02/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 0. Workspace configuration ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

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

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 1. Descriptive Data ===========================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

#Load data
db_geih <- read.csv(file.path(dir$processed,'data_cleanGEIH.csv')) %>% select(-c(...1))

#Get descriptive statistic table of all variables after cleaning
skim_result <- skim(db_geih |>select(-c(dominio,directorio,secuencia_p,orden)))

#Simplify the table
skim_result <- skim_result |>
  rename_with(~ gsub("numeric\\.", "", .x)) |>
  rename(variable = skim_variable, c_rate = complete_rate, type = skim_type) |>
  mutate(across(where(is.numeric), ~ format(.x, scientific = TRUE))) |>
  arrange(variable)

#Save the table of annex
stargazer(as.data.frame(skim_result), summary=F, type="text",out = file.path(dir$views, "P2_Data",'data_description_total.txt'))

#Get the summary table, the one that goes in the main document
skim_result_summary <- skim(db_geih |>select(-c(dominio,directorio,secuencia_p,orden)) |>
  select(c(p6870,p6050,relab,estrato1,p6240,regSalud,cotPension,p6050,p6090,p7495,p7505,p7040, 
           p7090,cuentaPropia,microEmpresa,sex,formal,age,totalHoursWorked,ingtot,mes,p7500s1a1,
           p7500s2a1,p7500s3a1,p7510s1a1,p7510s2a1,p7510s3a1,p7510s5a1,p7510s6a1,p7510s7a1)))

#Simplify the main table
skim_result_summary <- skim_result_summary |>
  select(-skim_type, -n_missing, -complete_rate, -numeric.hist) |>
  rename_with(~ gsub("numeric\\.", "", .x)) |>
  rename(variable = skim_variable) |>
  arrange(variable)

#save the descriptive table in a txt file
stargazer(as.data.frame(skim_result_summary), summary=F, type="text",out = file.path(dir$views,'data_description.txt'))

#Create a new df containing just discrete data
db_geih_discrete <- db_geih |> select(p6870, p6050, relab, estrato1, p6240, regSalud,
                                      cotPension, p6050, p6090,p7495, p7505,p7040, 
                                      p7090, cuentaPropia, microEmpresa, sex, formal)

#Get percentages of every possible category for all discrete data
porcentajes <- list()

for (col in names(db_geih_discrete)) {
  freq <- table(db_geih_discrete[[col]])
  props <- prop.table(freq)
  porcentajes[[col]] <- props
}

#Extract just percentages and create a df
tabla_porcentajes <- map_df(names(porcentajes), function(var) {
 
  prop_vector <- porcentajes[[var]]
  
  data.frame(
    variable = var,
    categoria = names(prop_vector),
    porcentaje = as.vector(prop_vector)
  )
})

#Next lines until saving the result in a txt file, create a organized table with percentages 

#All possible values for those discrete data take a minimum of 0 and a maximum of 9
categorias <- 0:9

#Here probabilities are going to be saved
prob_list <- list()

#This loop is going to fill all entries for every possible probability
for (var in names(porcentajes)) {
  prop_vector <- porcentajes[[var]]
  probs <- rep(NA, length(categorias))
  categorias_presentes <- as.numeric(names(prop_vector))
  for (i in seq_along(categorias_presentes)) {
    cat_val <- categorias_presentes[i]
    if (cat_val %in% categorias) {
      pos <- cat_val - min(categorias) + 1
      probs[pos] <- round(prop_vector[i], 2)
    }
  }
  prob_list[[var]] <- probs
}

tabla_porcentajes <- as.data.frame(prob_list)
tabla_porcentajes <- cbind(valor = categorias, tabla_porcentajes)

#Replace NA for a blank space
tabla_porcentajes[is.na(tabla_porcentajes)] <- ""

#Divide the table into 2 different ones to organize it better
tabla_porcentajes_1 = tabla_porcentajes |> select(valor, p6870, p6050, relab, estrato1, p6240, regSalud, cotPension, p6090)

tabla_porcentajes_2 = tabla_porcentajes |> select(valor, p7495, p7505, p7040, p7090, cuentaPropia, microEmpresa, sex, formal) |>
  filter(valor <=2)

#save the result tables in a txt file
stargazer(as.data.frame(tabla_porcentajes_1), summary = FALSE, type="text", out=file.path(dir$views, "P2_Data",'discrete_description_1.txt'))
stargazer(as.data.frame(tabla_porcentajes_2), summary = FALSE, type="text", out=file.path(dir$views, "P2_Data",'discrete_description_2.txt'))


#create a correlation graph for each variable
corr_graph <- db_geih |>select(-c(dominio,directorio,secuencia_p,orden)) |>
  select_if(is.numeric) |> 
  cor(use = "pairwise") |>
  round(1) 

library(ggcorrplot)

#Corr graph and variable names
p <- ggcorrplot(corr_graph, 
                type = "lower", 
                lab = TRUE, 
                lab_size = 3,              
                colors = c("blue", "white", "red"),  
                title = "Matriz de Correlaciones",   
                ggtheme = theme_minimal(),         
                show.diag = FALSE               
)

#Label personalization
p <- p + scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                              limits = c(-1, 1), breaks = c(-1, 0, 1), labels = c("-1", "0", "1"))

#Adjusting labels for better readability
p <- p + theme(
  axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),  
  axis.text.y = element_text(size = 8),
  plot.margin = margin(10, 10, 10, 10)
)


ggsave(filename =file.path(dir$views,'corr_graph.png'), plot = p, width = 10, height = 10, dpi = 300)

