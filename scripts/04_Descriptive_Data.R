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

#load data
db_geih <- read.csv(file.path(dir$processed,'data_cleanGEIH.csv'))

#get descriptive statistic table of all variables after cleaning
skim_result <- skim(db_geih |>select(-c(dominio,...1,directorio,secuencia_p,orden)))

#save the descriptive table in a txt file
stargazer(as.data.frame(skim_result), summary=F, type="text",out = file.path(dir$views,'data_description.txt'))

#create a new df containing just discrete data
db_geih_discrete <- db_geih |> select(p6870, p6050, relab, estrato1, p6240, regSalud,
                                      cotPension, p6050, p6090,p7495, p7505,p7040, 
                                      p7090, cuentaPropia, microEmpresa, sex, formal)

#get percentages of every possible category for all discrete data
porcentajes <- list()

for (col in names(db_geih_discrete)) {
  freq <- table(db_geih_discrete[[col]])
  props <- prop.table(freq)
  porcentajes[[col]] <- props
}

#extract just percentages and create a df
tabla_porcentajes <- map_df(names(porcentajes), function(var) {
 
  prop_vector <- porcentajes[[var]]
  
  data.frame(
    variable = var,
    categoria = names(prop_vector),
    porcentaje = as.vector(prop_vector)
  )
})

#Next lines up to saving the result in a txt file, create a organized table with percentages 

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

#save the result table in a txt file
stargazer(as.data.frame(tabla_porcentajes), summary = FALSE, type="text", out=file.path(dir$views,'discrete_description.txt'))

#create a correlation graph for each variable
corr_graph <- db_geih |>
  select_if(is.numeric) |> 
  cor(use = "pairwise") |>
  round(1)

p <- ggcorrplot(corr_graph, type = "lower", lab = T, show.legend = F) 
ggsave(filename =file.path(dir$views,'corr_graph.png'), plot = p, width = 10, height = 10, dpi = 300)

db_geih |> select(ingtot_H)
