##########################################################
# Title: Exercise Number 5 predicting earnings
# Description: This script ...
#
# Date: 26/02/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 0. Workspace configuration ===================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$raw <- file.path(dir$root, "stores", "raw")
dir$processed <- file.path(dir$root, "stores", "processed")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

pacman::p_load(caret) #### quitar

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 1. Load data =================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load clean data

data_clean <- read.csv(file.path(dir$processed,'data_cleanGEIH.csv'))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 2. Split the sample =========================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Set seed to achieve reproducibility

set.seed(111)

# Split data randomly

inTrain <- createDataPartition(
  y = data_clean$logwage,
  p = .70,
  list = F
)

# Define test and training data sets

training <- data_clean %>%
  filter(row_number() %in% inTrain)

testing <- data_clean %>%
  filter(!row_number() %in% inTrain)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 3. Compare the predictive performance of specifications =====================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

model_1 <- 


