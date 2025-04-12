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
dir$store <- file.path(dir$root, "store")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
dir$results <- file.path(dir$root, "results")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 1. Descriptive Data ===========================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# Load data

train_hogares <- read.csv(file.path(dir$store, "train_hogares.csv"))
train_personas <- read.csv(file.path(dir$store, "train_personas.csv"))

# Initial exploration

skim(train_hogares)
View(dfSummary(train_hogares))

# Descriptive statistics for numeric variables

datos_hogar %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    media = mean,
    sd = sd,
    min = min,
    max = max,
    q25 = ~quantile(., 0.25),
    q75 = ~quantile(., 0.75),
    skewness = ~e1071::skewness(.)
  ), na.rm = TRUE))

# Descriptive statistics for categorical variables

