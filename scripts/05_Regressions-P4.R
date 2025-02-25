##########################################################
# Title: Linear Regressions.
# Description: This script ...
#
# Date: 23/02/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 0. Workspace configuration ===================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  

# Clear workspace

rm(list = ls())

# Set up paths

dir <- list()
dir$root <- getwd()
dir$processed <- file.path(dir$root, "stores", "processed")
dir$raw <- file.path(dir$root, "stores", "raw")
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 1. Load data =================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Load clean data
data_clean <- read.csv(file.path(dir$processed,'data_cleanGEIH.csv'))

# Check the names of the variables
colnames(data_clean)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 2. Transform data ============================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# CONVERT TO HOUR WAGE AND LOG
data_clean <- data_clean %>% mutate(logwage=log(ingtot_H),
                                    age2=age^2)

data_clean <- data_clean %>% mutate(oficio = as.factor(oficio),
                                    relab = as.factor(relab),
                                    maxEducLevel = as.factor(maxEducLevel),
                                    regSalud = as.factor(regSalud),
                                    cotPension = as.factor(cotPension),
                                    college = as.factor(college),
                                    cuentaPropia = as.factor(cuentaPropia))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Run regressions wage vs sex ===============================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable
reg_simple <- lm(logwage ~ sex, data = data_clean)
stargazer(reg_simple, type = 'text')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Run regressions wage vs sex with controls ================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

resid_logwage <- residuals(lm(logwage ~ age + age2 + oficio + relab + p6426 + 
                                maxEducLevel + p6870 + regSalud + 
                                college + regSalud + cotPension + formal + 
                                cuentaPropia + p7495 + p7505, data = data_clean))

# Regress sex on all control variables (excluding logwage)
resid_sex <- residuals(lm(sex ~ age + age2 + oficio + relab + p6426 + 
                            maxEducLevel + p6870 + regSalud + 
                            college + regSalud + cotPension + formal + 
                            cuentaPropia + p7495 + p7505, data = data_clean))

# Regress residuals of logwage on residuals of sex
reg_multiple <- lm(resid_logwage ~ resid_sex)
