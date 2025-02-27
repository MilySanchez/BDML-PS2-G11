##########################################################
# Title: Exercise Number 4 gender earnings.
# Description: This script performs a regression analysis 
# of wages on gender. It estimates the wage gap using OLS,
# FWL, and FWL with bootstrap. Then estimates the peak ages
# at which wages are maximized, applies a bootstrap procedure
# to derive confidence intervals, and visualizes the results
# by gender.
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
dir$raw <- file.path(dir$root, "stores", "raw")
dir$processed <- file.path(dir$root, "stores", "processed")
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


# RELEVEL SOME VARIABLES
data_clean <- data_clean %>% mutate(cotPension = relevel(cotPension, 
                                                         ref = 2))

# CREATE FEMALE VARIABLE
data_clean <- data_clean %>% mutate(female = ifelse(sex == 0, 1, 0))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Estimate the wage gap with OLS (unconditional and conditional) ===========
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable
reg_simple <- lm(logwage ~ female, data = data_clean)
stargazer(reg_simple, type = 'latex')

# regression between wage and gender, controlling by characteristics of workers
reg_multi <- lm(logwage ~ female + age + age2 + relab + p6426 + 
                  p6870 + regSalud + p6210 + regSalud + cotPension + 
                  formal + p7495 + p7505, data = data_clean)
stargazer(reg_multi, type = 'latex', keep = 'female')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Estimate the conditional wage gap using FWL ==============================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regress logwage on all control variables
reg_y <- lm(logwage ~ age + age2 + relab + p6426 + p6870 + regSalud + 
                 p6210 + cotPension + formal + p7495 + p7505, 
               data = data_clean)
resid_y <- resid(reg_y)

# regress female on all control variables
reg_x <- lm(female ~ age + age2 + relab + p6426 + p6870 + regSalud + 
              p6210 + cotPension + formal + p7495 + p7505, 
            data = data_clean)
resid_x <- resid(reg_x)

# regress residuals
reg_FWL <- lm(resid_y ~ resid_x)
stargazer(reg_FWL, type = 'latex')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Estimate the conditional wage gap using FWL with bootstrap ===============
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# Create a function that gets coefficients
fn <- function(data, index){
  coef(lm(logwage ~ female + age + age2 + relab + p6426 + 
            p6870 + regSalud + p6210 + regSalud + cotPension + 
            formal + p7495 + p7505, data = data, subset = index))[2]
}

# Check that the function works
fn(data_clean, 1:nrow(data_clean))

# Estimate the standard error using boot

boot(data_clean, fn, R = 1000)



=======
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
dir$raw <- file.path(dir$root, "stores", "raw")
dir$processed <- file.path(dir$root, "stores", "processed")
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

# CONVERT CATEGORIC VARIABLES AS FACTOR
data_clean <- data_clean %>% mutate(oficio = as.factor(oficio),
                                    relab = as.factor(relab),
                                    p6210 = as.factor(p6210),
                                    regSalud = as.factor(regSalud),
                                    cotPension = as.factor(cotPension))

# RELEVEL SOME VARIABLES
data_clean <- data_clean %>% mutate(cotPension = relevel(cotPension, 
                                                         ref = 2))

# CREATE FEMALE VARIABLE
data_clean <- data_clean %>% mutate(female = ifelse(sex == 0, 1, 0))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Estimate the wage gap with OLS (unconditional and conditional) ===========
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable
reg_simple <- lm(logwage ~ female, data = data_clean)
stargazer(reg_simple, type = 'latex')

# regression between wage and gender, controlling by characteristics of workers
reg_multi <- lm(logwage ~ female + age + age2 + relab + p6426 + 
                  p6870 + regSalud + p6210 + regSalud + cotPension + 
                  formal + p7495 + p7505, data = data_clean)
stargazer(reg_multi, type = 'latex', keep = 'female')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Estimate the conditional wage gap using FWL ==============================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regress logwage on all control variables
reg_y <- lm(logwage ~ age + age2 + relab + p6426 + p6870 + regSalud + 
                 p6210 + cotPension + formal + p7495 + p7505, 
               data = data_clean)
resid_y <- resid(reg_y)

# regress female on all control variables
reg_x <- lm(female ~ age + age2 + relab + p6426 + p6870 + regSalud + 
              p6210 + cotPension + formal + p7495 + p7505, 
            data = data_clean)
resid_x <- resid(reg_x)

# regress residuals
reg_FWL <- lm(resid_y ~ resid_x)
stargazer(reg_FWL, type = 'latex')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Estimate the conditional wage gap using FWL with bootstrap ===============
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# solo bootstrap?
pacman::p_load(boot)

fn_coef <- function(data, index){
  model <- lm(logwage ~ female + age + age2 + relab + p6426 + 
            p6870 + regSalud + p6210 + regSalud + cotPension + 
            formal + p7495 + p7505, data = data, subset = index)
  return(coef(model)['female'])
}

fn_coef(data_clean, 1:nrow(data_clean))

set.seed(111)

boot_result <- boot(data = data_clean, statistic = fn_coef, R = 1000)

boot.ci(boot_result, type = 'perc')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 6. Plot the predicted age-wage profile ======================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


