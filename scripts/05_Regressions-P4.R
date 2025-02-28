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
dir$views <- file.path(dir$root, "views","P4_Gender-earnings-GAP")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# Set seed

set.seed(123)

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
                                    maxEducLevel = as.factor(p6210),
                                    regSalud = as.factor(regSalud),
                                    cotPension = as.factor(cotPension),
                                    college = as.factor(college),
                                    cuentaPropia = as.factor(cuentaPropia))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Run regressions wage vs sex ===============================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable
reg_simple <- lm(logwage ~ sex, data = data_clean)
stargazer(reg_simple, type = 'latex', out = file.path(dir$views,'reg_simple.txt'))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Run regressions wage vs sex with controls ================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# as a function

resid_logwage <- residuals(lm(logwage ~  oficio + relab + p6426 + 
                                maxEducLevel + p6870 + regSalud + 
                                college + regSalud + cotPension + formal + 
                                cuentaPropia + p7495 + p7505, data = data_clean))

# Regress sex on all control variables (excluding logwage)
resid_sex <- residuals(lm(sex ~ oficio + relab + p6426 + 
                            maxEducLevel + p6870 + regSalud + 
                            college + regSalud + cotPension + formal + 
                            cuentaPropia + p7495 + p7505, data = data_clean))

resid_age <- residuals(lm(age ~ oficio + relab + p6426 + 
                            maxEducLevel + p6870 + regSalud + 
                            college + regSalud + cotPension + formal + 
                            cuentaPropia + p7495 + p7505, data = data_clean))

resid_age2 <- residuals(lm(age2 ~ oficio + relab + p6426 + 
                             maxEducLevel + p6870 + regSalud + 
                             college + regSalud + cotPension + formal + 
                             cuentaPropia + p7495 + p7505, data = data_clean))

# Regress residuals of logwage on residuals of sex
reg_multiple <- lm(resid_logwage ~ resid_sex + resid_age + resid_age2)
stargazer(reg_multiple, type = 'latex', out = file.path(dir$views,'reg_multiple.txt'))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Comparison between standard errors ================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Bootstrap standard errors

# Number of bootstrap samples
B <- 1000

fn_reg_wage_gap <- function(data, index) {
  # Resample the data
  boot_sample <- data[index, ]
  
  # Obtain residuals from first-stage regressions
  resid_logwage <- residuals(lm(logwage ~ age + age2 + oficio + relab + p6426 + 
                                  maxEducLevel + p6870 + regSalud + 
                                  college + regSalud + cotPension + formal + 
                                  cuentaPropia + p7495 + p7505, data = boot_sample))
  
  resid_sex <- residuals(lm(sex ~ age + age2 + oficio + relab + p6426 + 
                              maxEducLevel + p6870 + regSalud + 
                              college + regSalud + cotPension + formal + 
                              cuentaPropia + p7495 + p7505, data = boot_sample))
  
  # Run regression on residuals
  reg_boot <- lm(resid_logwage ~ resid_sex)
  
  # Return the coefficient of resid_sex
  return(coef(reg_boot))
}

boot_reg_wage_gap <- boot(data_clean, statistic = fn_reg_wage_gap, R = B)

# Obtain the standard errors
se_boot_intercept <- sd(boot_reg_wage_gap$t[, 1])  # Intercept SE
se_boot_sex <- sd(boot_reg_wage_gap$t[, 2])  # resid_sex SE

stargazer(reg_multiple, type = 'latex', out = file.path(dir$views,'reg_multiple_boot.txt'),
          se = list(c(se_boot_intercept,se_boot_sex)))

# Drop boot because it is not used anymore and it is a large object
rm(boot_reg_wage_gap)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Graph predicted log(wage) ================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 



