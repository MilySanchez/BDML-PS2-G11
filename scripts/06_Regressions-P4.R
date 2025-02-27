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

data_clean <- read.csv(file.path(dir$processed,'data_cleanGEIH2.csv'))

# Check the names of the variables

colnames(data_clean)

#data_num <- data_clean %>% select(where(is.double))
#corrs <- cor(data_num, use = "complete.obs")
#corrplot(corrs, method = 'square', type = 'lower', diag = F, tl.cex = 0.6)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 2. Estimate the wage gap with OLS (unconditional and conditional) ===========
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable

reg_simple <- lm(logwage ~ female, data = data_clean)
stargazer(reg_simple, type = 'text')

#data_clean <- data_clean %>%
#  filter(impa > 0,
#         p6500 > 0,
#         y_salary_m > 0,
#         y_salary_m_hu > 0,
#         y_ingLab_m > 0,
#         y_ingLab_m_ha > 0) %>% 
#  mutate(impah = impa / (totalHoursWorked*4),
#         p6500h = p6500 / (totalHoursWorked*4),
#         salarymh = y_salary_m / (totalHoursWorked*4)) %>% 
#  mutate(l_impa = log(impah),
#         l_p6500 = log(p6500h),
#         l_ysalarym = log(salarymh),
#         l_ysalaryhu = log(y_salary_m_hu))

reg_simple2 <- lm(l_impa ~ female, data = data_clean)
stargazer(reg_simple2, type = 'text')

reg_simple3 <- lm(l_p6500 ~ female, data = data_clean)
stargazer(reg_simple3, type = 'text')

reg_simple4 <- lm(l_ysalarym ~ female, data = data_clean)
stargazer(reg_simple4, type = 'text')

reg_simple5 <- lm(l_ysalaryhu ~ female, data = data_clean)
stargazer(reg_simple5, type = 'text')

# regression between wage and gender, controlling by characteristics of workers

#data_clean <- data_clean %>% mutate(female = as.factor(female),
#                                    estrato1 = as.factor(estrato1),
#                                    relab = as.factor(relab),
#                                    p6240 = as.factor(p6240),
#                                    p6870 = as.factor(p6870),
#                                    regSalud = as.factor(regSalud),
#                                    p6210 = as.factor(p6210),
#                                    cotPension = as.factor(cotPension),
#                                    p7040 = as.factor(p7040),
#                                    formal = as.factor(formal),
#                                    p7495 = as.factor(p7495),
#                                    p7505 = as.factor(p7505),
#                                    cuentaPropia = as.factor(cuentaPropia))

#data_clean <- data_clean %>% mutate(relab = relevel(relab, ref = 8))
#data_clean <- data_clean %>% mutate(p6240 = relevel(p6240, ref = 4))

reg_multi <- lm(logwage ~ female + age + age2 + estrato1 + p6240 + 
                  p6426 + p6870 + regSalud + p6210 + cotPension + 
                  p7040 + p7495 + p7505, data = data_clean)
stargazer(reg_multi, type = 'text')

reg_multi2 <- lm(l_impa ~ female + age + age2 + estrato1 + p6240 + 
                  p6426 + p6870 + regSalud + p6210 + cotPension + 
                  p7040 + p7495 + p7505, data = data_clean)
stargazer(reg_multi2, type = 'text')

reg_multi3 <- lm(l_ysalaryhu ~ female + age + age2 + estrato1 + p6240 + 
                   p6426 + p6870 + regSalud + p6210 + cotPension + 
                   p7040 + p7495 + p7505, data = data_clean)
stargazer(reg_multi3, type = 'text')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Estimate the conditional wage gap using FWL ==============================
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

# regress the residuals

reg_FWL <- lm(resid_y ~ resid_x)
stargazer(reg_FWL, type = 'latex')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Estimate the conditional wage gap using FWL with bootstrap ===============
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# create function that estimates the coefficients using FWL

fn_coef <- function(data, index){
  reg_y <- lm(logwage ~ age + age2 + relab + p6426 + p6870 + regSalud + 
                p6210 + cotPension + formal + p7495 + p7505, 
              data = data_clean)
  resid_y <- resid(reg_y)
  reg_x <- lm(female ~ age + age2 + relab + p6426 + p6870 + regSalud + 
                p6210 + cotPension + formal + p7495 + p7505, 
              data = data_clean)
  resid_x <- resid(reg_x)
  reg_FWL <- lm(resid_y ~ resid_x)
  return(coef(reg_FWL)['female'])
}

# check the function works well

fn_coef(data_clean, 1:nrow(data_clean))

# set seed to achieve reproducibility

set.seed(111)

# use boot 

boot_result <- boot(data = data_clean, statistic = fn_coef, R = 1000)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Plot the predicted age-wage profile ======================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =


