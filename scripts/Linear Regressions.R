##########################################################
# Title: Linear Regressions.
# Description: This script ...
#
# Date: 23/02/2025
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
dir$views <- file.path(dir$root, "views")
dir$scripts <- file.path(dir$root, "scripts")
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 1. Load data ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = =

# Load clean data
data_geih <- read.csv(file.path(dir$stores, 'data_limpiaGEIH.csv'))

# Check the names of the variables
colnames(data_geih)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 2. Transform data ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = =

# Check the difference between ingtot and igntotob
sum(data_geih$ingtot == data_geih$ingtotob)

# Create a new column of the log(wage) and add columns of non labour income and wage
data_geih <- data_geih %>% mutate(log_ingtot = log(ingtot),
                                  log_ingtot = if_else(log_ingtot == '-Inf', 0, log_ingtot),
                                  ingnolab = p7500s1a1 + p7500s2a1 + p7500s3a1 +
                                    p7510s1a1 + p7510s2a1 + p7510s3a1 +
                                    p7510s5a1 + p7510s6a1 + p7510s7a1,
                                  ingtotsal = ingtot - ingnolab)

# Create a new column with data on the hourly wage
data_geih <- data_geih %>% mutate(salario_h = ingtotsal / (totalHoursWorked*4))

# Create variable of education ^2
data_geih <- data_geih %>% mutate(educ_2 = maxEducLevel^2)

# Define maxEducLevel and regSalud as factor (categoric) variables
data_geih$maxEducLevel <- as.factor(data_geih$maxEducLevel)
data_geih$regSalud <- as.factor(data_geih$regSalud)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 2. Run regressions wage vs sex ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = =

# regression between wage and the gender dichotomous variable
reg_simple <- lm(log_ingtot ~ sex, data = data_geih)
stargazer(reg_simple, type = 'text')

# regression between hourly wage and the gender dichotomous variable
reg_simple2 <- lm(salario_h ~ sex, data = data_geih)
stargazer(reg_simple2, type = 'text')

# regression between wage and gender, controlling by characteristics of workers
reg_mul1 <- lm(log_ingtot ~ sex + age + maxEducLevel + educ_2 + regSalud +
                 formal + totalHoursWorked + p7505, data = data_geih)
stargazer(reg_mul1, type = 'text')

# regression between hourly wage and gender dichotomous variable controlling by
# workers' characteristics
reg_mul2 <- lm(salario_h ~ sex + age + maxEducLevel + educ_2 + regSalud + 
                 formal + totalHoursWorked + p7505, data = data_geih)
stargazer(reg_mul2, type = 'text')

## deberíamos incluir oficio y relab!
