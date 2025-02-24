
##########################################################
# Title: Exercise Number 3 Regression Wage ~ age.
# Description: This script webscrapes the data from the website
# https://ignaciomsarmiento.github.io/GEIH2018_sample/ the
# objective is to retrive 10 chunks of data from the website
# corresponding to a sample of GEIH 2018. Each one would be
# stored in a individual csv file.
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
setwd(dir$root)

# Load required libraries

source(file.path(dir$scripts, "00_load_requierments.R"))


# 02_load clean data GEIH DB
data_clean <- read.csv(file.path(dir$processed,'data_cleanGEIH.csv'))

## 3

# CONVERT TO HOUR WAGE AND LOG
data_clean <- data_clean %>% mutate(logwage=log(ingtot_H), age2=age^2)

# REGRESSION
reg1 <- lm(logwage~age+age2, data=data_clean)

stargazer(reg1,summary = F)

ggplot(data_clean, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = TRUE) +
  labs(title = "Regresión",
       x = "Age",
       y = "log(W)") +
  theme_minimal()
