##########################################################
# Title: Exercise Number 3 Regression Wage ~ age.
# Description: This script performs a regression analysis 
# of wages on age using a quadratic specification. 
# It estimates the peak age at which wages are maximized, 
# applies a bootstrap procedure to derive 
# confidence intervals, and visualizes the results.
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

# REGRESSION
reg1 <- lm(logwage~age+age2, data=data_clean)

stargazer(reg1,summary = F, out=file.path(dir$views,'reg1.txt'))

ggplot(data_clean, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = TRUE) +
  labs(title = "Regresión",
       x = "Age",
       y = "log(W)") +
  theme_minimal()

# PEAK AGE - MAX DERIVATES
beta_1 <- coef(reg1)[2]
beta_2 <- coef(reg1)[3]
PEAK_AGE <- -beta_1 / (2 * beta_2) # 53 YEARS

# Function to estimates peak age in a boostrap model
PEAK_AGE_bootstrap <- function(data, indicator) {
  data_resampled <- data[indicator, ]  # Muestra con reemplazo
  reg_resampled <- lm(logwage ~ age + age2, data = data_resampled)
  beta_1_resampled <- coef(reg_resampled)[2]
  beta_2_resampled <- coef(reg_resampled)[3]
  peak_age_resampled <- -beta_1_resampled / (2 * beta_2_resampled)
  return(edad_pico_resampled)
}

# Do boostrap (1000 times)
set.seed(42)
bootstrap_result <- boot(data_clean, statistic = PEAK_AGE_bootstrap, R = 1000)

# Estimate confidence intervals (percent 2.5 and 97.5)
ci_lower <- quantile(bootstrap_result$t, 0.025)
ci_upper <- quantile(bootstrap_result$t, 0.975)

# Print results
cat("Peak age estimated: ", PEAK_AGE, "years\n")
cat("Confidence intervals: (", ci_lower, ", ", ci_upper, ") years\n")


# Create a new df with predict of to model to age
data_clean$predicted_logwage <- predict(reg1, newdata = data_clean)

# Plot wage-hour, peak age and confidence intervals
ggplot(data_clean, aes(x = age, y = logwage)) +
  geom_point(color = "red", alpha = 0.5) +  # Datos reales
  geom_line(aes(y = predicted_logwage), color = "blue", size = 1) +  # Perfil estimado
  geom_vline(xintercept = PEAK_AGE, color = "green", linetype = "dashed", size = 1) +  # Edad de pico
  geom_vline(xintercept = ci_lower, color = "orange", linetype = "dashed", size = 1) +  # Límite inferior CI
  geom_vline(xintercept = ci_upper, color = "orange", linetype = "dashed", size = 1) +  # Límite superior CI
  labs(title = "Perfil estimado de ganancias por edad y edad de pico con CI Bootstrap",
       x = "Edad",
       y = "Log(Salario)") +
  theme_minimal()

