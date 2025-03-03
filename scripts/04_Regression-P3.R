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

db_geih <- data_clean |> mutate(p7500s1_0 = ifelse(p7500s1a1==0,T,F),
                             p7500s2_0 = ifelse(p7500s2a1==0,T,F),
                             p7500s3_0 = ifelse(p7500s3a1==0,T,F),
                             p7510s1_0 = ifelse(p7510s1a1==0,T,F),
                             p7510s2_0 = ifelse(p7510s2a1==0,T,F),
                             p7510s3_0 = ifelse(p7510s3a1==0,T,F),
                             p7510s5_0 = ifelse(p7510s5a1==0,T,F),
                             p7510s6_0 = ifelse(p7510s6a1==0,T,F),
                             p7510s7_0 = ifelse(p7510s7a1==0,T,F))

stargazer(db_geih |> summarise(across(ends_with("_0"), summary)), type="text", summary=F, out = file.path(dir$views, "P3_Age-wage-profile", "T_F_variables.txt"))


# REGRESSION
reg1 <- lm(logwage~age+age2, data=data_clean)

stargazer(reg1,summary = F, out=file.path(dir$views,'reg1.txt'))

ggplot(data_clean, aes(x = age, y = logwage)) +
  geom_point(alpha = 0.5) +  
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), color = "red", se = TRUE) +
  labs(
       x = "Edad",
       y = "log(Salario)") +
  theme_minimal()

ggsave(file.path(dir$views, paste0("age_wage_regression", ".pdf")), 
       width = 8, height = 6, dpi = 300)

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
  return(peak_age_resampled)
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
PEAK_LOGWAGE <- data_clean$predicted_logwage[data_clean$age == round(PEAK_AGE)][1]

# Plot wage-hour, peak age and confidence intervals
ggplot(data_clean, aes(x = age, y = logwage)) +
  geom_line(aes(y = predicted_logwage), color = "#00BFC4", size = 1) +  # Perfil estimado
  geom_point(aes(x = PEAK_AGE, y = PEAK_LOGWAGE), color = "darkblue", size = 3) +  # Pico
  geom_errorbarh(aes(y = PEAK_LOGWAGE, xmin = ci_lower, xmax = ci_upper), color = "darkblue", height = 0.05, linewidth = 1.5) +  # Intervalo de confianza
  labs(x = "Edad",
       y = "Log(Salario)") +
  lims(x = c(18, 100), y = c(9,11)) +
  theme_minimal()

ggsave(file.path(dir$views, paste0("age_wage_profile", ".pdf")), 
       width = 8, height = 6, dpi = 300)
