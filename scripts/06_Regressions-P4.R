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
# 2. Estimate the wage gap with OLS (unconditional and conditional) ===========
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regression between wage and the gender dichotomous variable

reg_simple1 <- lm(logwage ~ female, data = data_clean)

# regression between wage and gender, controlling by characteristics of workers

data_clean <- data_clean %>% mutate(estrato1 = as.factor(estrato1),
                                    relab = as.factor(relab),
                                    p6240 = as.factor(p6240),
                                    p6870 = as.factor(p6870),
                                    regSalud = as.factor(regSalud),
                                    p6210 = as.factor(p6210),
                                    cotPension = as.factor(cotPension),
                                    p7040 = as.factor(p7040),
                                    formal = as.factor(formal),
                                    p7495 = as.factor(p7495),
                                    p7505 = as.factor(p7505),
                                    cuentaPropia = as.factor(cuentaPropia),
                                    sex = as.factor(sex),
                                    microEmpresa = as.factor(microEmpresa))

data_clean <- data_clean %>% mutate(relab = relevel(relab, ref = 8))
data_clean <- data_clean %>% mutate(p6240 = relevel(p6240, ref = 2))

reg_multi_inter <- lm(logwage ~ female + female:age + female:age2 + age + 
                         age2 + estrato1 + p6240 + p6426 + p6870 + regSalud + 
                         p6210 + cotPension + p7040 + p7495 + p7505, 
                       data = data_clean)

stargazer(reg_multi_inter, type = 'text')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 3. Estimate the conditional wage gap using FWL ==============================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# regress logwage on all control variables

reg_y_inter <- lm(logwage ~ age + age2 + female:age + female:age2 +
                    estrato1 + p6240 + p6426 + p6870 + regSalud + 
                    p6210 + cotPension + p7040 + p7495 + p7505, 
                  data = data_clean)
resid_y_inter <- resid(reg_y_inter)

# regress female on all control variables

reg_x_inter <- lm(female ~ age + age2 + female:age + female:age2 +
              estrato1 + p6240 + p6426 + p6870 + regSalud + p6210 + 
                cotPension + p7040 + p7495 + p7505, data = data_clean)
resid_x_inter <- resid(reg_x_inter)

# regress the residuals

reg_FWL_inter <- lm(resid_y_inter ~ resid_x_inter)
stargazer(reg_FWL_inter, type = 'text')

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 4. Estimate the conditional wage gap using FWL with bootstrap ===============
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# create function that estimates the coefficients using FWL

age_mean = mean(data_clean$age)

fn_fwl <- function(data, index, age) {
  reg_y <- lm(logwage ~ age + age2 + female:age + female:age2 + estrato1 + 
                p6240 + p6426 + p6870 + regSalud + p6210 + cotPension + 
                p7040 + p7495 + p7505, data = data[index, ])
  resid_y <- resid(reg_y)
  reg_x <- lm(female ~ age + age2 + female:age + female:age2 + estrato1 +
                p6240 + p6426 + p6870 + regSalud + p6210 + cotPension + 
                p7040 + p7495 + p7505, data = data[index, ])
  resid_x <- resid(reg_x)
  reg_fwl <- lm(resid_y ~ resid_x)
  return(reg_fwl$coefficients[2])
}

# set seed to achieve reproducibility

set.seed(111)

# use boot 

bootstrap <- boot(data = data_clean, statistic = fn_fwl, R = 1000)
bootstrap

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
# 5. Plot the predicted age-wage profile ======================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# create a range of ages for the plot

ages <- seq(18, max(data_clean$age, na.rm = T))

# create two dataframes, one for men and one for women

data_men <- data.frame(female = 0,
                       age = ages,
                       age2 = ages^2,
                       estrato1 = factor(2),
                       p6240 = factor(1),
                       p6426 = mean(data_clean$p6426, na.rm = T),
                       p6870 = factor(9),
                       regSalud = factor(1),
                       p6210 = factor(6),
                       cotPension = factor(1),
                       p7040 = factor(2),
                       p7495 = factor(2),
                       p7505 = factor(2)
                       )

data_women <- data.frame(female = 1,
                         age = ages,
                         age2 = ages^2,
                         estrato1 = factor(2),
                         p6240 = factor(1),
                         p6426 = mean(data_clean$p6426, na.rm = T),
                         p6870 = factor(9),
                         regSalud = factor(1),
                         p6210 = factor(6),
                         cotPension = factor(1),
                         p7040 = factor(2),
                         p7495 = factor(2),
                         p7505 = factor(2)
                         )

# predict the wage (log) by gender based on the previous multiple regression

data_men$logwage_predicted <- predict(reg_multi_inter, newdata = data_men)
data_women$logwage_predicted <- predict(reg_multi_inter, newdata = data_women)

# create a whole dataframe, combining men and women data, to be able to plot

df_plot <- bind_rows(data_men %>% mutate(gender = 'Men'),
                     data_women %>% mutate(gender = 'Women')
                     )

# plot the predicted age-wage profile by gender

ggplot(df_plot, aes(x = age, y = logwage_predicted, color = gender)) + 
  geom_line(size = 1) + labs(title = 'Predicted Age-Wage Profile by Gender',
                             x = 'Age', y = 'Log(Wage)') +
  theme_minimal()

ggsave(file.path(dir$views, paste0("age_wage_profile_by_gender", ".png")), 
       width = 8, height = 6, dpi = 300)

# save the coefficients of the multiple regression

coefs <- coefficients(reg_multi_inter)

# calculate the peak ages for each gender with the derivative of age=0

peak_age_men <- -coefs[3] / (2*coefs[4])
peak_age_men

peak_age_women <- -(coefs[3] + coefs[35]) / (2*(coefs[4] + coefs[36]))
peak_age_women

# Build confidence intervals using bootstrap

fn_peaks <- function(data, index) {
  reg_boot <- lm(logwage ~ age + age2 + female + female:age + female:age2 +
                   estrato1 + p6240 + p6426 + p6870 + regSalud + p6210 + 
                   cotPension + p7040 + p7495 + p7505, data = data[index, ])
  coefs <- coefficients(reg_boot)
  peak_age_men <- -coefs['age'] / (2*coefs['age2'])
  peak_age_women <- -(coefs['age'] + coefs[35]) / 
    (2*(coefs['age2'] + coefs[36]))
  return(c(peak_age_men, peak_age_women))
}

set.seed(111)

peaks_boot <- boot(data = data_clean, statistic = fn_peaks, R = 1000)
peaks_boot

ci_men <- boot.ci(peaks_boot, index = 1, type = 'perc')
ci_men
ci_women <- boot.ci(peaks_boot, index = 2, type = 'perc')
ci_women

