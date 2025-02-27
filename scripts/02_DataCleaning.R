
##########################################################
# Title: Data Cleaning.
# Description: This script cleans and prepares the data
obtained from the webscrapping process. This process included:
filtering observations corresponding to employed adults,
removing variables containing very little information,
transforming existing variables, and creating new variables
relevant for the next analyses.
#
# Date: 09/02/2025
##########################################################

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 
# 0. Workspace configuration ====================================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  = = = = = = = = 

# 01_Clear workspace

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


# 02_load inputs GEIH DB

db_geih <- read.csv(file.path(dir$raw,'table_geih.csv'))

# 03_Overview
# Columns
variable.names(db_geih)
## print data
head(db_geih)
## summary db
skim(db_geih) %>% head()


# 04_DATA CLEANING
# FILTER (>= 18 YEAR AND LABOR POPULATION) 16.542
db_geih <- db_geih %>% filter(age>=18,ocu==1)

skim_result <- skim(db_geih)


filter_columns <- skim_result[skim_result$complete_rate >= 0.7, ] %>% select(skim_variable) %>% pull()


# COLUMNS FILTER, COMPLETENESS OF DATA 70% OR ECONOMIC IMPORTANCE 70% (42 variables)
db_geih <- db_geih %>% select(all_of(filter_columns)) 

# TRANSFORMATION TO FACTOR CATAGORICAL VARIABLES AND WAGE BY HOUR
db_geih <- db_geih %>% mutate(sex=as.factor(sex),
                                  estrato1=as.factor(estrato1),
                                  p6050=as.factor(p6050),
                                  p6090=as.factor(p6090),
                                  p6100=as.factor(p6100),
                                  p6210=as.factor(p6210),
                                  p6210s1=as.factor(p6210s1),
                                  p6240=as.factor(p6240),
                                  p7495=as.factor(p7495),
                                  p7505=as.factor(p7505),
                                  pet=as.factor(pet),
                                  maxEducLevel=as.factor(maxEducLevel),
                                  regSalud=as.factor(regSalud),
                                  wap=as.factor(wap),
                                  ocu=as.factor(ocu),
                                  dsi=as.factor(dsi),
                                  pea=as.factor(pea),
                                  inac=as.factor(inac),
                                  formal=as.factor(formal),
                                  oficio=as.factor(oficio),
                                  ingtot_H=ingtot/totalHoursWorked
)

# REMOVE DUPLICATE COLUMNS
db_geih <- db_geih %>% select(-c(p6100,pet,wap,ocu,dsi,inac,p6920,pea,informal,p7070,maxEducLevel,sizeFirm))

# FILTER WAGE > 0
db_geih <- db_geih %>% filter(ingtot>0)

# CONVERT WAGE TO LOG AND CREATE SQUEARED AGE
db_geih <- db_geih %>% mutate(logwage=log(ingtot_H), age2=age^2)

# RELEVEL COTPENSION
db_geih <- db_geih %>% mutate(cotPension = relevel(cotPension, ref = 2))

# CREATE FEMALE VARIABLE
db_geih <- db_geih %>% mutate(female = ifelse(sex == 0, 1, 0))

# AVERAGES-MEANS OF NUMERICAL VARIABLES ON NAS
db_geih <- db_geih %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>% 
  mutate(age=as.numeric(age))

# SAVE DATA CLEAN
write.csv(db_geih, file.path(dir$processed, paste0("data_cleanGEIH", ".csv")), row.names = F)



