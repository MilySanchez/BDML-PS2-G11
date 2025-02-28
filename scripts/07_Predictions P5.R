##########################################################
# Title: Exercise Number 5 predicting earnings
# Description: This script ...
#
# Date: 26/02/2025
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

data_clean <- data_clean  %>% mutate(sex=as.factor(sex),
                                     estrato1=as.factor(estrato1),
                                     p6050=as.factor(p6050),
                                     p6090=as.factor(p6090),
                                     p6210=as.factor(p6210),
                                     p6210s1=as.factor(p6210s1),
                                     p6240=as.factor(p6240),
                                     p7495=as.factor(p7495),
                                     p7505=as.factor(p7505),
                                     regSalud=as.factor(regSalud),
                                     formal=as.factor(formal),
                                     oficio=as.factor(oficio),
                                     cotPension=as.factor(cotPension),
                                     ingtot_H=ingtot/totalHoursWorked,
                                     p6870 = as.factor(p6870))

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 2. Split the sample =========================================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Set seed to achieve reproducibility

set.seed(111)

# Split data randomly

inTrain <- createDataPartition(
  y = data_clean$logwage,
  p = .70,
  list = F
)

# Define test and training data sets

training <- data_clean %>%
  filter(row_number() %in% inTrain)

testing <- data_clean %>%
  filter(!row_number() %in% inTrain)

# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 3. Compare the predictive performance of specifications =====================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

model_1 <- lm(logwage ~ age + age2, data = training)

model_2 <- lm(logwage ~ female*age + female*age2 + estrato1 + p6240 + 
                p6426 + p6870 + regSalud  + cotPension + 
                p7040 + p7495 + p7505, data = training)

model_3 <- lm(logwage ~ female*age + female:age2 +female:poly(age,3,raw=T) + estrato1 + p6240 + 
                p6426 + p6870 + regSalud  + cotPension + 
                p7040 + p7495 + p7505, data = training)

model_4 <- lm(logwage ~ female:cotPension + female:p6870 + female + cotPension + 
                p6870  + estrato1 + p6240 + p6426  + regSalud + 
                 + cotPension + p7040 + p7495 + p7505 + age + age2, data = training)

model_5 <- lm(logwage ~ age + age2 + poly(age,3,raw=T) + poly(age,4,raw=T) + 
                poly(age,5,raw=T)  + poly(age,6,raw=T) + poly(age,7,raw=T) + poly(age,8,raw=T)
              + poly(age,9,raw=T) + poly(age,10,raw=T) , data = training)

model_6 <- lm(logwage ~ age + age2 + p6870:age + p6870:totalHoursWorked + totalHoursWorked +
                poly(totalHoursWorked,2,raw=T), data = training)

training <- training %>% mutate(logP6426 = ifelse(p6426 == 0, 0, log(p6426)))
testing <- testing %>% mutate(logP6426 = ifelse(p6426 == 0, 0, log(p6426)))
                
model_7 <- lm(logwage ~ age + age2 + p6426 + poly(p6426,2,raw=T) + logP6426+ 
                p6870 + estrato1 + p6240 + p6426 + regSalud + abs(age-53)+
                + cotPension + p7040 + p7495 + p7505, data = training)

RMSE_values <- numeric(7)  


for (i in 1:7) {
  model <- get(paste0('model_', i))
  pred <- predict(model, testing)
  RMSE_values[i] <- caret::RMSE(pred, testing$logwage)
}

