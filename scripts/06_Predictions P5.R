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

data_clean <- data_clean  %>% mutate(estrato1=as.factor(estrato1),
                                     p6050=as.factor(p6050),
                                     p6090=as.factor(p6090),
                                     p6210=as.factor(p6210),
                                     p6240=as.factor(p6240),
                                     p7495=as.factor(p7495),
                                     p7505=as.factor(p7505),
                                     regSalud=as.factor(regSalud),
                                     formal=as.factor(formal),
                                     oficio=as.factor(oficio),
                                     cotPension=as.factor(cotPension),
                                     p6870 = as.factor(p6870))

data_clean <- data_clean %>% mutate(p6240 = relevel(p6240, ref = 2))

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

# Graph the distribution of the dependent variable in training and testing samples

data_clean <- data_clean %>% mutate(partition = ifelse(row_number() %in% inTrain, "Entrenamiento", "Prueba"),
                                    gender = ifelse(female == 1, "Mujer", "Hombre"))

ggplot(data_clean) +
  geom_density(aes(x = logwage), fill = "blue", alpha = 0.5) +
  labs(
    x = "Log(Salario)",
    y = "Densidad"
  ) +
  facet_wrap(~partition) +
  theme_minimal() +
  theme(strip.text = element_text(size = 16, face = "bold"))+
  geom_text(
    data = data_clean %>%
      group_by(partition) %>%
      summarise(n = n()),
    aes(x = Inf, y = Inf, label = paste("N =", n)),
    hjust = 1.1, vjust = 5,
    inherit.aes = FALSE
  )

ggsave(file.path(dir$views, paste0("logwage_distribution_train_test", ".pdf")), 
       width = 8, height = 6, dpi = 300)


# Graph the distribution of the independent variable in training and testing samples
# Most used one, sex and age

ggplot(data_clean) +
  geom_density(aes(x = age), fill = "red", alpha = 0.5) +
  labs(
    x = "Edad",
    y = "Densidad"
  ) +
  facet_wrap(~partition) +
  theme_minimal() +
  theme(strip.text = element_text(size = 16, face = "bold"))+
  geom_text(
    data = data_clean %>%
      group_by(partition) %>%
      summarise(n = n()),
    aes(x = Inf, y = Inf, label = paste("N =", n)),
    hjust = 1.1, vjust = 5,
    inherit.aes = FALSE
  )

ggsave(file.path(dir$views, paste0("age_distribution_train_test", ".pdf")), 
       width = 8, height = 6, dpi = 300)

data_prop <- data_clean %>%
  group_by(partition, gender) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(partition) %>%
  mutate(prop = n / sum(n) * 100)  # Convertir a porcentaje

ggplot(data_prop) +
  geom_bar(aes(x = gender, y = prop, fill = gender), stat = "identity", alpha = 0.7) +
  labs(
    x = "Género",
    y = "Porcentaje"
  ) +
  facet_wrap(~partition) +
  theme_minimal() +
  theme(strip.text = element_text(size = 16, face = "bold"), legend.position = "none") 

ggsave(file.path(dir$views, paste0("sex_distribution_train_test", ".pdf")), 
       width = 8, height = 6, dpi = 300)


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 3. Compare the predictive performance of specifications =====================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Define several model specifications

model_1 <- lm(logwage ~ age + age2, data = training)

model_2 <- lm(logwage ~ female, data = training)

model_3 <- lm(logwage ~ female + age + age2 + female:age + 
                female:age2 + estrato1 + p6240 + 
                p6426 + p6870 + cotPension + 
                p7040 + p7495 + p7505, data = training)

model_4 <- lm(logwage ~ female + age + age2 + female:age +
                female:age2 + poly(age, 3, raw = T) + estrato1 + 
                p6240 + p6426 + p6870 + cotPension + 
                p7040 + p7495 + p7505, data = training)

model_5 <- lm(logwage ~ female:cotPension + female:p6870 + female + 
                cotPension + p6870  + estrato1 + p6240 + p6426 + 
                p7040 + p7495 + p7505 + age + age2, data = training)

model_6 <- lm(logwage ~ age + age2 + poly(age,3,raw=T) + poly(age,4,raw=T) + 
                poly(age,5,raw=T)  + poly(age,6,raw=T) + poly(age,7,raw=T) + 
                poly(age,8,raw=T) + poly(age,9,raw=T) + poly(age,10,raw=T), 
              data = training)

model_7 <- lm(logwage ~ age + age2 + p6870:age + p6870:totalHoursWorked + 
                totalHoursWorked + poly(totalHoursWorked, 2, raw=T), 
              data = training)

training <- training %>% mutate(logP6426 = ifelse(p6426 == 0, 0, log(p6426)))
testing <- testing %>% mutate(logP6426 = ifelse(p6426 == 0, 0, log(p6426)))
                
model_8 <- lm(logwage ~ age + age2 + p6426 + poly(p6426,2,raw=T) + logP6426+ 
                p6870 + estrato1 + p6240 + p6426 + abs(age-53)+
                + cotPension + p7040 + p7495 + p7505, data = training)

# Create

RMSE_values <- numeric(8)

# Calculate the RMSE for each model
 
for (i in 1:8) {
  model <- get(paste0('model_', i))
  pred <- predict(model, newdata = testing)
  RMSE_values[i] <- caret::RMSE(pred, testing$logwage)
}

stargazer(RMSE_values, type = "text", title = "RMSE values for different model specifications",
          out = file.path(dir$views, "RMSE_values.txt"))

# Compute the prediction errors in the test sample

best_pred <- predict(model_5, newdata = testing)
best_residuals <- testing$logwage - best_pred

# Examine its distribution

hist(best_residuals, breaks = 30, xlab = "Residuos", ylab = "Frecuencia", col = "lightblue", main = "")



# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =  
# 4. Calculate predictive error using LOOCV ===================================
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 

# Define LOOCV as the method for cross validation approach
ctrl <- trainControl(
  method = "LOOCV")

# Enable progress printing
ctrl$verboseIter <- T

# Train models
model_3b <- train(
  logwage ~ female + age + age2 + female:age + female:age2 + 
    estrato1 + p6240 + p6426 + p6870 + cotPension + p7040 + 
    p7495 + p7505, 
  data = data_clean,
  method = "lm",
  trControl = ctrl
)

model_4b <- train(
  logwage ~ female + age + age2 + female:age + female:age2 + 
    poly(age, 3, raw = TRUE) + estrato1 + p6240 + p6426 + 
    p6870 + cotPension + p7040 + p7495 + p7505, 
  data = data_clean,
  method = "lm",
  trControl = ctrl,
  metric = 'RMSE'
)

model_5b <- train(
  logwage ~ female:cotPension + female:p6870 + female + cotPension + 
    p6870  + estrato1 + p6240 + p6426 + p7040 + p7495 + p7505 + age + age2,
  data = data_clean,
  method = "lm",
  trControl = ctrl,
  metric = 'RMSE'
) 

stargazer(model_3b$results, model_4b$results, model_5b$results, type = "text", title = "LOOCV RMSE values for different model specifications",
          out = file.path(dir$views, "LOOCV_RMSE_values.txt"))
