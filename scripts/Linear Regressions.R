# Cargar librerías necesarias
source(file.path(dir$scripts, "00_load_requierments.R"))

# Definir directorio
setwd('C:/Users/jroja/OneDrive/Documents/Big Data & Machine Learning/Problem Set 1')

# Cargar data limpia
data_geih <- read.csv('clean_data.csv')

# Revisar las posibles variables para modelar
colnames(data_geih)

# Revisar qué diferencia existe entre ingtot e ingtotob
sum(data_geih$ingtot == data_geih$ingtotob)

# Calcular el log del ingreso en una nueva columna del df y añadir cols de ingreso no laboral y de ingreso por salario
data_geih <- data_geih %>% mutate(log_ingtot = log(ingtot),
                                  log_ingtot = if_else(log_ingtot == '-Inf', 0, log_ingtot),
                                  ingnolab = p7500s1a1 + p7500s2a1 + p7500s3a1 +
                                    p7510s1a1 + p7510s2a1 + p7510s3a1 +
                                    p7510s5a1 + p7510s6a1 + p7510s7a1,
                                  ingtotsal = ingtot - ingnolab)

# Calcular ingreso por salario por hora
data_geih <- data_geih %>% mutate(salario_h = ingtotsal / (totalHoursWorked*4))

# Crear transformación de educación al cuadrado
data_geih <- data_geih %>% mutate(educ_2 = maxEducLevel^2)

# Convertir maxEduc y regSalud en vars categóricas
data_geih$maxEducLevel <- as.factor(data_geih$maxEducLevel)
data_geih$regSalud <- as.factor(data_geih$regSalud)

# regresar el ingreso solo contra la variable dicótoma de género
reg_simple <- lm(log_ingtot ~ sex, data = data_geih)
stargazer(reg_simple, type = 'text')

# regresar el salario por hora contra la var de género
reg_simple2 <- lm(salario_h ~ sex, data = data_geih)
stargazer(reg_simple2, type = 'text')

# regresar el ingreso contra la var de género controlando por características de los trabajadores
reg_mul1 <- lm(log_ingtot ~ sex + age + maxEducLevel + educ_2 + regSalud +
                 formal + totalHoursWorked + p7505, data = data_geih)
stargazer(reg_mul1, type = 'text')

# regresar el salario contra la var de género controlando por características de los trabajadores
reg_mul2 <- lm(salario_h ~ sex + age + maxEducLevel + educ_2 + regSalud + 
                 formal + totalHoursWorked + p7505, data = data_geih)
stargazer(reg_mul2, type = 'text')

## deberíamos incluir oficio y relab!
