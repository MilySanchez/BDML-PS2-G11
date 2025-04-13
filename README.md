# Repositorio de Conjunto de Problemas 2: Prediciendo la pobreza - Grupo 11: Big Data y Machine Learning para la Economía Aplicada


Este repositorio tiene como objetivo predecir la pobreza en hogares colombianos utilizando técnicas de **Machine Learning** como regresión logística, Elastic Net, árboles de decisión (CARTs), bosques aleatorios y métodos de boosting. Los datos provienen del **DANE** y de la misión **MESE** (Empalme de las Series de Empleo, Pobreza y Desigualdad), e incluyen información tanto a nivel de hogar como de individuo, lo que permite construir variables adicionales que mejoran la capacidad predictiva de los modelos. Este proyecto hace parte de una competencia en [**Kaggle**](https://www.kaggle.com/competitions/uniandes-bdml-202510-ps-2/team), organizada en el marco del curso de *Big Data y Machine Learning* de la **Universidad de los Andes** durante el semestre 2025-1, pagina donde tambien se encuentra toda la información sobre los datos utilizados.
El objetivo es construir modelos precisos que utilicen la menor cantidad de variables posible con la mayor capacidad predictiva posible.

## Estructura del Repositorio

Este repositorio está dividido en varias carpetas que organizan el contenido de manera eficiente:

### 1. `document`
Contiene el archivo en formato `.pdf` del informe final presentado sobre el análisis realizado.

### 2. `stores`
Esta carpeta contiene todos los datos utilizados en el proyecto. Está subdividida en las siguientes secciones:
- **`raw`**: Contiene los datos recién extraídos sin procesar.
- **`processed`**: Contiene los datos procesados,incluyendo la transformación de las variables a nivel persona y hogar.
- **`predictions`**: Este directorio contiene las predicciones generadas por cada uno de los modelos utilizados. Los archivos siguen la convención de nombrado:  
  `"ModelName_param1_value1_param2_value2_... .csv"`  

### 3. `views`
Aquí se encuentran todas las gráficas utilizadas en la construcción del documento, 

### 4. `scripts`
Esta carpeta contiene todos los scripts de código utilizados para el desarrollo del análisis. Cada script se ejecuta de manera independiente pero sigue el progreso del script anterior. En total, hay **7 scripts** organizados de la siguiente manera:

1. **00_load_requierments.R**  
   **Función**: Carga las librerías necesarias para el funcionamiento de todos los códigos. Este script debe ejecutarse al inicio de cada uno de los siguientes scripts.
   
2. **01_data_processing.R**  
   **Función**: Procesa y limpia los datos crudos a nivel de hogar y persona. Incluye el tratamiento de valores nulos, la transformación de variables existentes y la creación de nuevas variables relevantes para el análisis.

   
3. **02_descriptive_data.R**  
      **Función**: Genera tablas de estadísticas descriptivas y un gráfico de correlación entre todas las variables disponibles después de la limpieza de datos. El script guarda una tabla en formato LaTeX y una imagen del gráfico de correlación.

Además, el repositorio incluye múltiples scripts en R utilizados para entrenar distintos modelos de clasificación. Cada script sigue la convención de nombrado:  
`"ModelName_param1_value1_param2_value2_..._ModelNumber.R"`  
donde `ModelNumber` se utiliza cuando se entrenan modelos con los mismos hiperparámetros pero distintas combinaciones de variables explicativas.

Se implementan diversos algoritmos, incluyendo **Elastic Net**, **Logit**, **Random Forest**, **CARTs**, **XGBoost** y **AdaBoost**, cada uno con diferentes configuraciones de parámetros especificadas en el nombre del archivo correspondiente.
   

## Cómo ejecutar los scripts

Para ejecutar los scripts, simplemente sigue el siguiente orden después de clonar o descargar el repositorio:

1. Ejecuta **00_load_requirements.R** para cargar todas las librerías necesarias.
2. Corre **01_data_processing.R** para procesar y limpiar los datos, así como para crear las variables que serán utilizadas por los modelos.
3. Los scripts correspondientes a cada modelo pueden ejecutarse de forma independiente, ya que han sido diseñados como módulos autocontenidos. Esto significa que, una vez completados los pasos anteriores, cada script puede correr de manera autónoma sin necesidad de pasos intermedios adicionales.

---

Este es un trabajo elaborado para la materia de Big Data y Machine Learning para economía de la Universidad de los Andes, semestre 2025-1.
Por los estudiantes: 
- Juanita Rojas Cuellar.
- Mily Geraldine Sanchez Julio.
- Natalia Cortes Güesguan.
- Alejandro Sarasti Sierra.
