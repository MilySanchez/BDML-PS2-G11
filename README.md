# Repositorio de Conjunto de Problemas - Grupo 11: Big Data y Machine Learning para la Economía Aplicada

Este repositorio tiene como objetivo analizar la relación del ingreso salarial por hora de la población colombiana, utilizando técnicas de **Big Data** y **Machine Learning** aplicadas a los datos obtenidos del **[DANE](https://www.dane.gov.co/index.php/estadisticas-por-tema/mercado-laboral/empleo-y-desempleo/geih-historicos)** a través del informe de "Medición de Pobreza Monetaria y Desigualdad".

## Estructura del Repositorio

Este repositorio está dividido en varias carpetas que organizan el contenido de manera eficiente:

### 1. `document`
Contiene el archivo en formato `.pdf` del informe final presentado sobre el análisis realizado.

### 2. `stores`
Esta carpeta contiene todos los datos utilizados en el proyecto. Está subdividida en las siguientes secciones:
- **`raw`**: Contiene los datos recién extraídos sin procesar.
- **`processed`**: Contiene los datos procesados, tanto con limpieza como sin limpieza.

### 3. `views`
Aquí se encuentran todas las gráficas y tablas creadas durante el análisis del problema. Estas están organizadas de acuerdo a las diferentes etapas del desarrollo del problema:
- **`P2_Data`**: Datos iniciales.
- **`P3_Age-wage-profile`**: Perfil de edad y salario.
- **`P4_Gender-earnings-GAP`**: Brecha salarial por género.
- **`P5_Predicting-earnings`**: Predicción de ingresos.

### 4. `scripts`
Esta carpeta contiene todos los scripts de código utilizados para el desarrollo del análisis. Cada script se ejecuta de manera independiente pero sigue el progreso del script anterior. En total, hay **8 scripts** organizados de la siguiente manera:

1. **00_load_requierments.R**  
   **Función**: Carga las librerías necesarias para el funcionamiento de todos los códigos. Este script debe ejecutarse al inicio de cada uno de los siguientes scripts.
   
2. **01_web_scrapping.R**  
   **Función**: Realiza un proceso de **web scraping** para extraer los datos de la página web [GEIH 2018 sample](https://ignaciomsarmiento.github.io/GEIH2018_sample/). El objetivo es recuperar 10 fragmentos de datos correspondientes a una muestra de GEIH 2018, almacenándolos en archivos CSV individuales y agrupados en un solo archivo CSV.
   
3. **02_DataCleaning.R**  
   **Función**: Limpia y prepara los datos obtenidos del proceso de **web scraping**. El proceso incluye: filtrar observaciones correspondientes a adultos empleados, eliminar variables con muy poca información, transformar variables existentes y crear nuevas variables relevantes para los siguientes análisis.
   
4. **03_Descriptive_Data.R**  
   **Función**: Crea tablas de estadísticas descriptivas y un gráfico de correlación de todas las variables existentes después de la limpieza de datos. También guarda la tabla en formato LaTeX y la imagen del gráfico de correlación.
   
5. **04_Regression-P3.R**  
   **Función**: Realiza un análisis de regresión de los salarios sobre la edad utilizando una especificación cuadrática. Estima la edad máxima a la que se alcanzan los salarios más altos, aplica un procedimiento de **bootstrap** para derivar intervalos de confianza y visualiza los resultados.
   
6. **05_Regressions-P4.R**  
   **Función**: Realiza un análisis de regresión de los salarios sobre el género. Estima la brecha salarial utilizando **OLS**, **FWL** y **FWL con bootstrap**. Además, estima las edades máximas a las que se alcanzan los salarios más altos, aplica un procedimiento de **bootstrap** para derivar intervalos de confianza y visualiza los resultados según el género.
   
7. **06_Predictions-P5.R**  
   **Función**: Realiza un análisis de datos y comparación de modelos para predecir los ingresos (logaritmo del salario) basándose en diversas características demográficas y laborales. El proceso incluye preprocesamiento de los datos, ajuste de modelos, evaluación y validación cruzada para identificar el mejor modelo para predecir los salarios.

## Cómo ejecutar los scripts

Para ejecutar los scripts, simplemente sigue el siguiente orden:
1. Carga las librerías con el script **00_load_requierments.R**.
2. Luego, ejecuta los scripts en orden, desde **01_web_scrapping.R** hasta **06_Predictions-P5.R**. Cada script está diseñado para ejecutarse de manera independiente, pero sigue un flujo de trabajo secuencial en términos de análisis.

---

Este repositorio es una herramienta útil para estudiar la relación entre los ingresos salariales y las variables demográficas en Colombia utilizando técnicas de **Big Data** y **Machine Learning**. Esperamos que los resultados de este análisis puedan contribuir a una mejor comprensión de la economía laboral colombiana.
