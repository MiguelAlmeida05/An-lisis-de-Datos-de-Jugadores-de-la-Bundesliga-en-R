# Instala los paquetes necesarios si no los tienes instalado
install.packages("wordcloud")
install.packages("rpart")
install.packages("sentimentr")
install.packages("RcolorBrewer")
install.packages("tidytext")

# Carga las bibliotecas necesarias
library(tidyverse)
library(ggplot2)
library(readr)
library(wordcloud)
library(rpart)
library(sentimentr)
library(RColorBrewer)
library(tidytext)


# Lee el archivo CSV
df <- read_csv("Data/bundesliga_player.csv")
head(df)

#Para ver la estructura de los datos
str(df)
summary(df)
dim(df)
colnames(df)

# Suponiendo que 'df' es tu dataframe
# Calcula el número de valores NA por columna
na_count <- colSums(is.na(df))

# Muestra el resultado
print(na_count)

# Define una función para resumir las columnas de un dataframe en R
column_summary <- function(data) {
  # Inicializa vectores para almacenar la información
  col <- character()
  dtype <- character()
  unique_vals <- list()
  n_unique <- numeric()
  nulls <- numeric()
  duplicated_vals <- numeric()
  
  # Itera sobre las columnas del dataframe
  for (column in names(data)) {
    # Añade el nombre de la columna
    col <- c(col, column)
    # Añade el tipo de datos
    dtype <- c(dtype, class(data[[column]]))
    # Calcula los valores únicos y su longitud
    unique_values <- unique(data[[column]])
    unique_vals <- c(unique_vals, list(unique_values))
    n_unique <- c(n_unique, length(unique_values))
    # Calcula el número de valores nulos
    nulls <- c(nulls, sum(is.na(data[[column]])))
    # Calcula el número de valores duplicados
    duplicated_vals <- c(duplicated_vals, sum(duplicated(data[[column]])))
  }
  
  # Crea un dataframe con los resultados
  summary_df <- data.frame('Column' = col, 'Data Type' = dtype,
                           'Unique Values' = I(unique_vals), 'Unique_Num' = n_unique,
                           'Null Values' = nulls, 'Duplicated Values' = duplicated_vals)
  
  return(summary_df)
}

# Llama a la función para obtener el resumen de columnas
summary_result <- column_summary(df)
print(summary_result)

# Suponiendo que 'df' es tu dataframe
# Selecciona todas las columnas excepto 'name' y 'full_name'
df1 <- subset(df, select = -c(name, full_name))

# Muestra las primeras filas del nuevo dataframe
head(df1)

# Suponiendo que 'df1' es tu dataframe en RStudio
# Calcula los recuentos de la columna 'age'
age_counts <- table(df1$age)

# Crea un gráfico de barras
barplot(age_counts, main = "Counts of Age", xlab = "Age", ylab = "Count")


# Suponiendo que 'df1' es tu dataframe en RStudio
# Define el tamaño del gráfico
par(mar = c(5, 4, 4, 2) + 0.1)  # Ajusta los márgenes para dejar espacio para las etiquetas de los ejes

# Crea el gráfico de barras
barplot(df1$max_price, names.arg = df1$age, xlab = 'Age', ylab = 'Max Price', 
        main = 'Price per Age', col = 'skyblue', las = 2)

# Añade una leyenda para la barra
legend("topright", legend = "Max Price", fill = "skyblue")

# Suponiendo que 'df1' es tu dataframe en RStudio
# Selecciona las columnas que contienen datos de tipo "character"
character_columns <- names(df1)[sapply(df1, is.character)]

# Muestra las columnas seleccionadas
character_columns

column_summary <- function(data) {
  # Inicializa vectores para almacenar la información
  col <- character()
  dtype <- character()
  unique_vals <- numeric()
  n_unique <- numeric()
  nulls <- numeric()
  duplicated_vals <- numeric()
  
  # Itera sobre las columnas del dataframe
  for (column in names(data)) {
    # Añade el nombre de la columna
    col <- c(col, column)
    # Añade el tipo de datos
    dtype <- c(dtype, class(data[[column]]))
    # Calcula los valores únicos y su longitud
    unique_values <- unique(data[[column]])
    unique_vals <- c(unique_vals, length(unique_values))
    n_unique <- c(n_unique, length(unique_values))
    # Calcula el número de valores nulos
    nulls <- c(nulls, sum(is.na(data[[column]])))
    # Calcula el número de valores duplicados
    duplicated_vals <- c(duplicated_vals, sum(duplicated(data[[column]])))
  }
  
  # Crea un dataframe con los resultados
  summary_df <- data.frame('Column' = col, 'Data Type' = dtype,
                           'Unique_Num' = n_unique,
                           'Null Values' = nulls, 'Duplicated Values' = duplicated_vals)
  
  return(summary_df)
}

# Llama a la función column_summary() con tu dataframe como argumento
summary_result <- column_summary(df1)
print(summary_result)


# Calcula los recuentos de nacionalidades
nationality_counts <- table(df$nationality)

# Crea la nube de palabras
wordcloud(words = names(nationality_counts), freq = nationality_counts, 
          min.freq = 1, max.words = 100,
          colors = brewer.pal(9, "Set1"), 
          scale = c(2, 0.5),  # Ajusta la escala de las palabras
          random.color = TRUE, # Usa colores aleatorios
          max.word.size = 80, # Tamaño máximo de la palabra
          min.word.length = 3, # Longitud mínima de la palabra
          rot.per = 0.3,      # Controla la rotación de las palabras
          main = "Nacionalidades en el DataFrame")  # Título del gráfico

# Ajustar un modelo de regresión lineal
lm_model <- lm(max_price ~ age, data = df1)

# Resumen del modelo
summary(lm_model)

# Realizar predicciones para nuevas edades
new_ages <- data.frame(age = c(20, 25, 30))
predictions <- predict(lm_model, newdata = new_ages)
predictions
