###########################################################################
### Partición del conjunto de datos
###########################################################################
### Autor: Gonzalo Chacaltana
### Tema:  Estudio comparativo de modelos de clasificación para identificar 
###        pacientes con diabetes
###########################################################################

# Cargamos la base de datos
library(data.table)
dataset <- fread("datos/dataDiabetes.csv", header = T, verbose = FALSE, 
                 stringsAsFactors = TRUE, showProgress = TRUE)

# Separamos los conjuntos de entrenamiento (75%) y validación (25%)
# -----------------------------------------------------------------
library(caret)
set.seed(123)
index <- caret::createDataPartition(dataset$diabetes, p = 0.75, list = FALSE)
dataset_train <- dataset[index, ] # 285 observaciones
dataset_test <-  dataset[-index, ] # 97 observaciones

## Distribución variable riesgo - datos de entrenamiento

plot(dataset_train$diabetes, col=c("green3","firebrick1"), 
     main = "Distribución variable diabetes - Datos de Entrenamiento",
     xlab = "Diabetes", ylab = "Pacientes")

printTable(dataset_train$diabetes)

## Distribución variable riesgo - datos de prueba

plot(dataset_test$diabetes, col=c("green3","firebrick1"), 
     main = "Distribución variable diabetes - Datos de Prueba",
     xlab = "Diabetes", ylab = "Pacientes")

printTable(dataset_test$diabetes)

# Creamos los dataset.
write.csv(dataset_train, "datos/diabetes_train.csv")
write.csv(dataset_test, "datos/diabetes_test.csv")