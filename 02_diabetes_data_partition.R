###########################################################################
### Partición del conjunto de datos
###########################################################################
### Autor: Gonzalo Chacaltana
### Tema:  Estudio comparativo de modelos de clasificación para identificar 
###        pacientes con diabetes
###########################################################################

# Limpiar el entorno de trabajo
#------------------------------
rm(list = ls(all.names = TRUE))

# Cambiar el directorio de trabajo
#---------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()

# Cargar la base de datos
library(data.table)
dataset <- fread("datos/dataDiabetes.csv", header = T, verbose = FALSE, 
                 stringsAsFactors = TRUE, showProgress = TRUE)

# Cambiamos valores de la target
dataset$diabetes <- ifelse(dataset$diabetes == "pos", 1, 0)

# load functions.
source("functions.R")

# Particionar el conjunto de datos. 
# Entrenamiento (75%) y validación (25%)
# -----------------------------------------------------------------
library(ggplot2)
library(caret)
set.seed(1212)
index <- caret::createDataPartition(dataset$diabetes, p = 0.75, list = FALSE)
dataset.train <- dataset[index, ] # 285 observaciones
dataset.test <-  dataset[-index, ] # 97 observaciones

## Distribución variable target - datos de entrenamiento
ggplot(dataset.train, aes(x = factor(diabetes))) +
    geom_bar() +
    ggtitle("Distribución variable diabetes \nDatos Entrenamiento") +
    xlab("Indicador diabetes") + ylab("Pacientes")

printTable(dataset.train$diabetes)

## Distribución variable target - datos de prueba

ggplot(dataset.test, aes(x = factor(diabetes))) +
    geom_bar() +
    ggtitle("Distribución variable diabetes \nDatos Prueba") +
    xlab("Indicador diabetes") + ylab("Pacientes")

printTable(dataset.test$diabetes)

# Tranformación de variables cuantitativas (predictoras)
# ------------------------------------------------------

numeric.variables = c("pregnant","glucose","pressure","triceps","insulin","mass","pedigree","age")

# Transformación por normalización - Entrenamiento
dataset.train.norm <- dataset.train
dataset.train.norm[,numeric.variables] <- lapply(dataset.train.norm[,..numeric.variables], normalize_data)

dataset.train.st <- dataset.train
dataset.train.st[,numeric.variables] <- lapply(dataset.train.st[,..numeric.variables], standardize_data)

# Transformación por normalización - Pruebas
dataset.test.norm <- dataset.test
dataset.test.norm[,numeric.variables] <- lapply(dataset.test.norm[,..numeric.variables], normalize_data)

dataset.test.st <- dataset.test
dataset.test.st[,numeric.variables] <- lapply(dataset.test.st[,..numeric.variables], standardize_data)


# Selección de variable - Boruta
boruta_variables = c("pregnant","glucose","pressure","triceps","insulin","mass","pedigree","age", "diabetes")

dataset.train.boruta.norm <- dataset.train.norm
dataset.train.boruta.st <- dataset.train.st

dataset.test.boruta.norm <- dataset.test.norm
dataset.test.boruta.st <- dataset.test.st


# Selección de variable - Curva Roc
roc_variables = c("glucose","triceps","insulin","age", "diabetes")

dataset.train.roc.norm <- dataset.train.norm[, c("glucose","triceps","insulin","age", "diabetes")]
dataset.train.roc.st  <- dataset.test.st[, c("glucose","triceps","insulin","age", "diabetes")]

dataset.test.roc.norm <- dataset.train.st[, c("glucose","triceps","insulin","age", "diabetes")]
dataset.test.roc.st  <- dataset.test.st[, c("glucose","triceps","insulin","age", "diabetes")]

save(dataset,
     # datos.train,
     dataset.train.boruta.norm,
     dataset.train.boruta.st,
     dataset.train.roc.norm,
     dataset.train.roc.st,
     dataset.test.boruta.norm,
     dataset.test.boruta.st,
     dataset.test.roc.norm,
     dataset.test.roc.st,
     file = 'datos/diabetes.rda'
)

# Creamos los dataset.
#write.csv(dataset_train, "datos/diabetes_train.csv")
#write.csv(dataset_test, "datos/diabetes_test.csv")
