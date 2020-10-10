################################################################################
# Análisis exploratorio de la base de datos diabetes.
# Gonzalo Chacaltana
###############################################################################

#---------------------------------------------------------
# Limpiar el workspace
rm(list = ls())

# Cambiar directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargamos la base de datos

library(data.table)
dataset <-fread("dataDiabetes.csv",header=T, verbose =FALSE, stringsAsFactors=TRUE, showProgress =TRUE)

# Variables y datos informativos de la base de datos
library(mlr)
summarizeColumns(dataset)

summary(dataset)

View(dataset)

# Análisis univariado

# Histograma de la variable pregnant
hist(dataset$pregnant, main = "Histograma de la variable Pregnat",
     xlab = "Cantidad de embarazos",
     ylab = "Frecuencia",
     col = "red",
     border = "black",
     xlim = c(min(dataset$pregnant),max(dataset$pregnant)))

# Histograma de la variable edad
hist(dataset$age, main = "Histograma de la variable Age",
     xlab = "Años de edad",
     ylab = "Frecuencia",
     col = "blue",
     border = "black",
     xlim = c(min(dataset$age),max(dataset$age)))

#Variable Diabetes
plot(dataset$diabetes,col = "green", main = "Distribución de la variable Diabetes",
     xlab="Resultado", ylab="Frecuencia")
