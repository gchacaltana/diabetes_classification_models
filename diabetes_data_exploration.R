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

barplot(table(dataset$pregnant), main = "Distribución de la cantidad de embarazos",
        xlab = "Cantidad de embarazos",
        ylab = "Frecuencia",
        col = "red",
        border = "black")

library(ggplot2)
# Histograma de la variable edad

hist(dataset$age, main = "Histograma de la variable Age",
     xlab = "Años de edad",
     ylab = "Frecuencia",
     col = "blue",
     border = "black",
     xlim = c(min(dataset$age),max(dataset$age)))

#Variable Diabetes
dat$label = factor(
        dat$label, levels = 1:3,
        labels = c("Negativo", "Positivo")
)

plot(dataset$diabetes ~ dat$label,col = c(1:2), main = "Distribución de la variable Diabetes",
     xlab="Resultado", ylab="Frecuencia")


# variable insulina
hist(dataset$insulin, main = "Histograma de la variable Insulin",
     xlab = "Insulina (ml)",
     ylab = "Frecuencia",
     col = "pink",
     border = "black",
     xlim = c(min(dataset$insulin),max(dataset$insulin)))

# analisis multivariado
# Resultado de Diabetes según cantidad de embarazos.

tc_pregnat_01=table(dataset$pregnant,dataset$diabetes)

barplot(t(tc_pregnat_01),col=2:3,beside = T,
        xlab="Cantidad de Embarazos",
        ylab="Frecuencia",
        main="Distribución de diabetes según cantidad de embarazos")
legend("topright",legend=levels(dataset$diabetes),col=2:3,
       pch=15,title="Resultado Diabetes")

# Resultado de diabetes según edad
tc_edad_01=table(dataset$age,dataset$diabetes)

barplot(t(tc_edad_01),col=2:3,beside = T,
        xlab="Edad",
        ylab="Frecuencia",
        main="Distribución de diabetes según edad")
legend("topright",legend=levels(dataset$diabetes),col=2:3,
       pch=15,title="Resultado Diabetes")

# Identificación de outliers

boxplot(dataset$pregnant,main="Datos atípicos Variable Cantidad de Embarazos")

# Identificación de outliers

boxplot(dataset$age,main="Datos atípicos Variable Edad")

boxplot(dataset$insulin,main="Datos atípicos Variable Insulina")
