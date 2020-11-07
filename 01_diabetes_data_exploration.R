###########################################################################
### Análisis exploratorio
###########################################################################
### Autor: Gonzalo Chacaltana
### Tema:  Estudio comparativo de modelos de clasificación para identificar 
###        pacientes con diabetes
###########################################################################

#---------------------------------------------------------
# Limpiar el workspace
rm(list = ls())

# Cambiar directorio de trabajo
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Cargamos la base de datos
library(data.table)
dataset <- fread("datos/dataDiabetes.csv", header=T, verbose =FALSE, 
                 stringsAsFactors=TRUE, showProgress =TRUE)

# Variables y datos informativos de la base de datos
library(mlr)
summarize_dataset <- data.frame(mlr::summarizeColumns(dataset))
write.csv(summarize_dataset,"salidas/tabla_resumen_dataset.csv")

summary(dataset)

#View(dataset)

source("functions.R")

######################################
# Análisis Univariado
######################################

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

printTable(dataset$pregnant)

library(ggplot2)

# Histograma de la variable edad
hist(dataset$age, main = "Histograma de la variable Edad",
     xlab = "Años de edad",
     ylab = "Frecuencia",
     col = "blue",
     border = "black",
     xlim = c(min(dataset$age),max(dataset$age)))


# variable insulina
hist(dataset$insulin, main = "Histograma de la variable Insulina",
     xlab = "Insulina (ml)",
     ylab = "Frecuencia",
     col = "pink",
     border = "black",
     xlim = c(min(dataset$insulin),max(dataset$insulin)))


#Variable Diabetes
plot(dataset$diabetes, col=c("green3","firebrick1"), 
     main = "Distribución variable diabetes",
     xlab = "Diabetes", ylab = "Pacientes")

printTable(dataset$diabetes)

###################################################
# Analisis Multivariado
###################################################

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

install.packages("visdat")
library(ggplot2)
library(visdat)

vis_miss(dataset)
vis_miss(dataset, cluster = TRUE) +
        theme(axis.text.x=element_text(size=rel(1.2), angle = 90))

dataset$diabetes <- ifelse(dataset$diabetes=="pos",1,0)
cor(dataset$pregnant,dataset$diabetes, method = "pearson")
