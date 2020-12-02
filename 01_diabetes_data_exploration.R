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

library(ggplot2)

# Histograma de la variable pregnant

ggplot(dataset, aes(x=pregnant)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Pregnant",x="Embarazos", y="Densidad")

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

# Histograma de la variable edad

ggplot(dataset, aes(x=age)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Age",x="Edad", y="Densidad")

hist(dataset$age, main = "Histograma de la variable Edad",
     xlab = "Años de edad",
     ylab = "Frecuencia",
     col = "blue",
     border = "black",
     xlim = c(min(dataset$age),max(dataset$age)))


# variable insulina

ggplot(dataset, aes(x=insulin)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Insulin",x="Nivel Insulina (U/ml)", y="Densidad")

hist(dataset$insulin, main = "Histograma de la variable Insulina",
     xlab = "Insulina (ml)",
     ylab = "Frecuencia",
     col = "pink",
     border = "black",
     xlim = c(min(dataset$insulin),max(dataset$insulin)))

# Glucosa

ggplot(dataset, aes(x=glucose)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Glucose",x="Nivel Glucosa", y="Densidad")

# Pressure

ggplot(dataset, aes(x=pressure)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Pressure",x="Presión Arterial (mm Hg)", y="Densidad")

# Triceps

ggplot(dataset, aes(x=triceps)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Triceps",x="Espesor cutáneo tríceps (mm)", y="Densidad")

# Mass

ggplot(dataset, aes(x=mass)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Mass",x="Indice Masa Corporal", y="Densidad")

# Pedigree

ggplot(dataset, aes(x=pedigree)) + 
        geom_histogram(bins=30,aes(y=..density..), colour="darkblue", fill="lightblue")+
        geom_density(alpha=.2, fill="red") +
        labs(title="Distribución Pedigree",x="Pedigree Diabetes", y="Densidad")


# Variable Diabetes
library(dplyr)

df <- dataset %>% group_by(diabetes) %>% summarise(counts = n())
df

ggplot(df, aes(x=diabetes,y=counts)) +
        geom_bar(colour="darkblue",fill = "lightblue", stat = "identity") +
        geom_text(aes(label = counts), vjust = -0.5) + 
        labs(title="Distribución Diabetes",x="Resultado Diabetes", y="Frecuencia")

plot(dataset$diabetes, col=c("green3","firebrick1"), 
     main = "Distribución variable diabetes",
     xlab = "Diabetes", ylab = "Pacientes")

printTable(dataset$diabetes)

# Correlaciones
# Cambiamos valores de la target
dataset$diabetes <- ifelse(dataset$diabetes == "pos", 1, 0)

install.packages("GGally")
library(GGally)
ggcorr(dataset[,-1], nbreaks = 5)

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

par(mfrow=c(1, 2))
boxplot(dataset$pregnant,main="Embarazos", ylab="Frecuencia")
boxplot(dataset$age,main="Edad", ylab="Frecuencia")

par(mfrow=c(1, 2))
boxplot(dataset$pressure,main="Presión Arterial", ylab="Frecuencia")
boxplot(dataset$glucose,main="Glucosa", ylab="Frecuencia")

par(mfrow=c(1, 2))
boxplot(dataset$insulin,main="Insulina", ylab="Frecuencia")
boxplot(dataset$mass,main="Masa Corporal", ylab="Frecuencia")
summary(dataset$insulin)

boxplot(dataset$triceps,main="Triceps", ylab="Frecuencia")
boxplot(dataset$pedigree,main="Pedigree", ylab="Frecuencia")

boxplot(dataset$insulin,main="Datos atípicos Variable Insulina")

install.packages("visdat")
library(ggplot2)
library(visdat)

vis_miss(dataset)
vis_miss(dataset, cluster = TRUE) +
        theme(axis.text.x=element_text(size=rel(1.2), angle = 90))

dataset$diabetes <- ifelse(dataset$diabetes=="pos",1,0)
cor(dataset$pregnant,dataset$diabetes, method = "pearson")
