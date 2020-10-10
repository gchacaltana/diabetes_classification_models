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
