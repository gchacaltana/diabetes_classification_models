#########################################################################
### -- Maestria en Ciencia de Datos -- ## 
#########################################################################
### Autores:  Gonzalo Chacaltana ## 
### Tema:  Estudio comparativo de modelos de clasificación para identificar 
###        pacientes con diabetes
###
### BINOMIAL LINKS
###
#########################################################################

#### 1) PREPARAR ENTORNO DE TRABAJO

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

########### 1) LIBRERIAS A UTILIZAR ################# 

library(ggplot2)
library(caret)
library(VIM)
library(caret)
library(tidyverse)
library(mlr)
library(MLmetrics)
library(pROC)

########### 2) CARGA DE DATASET DE ENTRENAMIENTO Y PRUEBAS

## Data Train
datos_train <- read.csv("datos/diabetes_train.csv", sep=",")
datos_train <- datos_train[ , -1] # X
datos_train$diabetes <- ifelse(datos_train$diabetes=="pos",1,0)
summary(datos_train)
round(prop.table(table(datos_train$diabetes)), digits = 2)

datos_test <- read.csv("datos/diabetes_test.csv", sep=",")
datos_test <- datos_test[ , -1] # X
datos_test$diabetes <- ifelse(datos_test$diabetes=="pos",1,0)
summary(datos_test)
round(prop.table(table(datos_test$diabetes)), digits = 2)

source("functions.R")

### Normalización de variables ##
variables_cuantitativas <- datos_train %>% dplyr::select(glucose,pressure,triceps,insulin,mass,pedigree,age)
variables_cualitativas <- datos_train %>% dplyr::select(pregnant)

variables_cuantitativas <- lapply(variables_cuantitativas,function(x) (x - min(x)) / (max(x) - min(x) ))
variables_cuantitativas <- as.data.frame(variables_cuantitativas)
diabetes <- datos_train$diabetes

datos_modelo <- cbind(diabetes,variables_cuantitativas,variables_cualitativas)

########################################
##### MODELO CLOG LOG - NORMALIZACIÓN
########################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_cloglog <- glm(formula,family = binomial(link = cloglog), data = datos_modelo)

summary(modelo_cloglog)

# Predicción - Probabilidades
pred <- predict(modelo_cloglog, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_cloglog)

pcorte=0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.1403846

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5

########################################
##### MODELO LOGIT
########################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_logit <- glm(formula,family = binomial(link = logit), data = datos_modelo)

summary(modelo_logit)

# Predicción - Probabilidades
pred <- predict(modelo_logit, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_logit)

pcorte = 0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.1403846

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5

########################################
##### MODELO PROBIT
########################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_probit <- glm(formula,family = binomial(link = probit), data = datos_modelo)

summary(modelo_probit)

# Predicción - Probabilidades
pred <- predict(modelo_probit, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_probit)

pcorte = 0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.1403846

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5

###################################
### Estandarización de variables ###
##################################

variables_cuantitativas <- datos_train %>% dplyr::select(glucose,pressure,triceps,insulin,mass,pedigree,age)
variables_cualitativas <- datos_train %>% dplyr::select(pregnant)

variables_cuantitativas <- lapply(variables_cuantitativas,function(x) (x - mean(x)) / (sd(x) ))
variables_cuantitativas <- as.data.frame(variables_cuantitativas)
diabetes <- datos_train$diabetes

datos_modelo <- cbind(diabetes,variables_cuantitativas,variables_cualitativas)

####################################################
##### MODELO CLOG LOG - ESTANDARIZACIÓN DE VARIABLES
####################################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_cloglog <- glm(formula,family = binomial(link = cloglog), data = datos_modelo)

summary(modelo_cloglog)

# Predicción - Probabilidades
pred <- predict(modelo_cloglog, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_cloglog)

pcorte=0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.3491336

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5

########################################
##### MODELO LOGIT
########################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_logit <- glm(formula,family = binomial(link = logit), data = datos_modelo)

summary(modelo_logit)

# Predicción - Probabilidades
pred <- predict(modelo_logit, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_logit)

pcorte = 0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.1403846

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5

########################################
##### MODELO PROBIT
########################################

formula = diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

modelo_probit <- glm(formula,family = binomial(link = probit), data = datos_modelo)

summary(modelo_probit)

# Predicción - Probabilidades
pred <- predict(modelo_probit, newdata = datos_test, type = "response")

# Calculo de indicadores

calcula_indicadores(modelo_probit)

pcorte = 0.6

diabetes_predict_test <- ifelse(pred>=pcorte,1,0)

MLmetrics::Accuracy(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

MLmetrics::Recall(diabetes_predict_test,datos_test$diabetes)
# 0.3298969

#MLmetrics::Precision(diabetes_predict_test,datos_test$diabetes)
#MLmetrics::F1_Score(diabetes_predict_test,datos_test$diabetes)

MLmetrics::Gini(diabetes_predict_test,datos_test$diabetes)
# 0.1403846

MLmetrics::AUC(diabetes_predict_test,datos_test$diabetes)
# 0.5
