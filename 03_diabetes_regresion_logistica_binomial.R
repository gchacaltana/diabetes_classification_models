###########################################################################
### Modelo de Regresión Logística Binomial
###########################################################################
### Autor: Gonzalo Chacaltana
### Tema:  Estudio comparativo de modelos de clasificación para identificar 
###        pacientes con diabetes
###########################################################################

#### 1) PREPARAR ENTORNO DE TRABAJO

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

#### 1) LIBRERIAS A UTILIZAR ################# 
#install.packages("rJava")
#install.packages("xlsx")

library("rJava")
library("xlsx")

library(mlr)
library(ggplot2)
library(caret)

#### 2) CARGA DE DATASET DE ENTRENAMIENTO

load('datos/diabetes.rda')

source("functions.R")

library(ggplot2)
library(caret)
library(MLmetrics)
library(pROC)

# Creamos dataframe donde almacenaremos indicadores de los modelos.
remove(df_indicators)
df_indicators = data.frame()

# MODELO 1: variables ROC | normalizadas
# ---------------------------------------------

x = subset(dataset.train.roc.norm, select = -c(diabetes))
y = dataset.train.roc.norm$diabetes

formula <- paste('diabetes ~ ', paste(names(x), sep="", collapse=" + "))
formula

model <- glm(formula, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset.test.roc.norm, select = -c(diabetes)))

inds_rocs_norm <- calc_indicators_prob(pred = pred, y_true = dataset.test.roc.norm$diabetes)

df_indicators = rbind(df_indicators,inds_rocs_norm)

df_indicators


# MODELO 2: variables ROC | estandarizadas
# ---------------------------------------------

x = subset(dataset.train.roc.st, select = -c(diabetes))
y = dataset.train.roc.st$diabetes

formula <- paste('diabetes ~ ', paste(names(x), sep="", collapse=" + "))
formula

model <- glm(formula, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset.test.roc.st, select = -c(diabetes)))

inds_rocs_st <- calc_indicators_prob(pred = pred, y_true = dataset.test.roc.st$diabetes)

df_indicators = rbind(df_indicators,inds_rocs_st)

df_indicators


# MODELO 3: variables Boruta | normalizadas
# ---------------------------------------------

x = subset(dataset.train.boruta.norm, select = -c(diabetes))
y = dataset.train.boruta.norm$diabetes

formula <- paste('diabetes ~ ', paste(names(x), sep="", collapse=" + "))
formula

model <- glm(formula, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset.test.boruta.norm, select = -c(diabetes)))

inds_boruta_norm <- calc_indicators_prob(pred = pred, y_true = dataset.test.boruta.norm$diabetes)

df_indicators = rbind(df_indicators,inds_boruta_norm)

df_indicators


# MODELO 4: variables Boruta | estandarizadas
# ---------------------------------------------

x = subset(dataset.train.boruta.st, select = -c(diabetes))
y = dataset.train.boruta.st$diabetes

formula <- paste('diabetes ~ ', paste(names(x), sep="", collapse=" + "))
formula

model <- glm(formula, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset.test.boruta.st, select = -c(diabetes)))

inds_boruta_st <- calc_indicators_prob(pred = pred, y_true = dataset.test.boruta.st$diabetes)

df_indicators = rbind(df_indicators,inds_boruta_st)

df_indicators

# Generar archivo csv con indicadores
write.csv(round(df_indicators,4),"salidas/indicadores_rlb.csv",row.names = F)

## Pruebas de ajuste de bondad del modelo (parte 2)

## Evaluación de asociaciones para variables cuantitativas
## -------------------------------------------------------
#install.packages("pROC")
library("pROC")

###########################################################
# Utilizamos la curva roc para la variable Pregnant
rocPregnant = roc(dataset_train$diabetes ~ dataset_train$pregnant)
# Devuleve como resultado un area bajo la curva
rocPregnant
# Area under the curve: 0.5771 => 57.71%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocPregnant)
# 95% CI: 0.5039-0.6503 (DeLong) => Intervalo de confianza: de 50.39% a 65.03%

##########################################################

###########################################################
# Utilizamos la curva roc para la variable Glucose
rocGlucose = roc(dataset_train$diabetes ~ dataset_train$glucose)
# Devuleve como resultado un area bajo la curva
rocGlucose
# Area under the curve: 0.8214 => 82.14%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocGlucose)
# 95% CI: 0.7702-0.8727 (DeLong) => Intervalo de confianza: de 77.02% a 87.27%

##########################################################

###########################################################
# Utilizamos la curva roc para la variable Pressure
rocPressure = roc(dataset_train$diabetes ~ dataset_train$pressure)
# Devuleve como resultado un area bajo la curva
rocPressure
# Area under the curve: 0.6085 => 60.85%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocPressure)
# 95% CI: 0.5373-0.6796 (DeLong) => Intervalo de confianza: de 53.73% a 67.96%

##########################################################


###########################################################
# Utilizamos la curva roc para la variable Triceps
rocTriceps = roc(dataset_train$diabetes ~ dataset_train$triceps)
# Devuleve como resultado un area bajo la curva
rocTriceps
# Area under the curve: 0.6826 => 68.26%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocTriceps)
# 95% CI: 0.6199-0.7453 (DeLong) => Intervalo de confianza: de 61.99% a 74.53%

##########################################################


###########################################################
# Utilizamos la curva roc para la variable Insulin
rocInsulin = roc(dataset_train$diabetes ~ dataset_train$insulin)
# Devuleve como resultado un area bajo la curva
rocInsulin
# Area under the curve: 0.7632 => 76.32%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocInsulin)
# 95% CI: 0.706-0.8205 (DeLong) => Intervalo de confianza: de 70.60% a 82.05%

##########################################################


###########################################################
# Utilizamos la curva roc para la variable Mass
rocMass = roc(dataset_train$diabetes ~ dataset_train$mass)
# Devuleve como resultado un area bajo la curva
rocMass
# Area under the curve: 0.6704 => 67.04%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocMass)
# 95% CI: 0.6078-0.7331 (DeLong) => Intervalo de confianza: de 60.78% a 73.31%

##########################################################


###########################################################
# Utilizamos la curva roc para la variable Pedigree
rocPedigree = roc(dataset_train$diabetes ~ dataset_train$pedigree)
# Devuleve como resultado un area bajo la curva
rocPedigree
# Area under the curve: 0.6264 => 62.64%. < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocPedigree)
# 95% CI: 0.5585-0.6942 (DeLong) => Intervalo de confianza: de 55.85% a 69.42%

##########################################################


###########################################################
# Utilizamos la curva roc para la variable Age
rocAge = roc(dataset_train$diabetes ~ dataset_train$age)
# Devuleve como resultado un area bajo la curva
rocAge
# Area under the curve: 0.7177 => 71.77% < 98 casos
# Una variable es buen predictor si su area bajo la curva es mayor

# Obteniendo intervalo de confianza de la curva roc
ci.auc(rocAge)
# 95% CI: 0.6576-0.7779 (DeLong) => Intervalo de confianza: de 65.76% a 77.79%

##########################################################

# Mientras mayor es su area sobre la curva, se considera a la variable como mejor predictor.

par(mfrow=c(1,2))
plot(rocPregnant, main="Curva ROC - Variable Pregnant")
plot(rocGlucose, main="Curva ROC - Variable Glucose")
par(mfrow=c(1,2))

par(mfrow=c(1,2))
plot(rocPressure, main="Curva ROC - Variable Pressure")
plot(rocTriceps, main="Curva ROC - Variable Triceps")
par(mfrow=c(1,2))

par(mfrow=c(1,2))
plot(rocInsulin, main="Curva ROC - Variable Insulin")
plot(rocMass, main="Curva ROC - Variable Mass")
par(mfrow=c(1,2))

par(mfrow=c(1,2))
plot(rocPedigree, main="Curva ROC - Variable Pedigree")
plot(rocAge, main="Curva ROC - Variable Age")
par(mfrow=c(1,2))

#############################################################
## Generemos modelos de Regresión Logistica univariante
## ----------------------------------------------------

# Modelo RLB Pregnant
mu01 = glm(diabetes ~ pregnant,data = dataset_train, family=binomial)
summary(mu01)
# AIC: 369.41

# Modelo RLB Glucose
mu02 = glm(diabetes ~ glucose, data=dataset_train, family=binomial)
summary(mu02)
# AIC: 286.44

# Modelo RLB Pressure
mu03 = glm(diabetes ~ pressure, data=dataset_train, family=binomial)
summary(mu03)
# AIC: 369.13

# Modelo RLB Triceps
mu04 = glm(diabetes ~ triceps, data=dataset_train, family=binomial)
summary(mu04)
# AIC: 353.24

# Modelo RLB Insulin
mu05 = glm(diabetes ~ insulin, data=dataset_train, family=binomial)
summary(mu05)
# AIC: 331.60

# Modelo RLB Mass
mu06 = glm(diabetes ~ mass, data=dataset_train, family=binomial)
summary(mu06)
# AIC: 353.63

# Modelo RLB Pedigree
mu07 = glm(diabetes ~ pedigree, data=dataset_train, family=binomial)
summary(mu07)
# AIC: 362.81

# Modelo RLB Age
mu08 = glm(diabetes ~ age, data=dataset_train, family=binomial)
summary(mu08)
# AIC: 352.27

# Selección de variables por boruta
library(Boruta)
Boruta(diabetes~.,data=dataset_train,doTrace=2)->Bor.hvo;
plot(Bor.hvo,las=3);
Bor.hvo$finalDecision

############################################################################
# Modelo RLB Multivariante

# Modelo 01 (A)
formula_modelo_multi_01 <- diabetes ~ pregnant + glucose + pressure + triceps + insulin + mass + pedigree + age

mm01 = glm(formula_modelo_multi_01, data=dataset_train, family=binomial)
summary(mm01)
# AIC: 266.31

# Modelo 02 (B)
formula_modelo_multi_02 <- diabetes ~ glucose + insulin  + age + triceps

mm02 = glm(formula_modelo_multi_02, data=dataset_train, family=binomial)
summary(mm02)
# AIC: 1963.8

# Modelo 03 (C)
formula_modelo_multi_03 <- diabetes ~ glucose + insulin  + age

mm03 = glm(formula_modelo_multi_03, data=dataset_train, family=binomial)
summary(mm03)
# AIC: 282.52

# Modelo 04 (D)
formula_modelo_multi_04 <- diabetes ~ glucose + insulin

mm04 = glm(formula_modelo_multi_04, data=dataset_train, family=binomial)
summary(mm04)
# AIC: 285.44

# Modelo 05 (E)
formula_modelo_multi_05 <- diabetes ~ glucose + triceps + insulin + mass + pedigree + age

mm05 = glm(formula_modelo_multi_05, data=dataset_train, family=binomial)
summary(mm05)
# AIC: 263.25

# AIC (Criterio de Akaike) => nos dice que el modelo de predicción explica 
# mejor los datos cuando posee un valor mas pequeño


is_defined <- function(sym) {
    sym <- deparse(substitute(sym))
    #e <- environment()
    env <- globalenv()
    exists(sym, env)
}

calc_indicators <- function(pred, y_true){
    
    roc <- pROC::roc(y_true, pred, percent = TRUE)
    coords <- coords(roc, "best", ret="threshold", transpose = FALSE, best.method="youden")
    pcorte <- coords[1, 1]
    pred_value <- ifelse(pred >= pcorte, 1, 0)
    
    inds <- c(
        #these functions needs predicted values
        round(MLmetrics::Accuracy(pred_value, y_true),4),
        round(MLmetrics::Precision(pred_value, y_true),4),
        round(MLmetrics::Recall(pred_value, y_true),4),
        round(MLmetrics::Sensitivity(y_true, pred_value),4),
        round(MLmetrics::Specificity(y_true, pred_value),4),
        #these functions needs predicted probabilities
        round(MLmetrics::AUC(pred, y_true),4),
        round(MLmetrics::Gini(pred, as.numeric(y_true)),4)
    )
    return(inds)
}

# Modelo con variables Boruta
formula_boruta = diabetes ~ pregnant + glucose + pressure + triceps + 
    insulin + mass + pedigree + age

formula_roc = diabetes ~ glucose + triceps + insulin + age

x = subset(dataset_train, select = -c(diabetes))
y = dataset_train$diabetes

model <- glm(formula_boruta, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset_test, select = -c(diabetes)))

inds <- calc_indicators(pred = pred, y_true = dataset_test$diabetes)
inds

## Modelo RLB Curva ROC
model <- glm(formula_roc, data=cbind(x,diabetes = y), family=binomial)

pred <- predict(model, subset(dataset_test, select = -c(diabetes)))

inds <- calc_indicators(pred = pred, y_true = dataset_test$diabetes)
inds


#############################################################################
# Pruebas del Modelo
############################################################################

# Calculo del PSeudo R2 (cuadrado) ó Pseudo coeficiente de determinación (PCD) 
# mediante los valores estadísticos McFadden y Nagel

# PCD: Cuando su valor es más cercano a 1, la variabilidad de los datos está 
# explicada por el modelo
install.packages("DescTools")
library("DescTools")
PseudoR2(mm05,c("McFadden", "Nagel"))
PseudoR2(mm01,c("McFadden", "Nagel"))
PseudoR2(mm02,c("McFadden", "Nagel"))

# Modelo con mejor PCD NagelKerke es el Modelo 1 (A)

##########################################################################
# Test de Hosmer-Lemeshow: para evaluar bondad de ajuste del modelo 
# Test de Hosmer-Lemeshow: Es un test de hipotesis.

# H0 (Hipotesis Nula): Nos indica que el modelo es capaz de predecir los datos
# Ha (Hipotesis Alternativa): Nos indica que los modelos observados difieren de los modelos obtenidos a traves de la regresión 

install.packages("ResourceSelection")
library("ResourceSelection")
hoslem.test(dataset_train$diabetes,fitted(mm05))
# Resultado: p-value < 2.2e-16 (mejor modelo)
