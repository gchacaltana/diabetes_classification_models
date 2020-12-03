#########################################################################
### Autor:  Gonzalo Chacaltana ## 
### MODELO CLOG LOG
#########################################################################

#### 1) PREPARAR ENTORNO DE TRABAJO

rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

########### 1) LIBRERIAS A UTILIZAR ################# 

library(MLmetrics)
library(pROC)

#### 2) CARGA DE DATASET DE ENTRENAMIENTO

load('datos/diabetes.rda')

source("functions.R")

# Creamos dataframe donde almacenaremos indicadores de los modelos.
remove(df_indicators)
df_indicators = data.frame()

# MODELO PROBIT

# MODELO 1: variables ROC | normalizadas
# ---------------------------------------------

x = subset(dataset.train.roc.norm, select = -c(diabetes))
y = dataset.train.roc.norm$diabetes

formula <- paste('diabetes ~ ', paste(names(x), sep="", collapse=" + "))
formula

model <- glm(formula, data=cbind(x,diabetes = y), binomial(link = "cloglog"))

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

model <- glm(formula, data=cbind(x,diabetes = y), binomial(link = "cloglog"))

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

model <- glm(formula, data=cbind(x,diabetes = y), binomial(link = "cloglog"))

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

model <- glm(formula, data=cbind(x,diabetes = y), binomial(link = "cloglog"))

pred <- predict(model, subset(dataset.test.boruta.st, select = -c(diabetes)))

inds_boruta_st <- calc_indicators_prob(pred = pred, y_true = dataset.test.boruta.st$diabetes)

df_indicators = rbind(df_indicators,inds_boruta_st)

# Generar archivo csv con indicadores
write.csv(round(df_indicators,4),"salidas/indicadores_cloglog.csv",row.names = F)
