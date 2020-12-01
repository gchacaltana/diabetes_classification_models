# función para crear archivo excel con el resultado de la asociacion
# de las variables cualitativas
addTableToExcel<-function(var_obj, var_pred, columnNames ,file_xlsx, sheet_name){
    a = table(var_pred,var_obj)
    b = round(prop.table(a,1)*100,2)
    dt <- data.frame(a[,1],b[,1],a[,2],b[,2],a[,1]+a[,2])
    colnames(dt)<-columnNames
    write.xlsx(dt, file = file_xlsx,sheetName = sheet_name, append = TRUE, col.names = TRUE)
    a
}

# Función para mostrar tabla informativa con la cantidad de elementos y
# porcentajes para una variable de una tabla de contingencia.
printTable<-function(data) {
    table_data<-table(data)
    cbind_data<-cbind(table_data,round(prop.table(table_data)*100,2))
    colnames(cbind_data)<-c("Cantidad","Porcentaje")
    cbind_data
}

# Función para calcular indicadores de probabilidad.
#Accuracy(y_pred, y_true)
#Precision(y_true, y_pred, positive = NULL)
#Recall(y_true, y_pred, positive = NULL)
#Sensitivity(y_true, y_pred, positive = NULL)
#Specificity(y_true, y_pred, positive = NULL)
#F1_Score(y_true, y_pred, positive = NULL)
#AUC(y_pred, y_true)
#Gini(y_pred, y_true)
#KS_Stat(y_pred, y_true)
calc_indicators_prob <- function(pred, y_true){
    roc <- pROC::roc(y_true, pred, percent = TRUE)
    coords <- coords(roc, "best", ret="threshold", transpose = FALSE, best.method="youden")
    pcorte <- coords[1, 1]
    pred_value <- ifelse(pred >= pcorte, 1, 0)
    inds <- c(
        MLmetrics::Accuracy(pred_value, y_true),
        MLmetrics::Precision(y_true, pred_value, positive = 1),
        MLmetrics::Recall(y_true, pred_value, positive = 1),
        MLmetrics::Sensitivity(y_true, pred_value, positive = 1),
        MLmetrics::Specificity(y_true, pred_value, positive = 1),
        MLmetrics::F1_Score(y_true, pred_value, positive = 1),
        MLmetrics::AUC(pred, as.numeric(y_true)),
        MLmetrics::Gini(pred, as.numeric(y_true)),
        MLmetrics::KS_Stat(pred, as.numeric(y_true))
    )
    return(inds)
}

# Función para aplicar la transformación de datos (estandarización)
standardize_data <- function(x){
    (x-mean(x))/(sd(x))
}

# Función para aplicar la transformación de datos (normalización)
normalize_data <- function(x){
    (x-min(x))/(max(x)-min(x))
}