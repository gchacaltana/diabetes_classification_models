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

# función para mostrar tabla informativa con la cantidad de elementos y
# porcentajes para una variable de una tabla de contingencia.
printTable<-function(data) {
    table_data<-table(data)
    cbind_data<-cbind(table_data,round(prop.table(table_data)*100,2))
    colnames(cbind_data)<-c("Cantidad","Porcentaje")
    cbind_data
}

## GINI y KS

calcula_indicadores <- function(objeto_logit)
{
    SSE <-measureSSE(objeto_logit$y, objeto_logit$fitted.values)
    correlacion <- cor(objeto_logit$y, objeto_logit$fitted.values,method = 'spearman')
    out <- data.frame(variable="regresion",SSE,correlacion)
    return(out)
}

## Correlaciones

correlacionS <- function(m) {
    ut <- upper.tri(m)
    data.frame(i = rownames(m)[row(m)[ut]],
               j = rownames(m)[col(m)[ut]],
               cor=t(m)[ut])
}


# Pesos

pesos <- function(objeto_logit)
{
    coef_model<-data.frame(summary(objeto_logit)[["coefficients"]][, "t value"])
    colnames(coef_model)<-c("t_value")
    coef_model$t_value2<-coef_model$t_value^2
    coef_model$variable<-rownames(coef_model)
    coef_model<-coef_model[2:nrow(coef_model),]
    coef_model$variable2<-substr(coef_model$variable,1,nchar(coef_model$variable)-1)
    coef_model$total<-sum(coef_model$t_value2)
    coef_model$part<-coef_model$t_value2/coef_model$total
    datos1<-sqldf('select variable2, (sum(part))*100 as pesos 
              from coef_model 
              group by variable2 
              order by 2')
    return(datos1)
}