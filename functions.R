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

printTable<-function(data) {
    table_data<-table(data)
    cbind_data<-cbind(table_data,round(prop.table(table_data)*100,2))
    colnames(cbind_data)<-c("Cantidad","Porcentaje")
    cbind_data
}