#Función que compara 3 gráficos para una variable categórica en los datasets prepago, postpago, control
comparaVariablesCategoricas <- function(nombreVariable){
  
  par(mfrow=c(3,1))
  
  barplot(table(V4DATOFIN_pure_prepago[names(V4DATOFIN_pure) == nombreVariable]))
  title(paste(nombreVariable," - Prepago"))
  barplot(table(V4DATOFIN_pure_postpago[names(V4DATOFIN_pure) == nombreVariable]))
  title(paste(nombreVariable," - Postpago"))
  barplot(table(V4DATOFIN_pure_control[names(V4DATOFIN_pure) == nombreVariable]))
  title(paste(nombreVariable," - Control"))
  
  
}