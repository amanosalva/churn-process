#Función que compara 3 gráficos para una variable numérica en los datasets prepago, postpago, control
comparaVariablesNumericas <- function(nombreVariable){
  
  par(mfrow=c(3,1))
  
  hist(unlist(V4DATOFIN_pure_prepago[names(V4DATOFIN_pure) == nombreVariable]), main = paste(nombreVariable," - Prepago"), xlab = nombreVariable, ylab = "Frecuencia")
  hist(unlist(V4DATOFIN_pure_postpago[names(V4DATOFIN_pure) == nombreVariable]), main = paste(nombreVariable," - Postpago"), xlab = nombreVariable, ylab = "Frecuencia")
  hist(unlist(V4DATOFIN_pure_control[names(V4DATOFIN_pure) == nombreVariable]), main = paste(nombreVariable," - Control"), xlab = nombreVariable, ylab = "Frecuencia")
  
  
}