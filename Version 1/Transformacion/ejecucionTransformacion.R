#Prueba BATCH:
path_origen <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure.csv"
path_destino <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.final/V4DATOFIN_v1_pure_final.csv"
batch <- transformacion(path_origen,path_destino,"","","B")

#Prueba ONLINE:
#Simulaci?n de data:
trama <- "32;85;1;control;PREMIUM;n;11;s;59.99;n;12;4;8;4;12;0;3;0;0;0;0;3;0;0;513;16;3;68;411;68;101;25;0;0;0;0;2372125;2372125;0;0;173;107;8;2;0;1;1;1;N;N;N;N;3;0.08;1;0.05;1;0.02;1;0.01;3862.14;N;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;LIMA;LIMA;0"
system.time(transformacion("","",trama,";","O"))
online <- transformacion("","",trama,";","O")


#Simulando entrada de varios datos:
#V4DATOFIN_v1_pure_coma <- read.csv("C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.1/Data pura v1/V4DATOFIN_v1_pure.csv", sep = ";")

#vector <- c()
#for(i in 1:20){
#  trama <- as.character(unlist(V4DATOFIN_v1_pure_coma[i,]))
#  trama <- gsub(",",";",trama)
#  
#  #Aplica la funci?n:
#  row <- transformacion("","",trama,"","O")
#  
#  #apilando registros para csv:
#  vector[i] <- do.call(paste, c(row, sep = ","))
#  
#}
#vector
#c <- as.data.frame(vector)

