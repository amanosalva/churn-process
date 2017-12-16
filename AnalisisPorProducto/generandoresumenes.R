#Analizando data pura para prepagos y postpagos.
library(readr)
path_origen <- "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure.csv"
V4DATOFIN_pure <- read_csv(path_origen, 
                           col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                            "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                            "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                            "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                            "fld146_n"= col_double(), "cant1m_n"= col_double(), 
                                            "mont1m_n"= col_double(), "cant3m_n"= col_double(), 
                                            "mont3m_n"= col_double(), "cant6m_n"= col_double(), 
                                            "mont6m_n"= col_double(), "ctotal_n"= col_double(), 
                                            "mtotal_n"= col_double()))

#Separando prepago de los postpago y control.



V4DATOFIN_pure_prepago<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == "prepago")
V4DATOFIN_pure_postpago<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == "postpago")
V4DATOFIN_pure_control<- subset(V4DATOFIN_pure, V4DATOFIN_pure$produc_c == "control")



#Resumen
V4DATOFIN_pure_prepago_resumen <- summary(V4DATOFIN_pure_prepago)
summary(V4DATOFIN_pure_prepago)
write.table(V4DATOFIN_pure_prepago_resumen, 'V4DATOFIN_pure_prepago_resumen.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)

V4DATOFIN_pure_postpago_resumen <- summary(V4DATOFIN_pure_postpago)
write.table(V4DATOFIN_pure_postpago_resumen, 'V4DATOFIN_pure_postpago_resumen.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)

V4DATOFIN_pure_control_resumen <- summary(V4DATOFIN_pure_control)
write.table(V4DATOFIN_pure_control_resumen, 'V4DATOFIN_pure_control_resumen.csv', sep=",", row.names=FALSE, col.names=TRUE, quote=FALSE)


#Selección de variables usando el algoritmo Boruta
library(Boruta)
library(ByteAnalitycsUtils)
usarPaquetes(c("Boruta"))
boruta_output_postpago <- Boruta(target_c ~ ., data=na.omit(V4DATOFIN_pure_postpago), doTrace=2)

boruta_output_prepago <- Boruta(target_c ~ ., data=na.omit(V4DATOFIN_pure_prepago), doTrace=2)


write.csv(V4DATOFIN_pure_prepago, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_prepago.csv")
write.csv(V4DATOFIN_pure_postpago, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_postpago.csv")
write.csv(V4DATOFIN_pure_control, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_control.csv")



