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

rm(path_origen)
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


#Análisis de cada variable.

comparaVariablesNumericas("edad_n")
comparaVariablesNumericas("antig_n")
comparaVariablesNumericas("numlin_n")
comparaVariablesCategoricas("gama_c")
comparaVariablesCategoricas("eqaseg_c")
comparaVariablesNumericas("eqanti_n")
comparaVariablesCategoricas("linper_c")
comparaVariablesCategoricas("costpl_n")
comparaVariablesNumericas("costpl_n")
comparaVariablesCategoricas("conren_c")
comparaVariablesNumericas("cnttoc_n")
comparaVariablesCategoricas("cnttoc_n")
comparaVariablesNumericas("cntnrc_n")
comparaVariablesCategoricas("cntnrc_n")
comparaVariablesNumericas("cnttmc_n")
comparaVariablesCategoricas("cnttmc_n")
comparaVariablesNumericas("cntctd_n")
comparaVariablesCategoricas("cntctd_n")
comparaVariablesNumericas("cntctl_n")
comparaVariablesCategoricas("cntctl_n")
comparaVariablesNumericas("cntctl_1_n")
comparaVariablesCategoricas("cntctl_1_n")
comparaVariablesNumericas("cntctl_2_n")
comparaVariablesCategoricas("cntctl_2_n")
comparaVariablesNumericas("cntsol_n")
comparaVariablesCategoricas("cntsol_n")
comparaVariablesNumericas("f19_n")
comparaVariablesCategoricas("f19_n")
comparaVariablesNumericas("f20_n")
comparaVariablesCategoricas("f20_n")
comparaVariablesNumericas("f21_n")
comparaVariablesCategoricas("f21_n")
comparaVariablesNumericas("fld041_n")
comparaVariablesCategoricas("fld041_n")
comparaVariablesNumericas("fld045_n")
comparaVariablesCategoricas("fld045_n")
comparaVariablesNumericas("fld049_n")
comparaVariablesCategoricas("fld049_n")
comparaVariablesNumericas("fld053_n")
comparaVariablesCategoricas("fld053_n")
comparaVariablesNumericas("fld054_n")
comparaVariablesCategoricas("fld054_n")
comparaVariablesNumericas("fld055_n")
comparaVariablesCategoricas("fld055_n")
comparaVariablesNumericas("fld056_n")
comparaVariablesCategoricas("fld056_n")
comparaVariablesNumericas("fld057_n")
comparaVariablesCategoricas("fld057_n")
comparaVariablesNumericas("fld058_n")
comparaVariablesCategoricas("fld058_n")
comparaVariablesNumericas("fld059_n")
comparaVariablesCategoricas("fld059_n")
comparaVariablesNumericas("fld060_n")
comparaVariablesCategoricas("fld060_n")
comparaVariablesNumericas("fld061_n")
comparaVariablesCategoricas("fld061_n")
comparaVariablesNumericas("fld062_n")
comparaVariablesCategoricas("fld062_n")
comparaVariablesNumericas("fld063_n")
comparaVariablesCategoricas("fld063_n")
comparaVariablesNumericas("fld064_n")
comparaVariablesCategoricas("fld064_n")
comparaVariablesNumericas("fld065_n")
comparaVariablesCategoricas("fld065_n")
comparaVariablesNumericas("fld066_n")
comparaVariablesCategoricas("fld066_n")
comparaVariablesNumericas("fld067_n")
comparaVariablesCategoricas("fld067_n")
comparaVariablesNumericas("fld068_n")
comparaVariablesCategoricas("fld068_n")
comparaVariablesNumericas("fld069_n")
comparaVariablesCategoricas("fld069_n")
comparaVariablesNumericas("fld070_n")
comparaVariablesCategoricas("fld070_n")
comparaVariablesNumericas("fld071_n")
comparaVariablesCategoricas("fld071_n")
comparaVariablesNumericas("fld072_n")
comparaVariablesCategoricas("fld072_n")
comparaVariablesNumericas("fld073_n")
comparaVariablesCategoricas("fld073_n")
comparaVariablesNumericas("fld115_n")
comparaVariablesCategoricas("fld115_n")
comparaVariablesNumericas("fld116_n")
comparaVariablesCategoricas("fld116_n")
comparaVariablesNumericas("fld117_n")
comparaVariablesCategoricas("fld117_n")
comparaVariablesCategoricas("fld119_c")
comparaVariablesCategoricas("fld120_c")
comparaVariablesCategoricas("fld121_c")
comparaVariablesCategoricas("fld122_c")
comparaVariablesNumericas("fld138_n")
comparaVariablesCategoricas("fld138_n")
comparaVariablesNumericas("fld139_n")
comparaVariablesCategoricas("fld139_n")
comparaVariablesNumericas("fld140_n")
comparaVariablesCategoricas("fld140_n")
comparaVariablesNumericas("fld141_n")
comparaVariablesCategoricas("fld141_n")
comparaVariablesNumericas("fld142_n")
comparaVariablesCategoricas("fld142_n")
comparaVariablesNumericas("fld143_n")
comparaVariablesCategoricas("fld143_n")
comparaVariablesNumericas("fld144_n")
comparaVariablesCategoricas("fld144_n")
comparaVariablesNumericas("fld145_n")
comparaVariablesCategoricas("fld145_n")
comparaVariablesNumericas("fld146_n")
comparaVariablesCategoricas("fld146_n")
comparaVariablesCategoricas("pli_sn_c")
comparaVariablesNumericas("cant1m_n")
comparaVariablesCategoricas("cant1m_n")
comparaVariablesNumericas("mont1m_n")
comparaVariablesCategoricas("mont1m_n")
comparaVariablesNumericas("cant3m_n")
comparaVariablesCategoricas("cant3m_n")
comparaVariablesNumericas("mont3m_n")
comparaVariablesCategoricas("mont3m_n")
comparaVariablesNumericas("cant6m_n")
comparaVariablesCategoricas("cant6m_n")
comparaVariablesNumericas("mont6m_n")
comparaVariablesCategoricas("mont6m_n")
comparaVariablesNumericas("ctotal_n")
comparaVariablesCategoricas("ctotal_n")
comparaVariablesNumericas("mtotal_n")
comparaVariablesCategoricas("mtotal_n")
comparaVariablesNumericas("rec1to_3_n")
comparaVariablesCategoricas("rec1to_3_n")
comparaVariablesNumericas("rec2to_3_n")
comparaVariablesCategoricas("rec2to_3_n")
comparaVariablesNumericas("rec3to_3_n")
comparaVariablesCategoricas("rec3to_3_n")
comparaVariablesNumericas("rec4to_3_n")
comparaVariablesCategoricas("rec4to_3_n")
comparaVariablesNumericas("rep1to_3_n")
comparaVariablesCategoricas("rep1to_3_n")
comparaVariablesNumericas("rep2to_3_n")
comparaVariablesCategoricas("rep2to_3_n")
comparaVariablesNumericas("rep3to_3_n")
comparaVariablesCategoricas("rep3to_3_n")
comparaVariablesNumericas("rep4to_3_n")
comparaVariablesCategoricas("rep4to_3_n")
comparaVariablesCategoricas("target_c")

#Análisis adicionales:

length(V4DATOFIN_pure_prepago$linper_c[V4DATOFIN_pure_prepago$linper_c == "s" ])
length(V4DATOFIN_pure_prepago$linper_c[V4DATOFIN_pure_prepago$linper_c == "s" & V4DATOFIN_pure_prepago$target_c == 0])
length(V4DATOFIN_pure_prepago$linper_c[V4DATOFIN_pure_prepago$linper_c == "s" & V4DATOFIN_pure_prepago$target_c == 1])


length(V4DATOFIN_pure_prepago$fld116_n[V4DATOFIN_pure_prepago$fld116_n > 9999 & V4DATOFIN_pure_prepago$target_c == "1"])


length(V4DATOFIN_pure_prepago$fld116_n[V4DATOFIN_pure_prepago$fld116_n > 9999 & V4DATOFIN_pure_prepago$target_c == "1"])

length(V4DATOFIN_pure_postpago$pli_sn_c[V4DATOFIN_pure_postpago$pli_sn_c == "S"])
length(V4DATOFIN_pure_control$pli_sn_c[V4DATOFIN_pure_control$pli_sn_c == "S"])

#Selección de variables usando el algoritmo Boruta
library(Boruta)
usarPaquetes(c("Boruta"))
boruta_output <- Boruta(target_c ~ ., data=na.omit(V4DATOFIN_pure_postpago), doTrace=2)

boruta_output <- Boruta(target_c ~ ., data=na.omit(V4DATOFIN_pure_prepago), doTrace=2)


write.csv(V4DATOFIN_pure_prepago, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_prepago.csv")
write.csv(V4DATOFIN_pure_postpago, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_postpago.csv")
write.csv(V4DATOFIN_pure_control, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_control.csv")



