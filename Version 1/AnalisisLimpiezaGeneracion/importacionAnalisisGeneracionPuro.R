#autor: amanosalva
#Data generada con las correcciones de José Coca y José Neyra
#Script que genera dataset puro, a partir del dataset crudo.
#Activar librería ByteAnalyticsUtils

#Importación de data cruda.
library(readr)
V4DATOFIN <- read_csv("C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/RAW DATA/Corrección Coca - Neyra/V4DATOFIN_raw_cor.csv", 
                      col_types = list("costpl_n" = col_double(), "cnttoc_n"= col_double(), 
                                       "cntnrc_n"= col_double(), "cnttmc_n"= col_double(), 
                                       "fld139_n"= col_double(), "fld141_n"= col_double(), 
                                       "fld143_n"= col_double(), "fld145_n"= col_double(), 
                                       "fld146_n"= col_double(),
                                                                                                                                                                "cant1m_n"= col_double(), "mont1m_n"= col_double(), "cant3m_n"= col_double(), "mont3m_n"= col_double(), "cant6m_n"= col_double(), "mont6m_n"= col_double(), "ctotal_n"= col_double(), "mtotal_n"= col_double()))
V4DATOFIN_SAVE <- V4DATOFIN
V4DATOFIN <- eliminaCaracteristica(V4DATOFIN, c("X1"))


#Visualización de campos nulos.
exportaValoresFaltantes(dataset = V4DATOFIN, path = "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/0. Generación del dataset/CuadroResumenNA_cor_.csv")


#Visualización general de la data.
#Cabecera
exporta100primeros(V4DATOFIN,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/0. Generación del dataset/V4DATOFIN_HEAD_cor.csv")

#Cola
exporta100ultimos(V4DATOFIN,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Documentos/0. Generación del dataset/V4DATOFIN_TAIL_cor.csv")

#Características a eliminar (esta eliminación surgió por el análisis con técnicas de filtrado y bajo criterio de los encargados).
caracteristicasInutiles<-c("tcnfol_n","tcntel_n","genero_c","ecivil_c","X1_1","X2","X3","serietd_c",
                           "rec1to_n","rec1fi_n","rec1pr_n","rec1np_n","rec1an_n",
                           "rec1pe_n","rec1to_1_n","rec1fi_1_n","rec1pr_1_n",
                           "rec1np_1_n","rec1an_1_n","rec1pe_1_n","rec1to_2_n",
                           "rec1fi_2_n","rec1pr_2_n","rec1np_2_n","rec1an_2_n",
                           "rec1pe_2_n","rec2to_n","rec2fi_n","rec2pr_n","rec2np_n",
                           "rec2an_n","rec2pe_n","rec2to_1_n","rec2fi_1_n","rec2pr_1_n",
                           "rec2np_1_n","rec2an_1_n","rec2pe_1_n","rec2to_2_n","rec2fi_2_n",
                           "rec2pr_2_n","rec2np_2_n","rec2an_2_n","rec2pe_2_n","rec3to_n",
                           "rec3fi_n","rec3pr_n","rec3np_n","rec3an_n","rec3pe_n",
                           "rec3to_1_n","rec3fi_1_n","rec3pr_1_n","rec3np_1_n","rec3an_1_n",
                           "rec3pe_1_n","rec3to_2_n","rec3fi_2_n","rec3pr_2_n","rec3np_2_n",
                           "rec3an_2_n","rec3pe_2_n","rec4to_n","rec4fi_n","rec4pr_n",
                           "rec4np_n","rec4an_n","rec4pe_n","rec4to_1_n","rec4fi_1_n",
                           "rec4pr_1_n","rec4np_1_n","rec4an_1_n","rec4pe_1_n","rec4to_2_n",
                           "rec4fi_2_n","rec4pr_2_n","rec4np_2_n","rec4an_2_n","rec4pe_2_n",
                           "tccpro_c","tcclip_c","tcctse_c","tctico_c","cordir_n","tccopl_n",
                           "tcinyn_c","ivtipi_c","tcsera_c","tcsecn_c","tccesq_n","tcaesq_n",
                           "tcmesq_n","tcdesq_n","ivgase_c","tcdsga_c","tcccli_n","tccdia_n",
                           "tccmes_n","tccano_n","tcndia_n","tcnmes_n","tcnano_n","tcesci_c",
                           "tcsexo_c","rep1to_n","rep1fi_n","rep1an_n","rep1pe_n","rep1to_1_n",
                           "rep1fi_1_n","rep1an_1_n","rep1pe_1_n","rep1to_2_n","rep1fi_2_n",
                           "rep1an_2_n","rep1pe_2_n","rep2to_n","rep2fi_n","rep2an_n","rep2pe_n",
                           "rep2to_1_n","rep2fi_1_n","rep2an_1_n","rep2pe_1_n","rep2to_2_n",
                           "rep2fi_2_n","rep2an_2_n","rep2pe_2_n","rep3to_n","rep3fi_n","rep3an_n",
                           "rep3pe_n","rep3to_1_n","rep3fi_1_n","rep3an_1_n","rep3pe_1_n","rep3to_2_n",
                           "rep3fi_2_n","rep3an_2_n","rep3pe_2_n","rep4to_n","rep4fi_n","rep4an_n",
                           "rep4pe_n","rep4to_1_n","rep4fi_1_n","rep4an_1_n","rep4pe_1_n","rep4to_2_n",
                           "rep4fi_2_n","rep4an_2_n","rep4pe_2_n","f3_n","f4_n","f5_n","f6_n","f7_n",
                           "f8_n","f9_n","f10_n","f11_n","f12_n","f13_n","f14_n","f15_n","f16_n","f17_n",
                           "f18_n","fld042_n","fld043_n","fld044_n","fld046_n","fld047_n","fld048_n",
                           "fld050_n","fld051_n","fld052_n","fld074_n","fld075_n","fld076_n", "disti_c")

length(caracteristicasInutiles)
#182 características
V4DATOFIN <- eliminaCaracteristica(V4DATOFIN,caracteristicasInutiles)
names(V4DATOFIN)
rm(caracteristicasInutiles)





#Transformación de los registros NA a 0 en características que expresan cantidad, (según lo documentado como comentario en la sección CuadroResumenNA).
V4DATOFIN$cant1m_n <- ifelse(is.na(V4DATOFIN$cant1m_n), 0, V4DATOFIN$cant1m_n)
sum(is.na(V4DATOFIN$cant1m_n))
V4DATOFIN$mont1m_n <- ifelse(is.na(V4DATOFIN$mont1m_n), 0, V4DATOFIN$mont1m_n)
sum(is.na(V4DATOFIN$mont1m_n))
V4DATOFIN$cant3m_n <- ifelse(is.na(V4DATOFIN$cant3m_n), 0, V4DATOFIN$cant3m_n)
sum(is.na(V4DATOFIN$cant3m_n))
V4DATOFIN$mont3m_n <- ifelse(is.na(V4DATOFIN$mont3m_n), 0, V4DATOFIN$mont3m_n)
sum(is.na(V4DATOFIN$mont3m_n))
V4DATOFIN$cant6m_n <- ifelse(is.na(V4DATOFIN$cant6m_n), 0, V4DATOFIN$cant6m_n)
sum(is.na(V4DATOFIN$cant6m_n))
V4DATOFIN$mont6m_n <- ifelse(is.na(V4DATOFIN$mont6m_n), 0, V4DATOFIN$mont6m_n)
sum(is.na(V4DATOFIN$mont6m_n))
V4DATOFIN$ctotal_n <- ifelse(is.na(V4DATOFIN$ctotal_n), 0, V4DATOFIN$ctotal_n)
sum(is.na(V4DATOFIN$ctotal_n))
V4DATOFIN$mtotal_n <- ifelse(is.na(V4DATOFIN$mtotal_n), 0, V4DATOFIN$mtotal_n)
sum(is.na(V4DATOFIN$mtotal_n))
V4DATOFIN$costpl_n <- ifelse(is.na(V4DATOFIN$costpl_n), 0, V4DATOFIN$costpl_n)
sum(is.na(V4DATOFIN$costpl_n))
V4DATOFIN$cnttoc_n <- ifelse(is.na(V4DATOFIN$cnttoc_n), 0, V4DATOFIN$cnttoc_n)
sum(is.na(V4DATOFIN$cnttoc_n))
V4DATOFIN$cntnrc_n <- ifelse(is.na(V4DATOFIN$cntnrc_n), 0, V4DATOFIN$cntnrc_n)
sum(is.na(V4DATOFIN$cntnrc_n))
V4DATOFIN$cnttmc_n <- ifelse(is.na(V4DATOFIN$cnttmc_n), 0, V4DATOFIN$cnttmc_n)
sum(is.na(V4DATOFIN$cnttmc_n))


#Proceso de purificación
#Eliminamos las características mapeadas innecesarias:
V4DATOFIN<- eliminaCaracteristica(V4DATOFIN, c("X1_1"))
names(V4DATOFIN)

#De lo analizado en el documento que muestra los valores faltantes, se crea un nuevo dataset sin valores faltantes.
V4DATOFIN_pure<- subset(V4DATOFIN, !is.na(V4DATOFIN$edad_n) &
                              !is.na(V4DATOFIN$antig_n) & !is.na(V4DATOFIN$numlin_n) &
                              !is.na(V4DATOFIN$gama_c) & !is.na(V4DATOFIN$eqaseg_c) &
                              !is.na(V4DATOFIN$eqanti_n) & !is.na(V4DATOFIN$provi_c) & !is.na(V4DATOFIN$depart_c))

#Verificación

sum(is.na(V4DATOFIN_pure$cant1m_n))
sum(is.na(V4DATOFIN_pure$mont1m_n))
sum(is.na(V4DATOFIN_pure$cant3m_n))
sum(is.na(V4DATOFIN_pure$mont3m_n))
sum(is.na(V4DATOFIN_pure$cant6m_n))
sum(is.na(V4DATOFIN_pure$mont6m_n))
sum(is.na(V4DATOFIN_pure$ctotal_n))
sum(is.na(V4DATOFIN_pure$mtotal_n))
sum(is.na(V4DATOFIN_pure$costpl_n))
sum(is.na(V4DATOFIN_pure$cnttoc_n))
sum(is.na(V4DATOFIN_pure$cntnrc_n))
sum(is.na(V4DATOFIN_pure$cnttmc_n))
#Hay algún NA en toda la data pura?
sum(is.na(V4DATOFIN_pure))


#Eliminación de registros duplicados en la data pura:
V4DATOFIN_pure <- eliminaDuplicados(V4DATOFIN_pure)

#Se mueve el target al final
target_c_temp <- V4DATOFIN_pure$target_c
V4DATOFIN_pure <- eliminaCaracteristica(V4DATOFIN_pure, c("target_c"))
target_c <- target_c_temp
V4DATOFIN_pure <- cbind(V4DATOFIN_pure, target_c)
rm(target_c, target_c_temp)
names(V4DATOFIN_pure)


#Aplicando algoritmo de balanceo de datos: Oversampling
#Instalando y activando paquetes necesarios para el balanceo
paquetes <- c("unbalanced")
usarPaquetes(paquetes)

#Separando en output (target), input (las demás características)
v4datofin_pure_size <- ncol(V4DATOFIN_pure)
output <- V4DATOFIN_pure[,v4datofin_pure_size]
input <- V4DATOFIN_pure[,-v4datofin_pure_size]
data <- ubBalance(X=input, Y=as.factor(output), type="ubOver", k=0)
#oversampled dataset
overData <- data.frame(data$X, target_c=data$Y)
#Ver distribución del target
barplot(table(overData$target_c))



V4DATOFIN_pure <- overData
rm(input,output,overData,data,v4datofin_pure_size)
names(V4DATOFIN_pure)

#Exportación de los 100 primeros y 100 últimos
exporta100primeros(V4DATOFIN_pure, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_HEAD.csv")
exporta100ultimos(V4DATOFIN_pure, "C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure_TAIL.csv")


#Exportación de la data
write.csv(V4DATOFIN_pure,"C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.puro/V4DATOFIN_pure.csv", row.names = FALSE)

