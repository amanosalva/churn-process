#@author: amanosalva
#@function: transformacion(path_origen, path_destino, trama, token, opcion)
#@parameters: 
## - path_origen: Direcci?n origen desde donde se leer? el dataset en formato csv. V?lido para opcion="B"
## - path_destino: Direcci?n destino donde se guardar? el dataset transformado. V?lido para opcion="B"
## - trama: Cadena que contiene un registro, donde cada caracter?stica estar? separado por token. V?lido para opcion="O"
## - token: Separador en la trama. V?lido para opci?n="O"
## - opcion: Valores posibles: "B":Batch , "O":Online.
##          Si se elije la opci?n por Batch (B), R cargar? el dataset completo desde la ruta path_origen y exportar? el dataset resultante a path_destino. En este caso, la funci?n retorna null.
##          Si se elije la opci?n Online (O), R recibir? como par?metro la trama representando un registro, separado por el token y retornar? como cadena la trama transformada, separado por coma.
#Observaciones: El path (para ambos casos), incluye el nombre del archivo con la extensi?n, adem?s, como separador se utiliza "/".


transformacion <- function(path_origen, path_destino, trama, token, opcion){
  
  out <- tryCatch(
    {  
      #Función para eliminar características innecesarias.
      eliminaCaracteristica <- function(dataset, caracteristicas){
        dataset <- dataset[, !(names(dataset) %in% caracteristicas)]
        return(dataset)
      }
      
      
      
      switch(opcion, 
             B = {
               library(readr)
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
               
               
             },
             
             O = {
               
               #Función para recibir tramas asignándole los nombres específicos del dataset puro.
               recibeTrama <- function(trama,tokenp){
                 registro <- as.data.frame(do.call(rbind,strsplit(trama,tokenp)))
                 names(registro) <- c("edad_n","antig_n","numlin_n","produc_c", 
                                      "gama_c","eqaseg_c","eqanti_n","linper_c",
                                      "costpl_n","conren_c","cnttoc_n","cntnrc_n",
                                      "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n",
                                      "cntctl_2_n","cntsol_n","f19_n","f20_n","f21_n",
                                      "fld041_n","fld045_n","fld049_n","fld053_n",
                                      "fld054_n","fld055_n","fld056_n","fld057_n",
                                      "fld058_n","fld059_n","fld060_n","fld061_n",
                                      "fld062_n","fld063_n","fld064_n","fld065_n",
                                      "fld066_n","fld067_n","fld068_n","fld069_n",
                                      "fld070_n","fld071_n","fld072_n","fld073_n",
                                      "fld115_n","fld116_n","fld117_n","fld119_c",
                                      "fld120_c","fld121_c","fld122_c","fld138_n",
                                      "fld139_n","fld140_n","fld141_n","fld142_n",
                                      "fld143_n","fld144_n","fld145_n","fld146_n",
                                      "pli_sn_c","cant1m_n","mont1m_n","cant3m_n",
                                      "mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                                      "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n",
                                      "rec4to_3_n","rep1to_3_n","rep2to_3_n","rep3to_3_n",
                                      "rep4to_3_n","depart_c","provi_c","target_c")
                 #Asignación del tipo de dato. Convierte factor a numérico
                 registro[,c("edad_n","antig_n","numlin_n", "eqanti_n","costpl_n","cnttoc_n","cntnrc_n",
                             "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n","cntctl_2_n","cntsol_n",
                             "f19_n","f20_n","f21_n","fld041_n","fld045_n","fld049_n","fld053_n",
                             "fld054_n","fld055_n","fld056_n","fld057_n","fld058_n","fld059_n",
                             "fld060_n","fld061_n","fld062_n","fld063_n","fld064_n","fld065_n",
                             "fld066_n","fld067_n","fld068_n","fld069_n","fld070_n","fld071_n",
                             "fld072_n","fld073_n","fld115_n","fld116_n","fld117_n", "fld138_n","fld139_n","fld140_n","fld141_n",
                             "fld142_n","fld143_n","fld144_n","fld145_n","fld146_n","cant1m_n",
                             "mont1m_n","cant3m_n","mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                             "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n","rec4to_3_n",
                             "rep1to_3_n","rep2to_3_n","rep3to_3_n","rep4to_3_n")] <- as.numeric(as.character(unlist(registro[,c("edad_n","antig_n","numlin_n","eqanti_n","costpl_n","cnttoc_n","cntnrc_n",
                                                                                                                                 "cnttmc_n","cntctd_n","cntctl_n","cntctl_1_n","cntctl_2_n","cntsol_n",
                                                                                                                                 "f19_n","f20_n","f21_n","fld041_n","fld045_n","fld049_n","fld053_n","fld054_n",
                                                                                                                                 "fld055_n","fld056_n","fld057_n","fld058_n","fld059_n","fld060_n","fld061_n",
                                                                                                                                 "fld062_n","fld063_n","fld064_n","fld065_n","fld066_n","fld067_n","fld068_n",
                                                                                                                                 "fld069_n","fld070_n","fld071_n","fld072_n","fld073_n","fld115_n","fld116_n","fld117_n","fld138_n","fld139_n",
                                                                                                                                 "fld140_n","fld141_n","fld142_n","fld143_n","fld144_n","fld145_n","fld146_n",
                                                                                                                                 "cant1m_n","mont1m_n","cant3m_n","mont3m_n","cant6m_n","mont6m_n","ctotal_n",
                                                                                                                                 "mtotal_n","rec1to_3_n","rec2to_3_n","rec3to_3_n","rec4to_3_n","rep1to_3_n",
                                                                                                                                 "rep2to_3_n","rep3to_3_n","rep4to_3_n")])))
                 #Asignación del tipo de dato. Convierte factor a character
                 registro[,c("produc_c","gama_c","eqaseg_c", 
                             "linper_c","conren_c",
                             "fld119_c",
                             "fld120_c","fld121_c","fld122_c", 
                             "pli_sn_c","depart_c","provi_c","target_c")] <- as.character(unlist(registro[,c("produc_c","gama_c","eqaseg_c", "linper_c","conren_c",
                                                                                                             "fld119_c","fld120_c","fld121_c","fld122_c", "pli_sn_c","depart_c","provi_c","target_c")]))
                 
                 return(registro)
               }
               V4DATOFIN_pure <- recibeTrama(trama,token)},
             
             {
               stop("OPCION INVÁLIDA")
               
             })                                                 
      
      #Eliminación de la primera columna que siempre se incluye sola:
      V4DATOFIN_pure <- eliminaCaracteristica(V4DATOFIN_pure, c("X1", "X1_1"))
      
      V4DATOFIN_pure_trans <- V4DATOFIN_pure
      
      print("TRANSFORMANDO DATA")
      
      # numlin_c
      V4DATOFIN_pure_trans$numlin_n <- ifelse(V4DATOFIN_pure_trans$numlin_n > 0 & V4DATOFIN_pure_trans$numlin_n  <= 5 & !is.na(V4DATOFIN_pure_trans$numlin_n), "normal",
                                   ifelse(V4DATOFIN_pure_trans$numlin_n > 5 & V4DATOFIN_pure_trans$numlin_n  <= 10 & !is.na(V4DATOFIN_pure_trans$numlin_n),"normal-alto", 
                                          ifelse(V4DATOFIN_pure_trans$numlin_n > 10 & V4DATOFIN_pure_trans$numlin_n  <= 20 & !is.na(V4DATOFIN_pure_trans$numlin_n), "alto", 
                                                 ifelse(V4DATOFIN_pure_trans$numlin_n > 20 & !is.na(V4DATOFIN_pure_trans$numlin_n), "muy-alto", V4DATOFIN_pure_trans$numlin_n))))

      
      # fld115_c: Cantidad de minutos libres que ofrece el plan.   
      V4DATOFIN_pure_trans$fld115_n <- ifelse(V4DATOFIN_pure_trans$fld115_n >= 0 & V4DATOFIN_pure_trans$fld115_n  < 10 & !is.na(V4DATOFIN_pure_trans$fld115_n), "poco",
                                   ifelse(V4DATOFIN_pure_trans$fld115_n >= 10 & V4DATOFIN_pure_trans$fld115_n  < 30 & !is.na(V4DATOFIN_pure_trans$fld115_n),"normal", 
                                          ifelse(V4DATOFIN_pure_trans$fld115_n >= 30 & V4DATOFIN_pure_trans$fld115_n  < 200 & !is.na(V4DATOFIN_pure_trans$fld115_n), "alto",
                                                 ifelse(V4DATOFIN_pure_trans$fld115_n >= 200 & V4DATOFIN_pure_trans$fld115_n  <= 3000 & !is.na(V4DATOFIN_pure_trans$fld115_n), "muy-alto",
                                                        ifelse(V4DATOFIN_pure_trans$fld115_n > 3000 & !is.na(V4DATOFIN_pure_trans$fld115_n), "ilimitado", V4DATOFIN_pure_trans$fld115_n)))))

      
      # fld116_c: Cantidad de KBs de datos que ofrece el plan.
      V4DATOFIN_pure_trans$fld116_n <- ifelse(V4DATOFIN_pure_trans$fld116_n >= 0 & V4DATOFIN_pure_trans$fld116_n  < 102400 & !is.na(V4DATOFIN_pure_trans$fld116_n), "poco",
                                   ifelse(V4DATOFIN_pure_trans$fld116_n >= 102400 & V4DATOFIN_pure_trans$fld116_n  < 1024000 & !is.na(V4DATOFIN_pure_trans$fld116_n),"normal", 
                                          ifelse(V4DATOFIN_pure_trans$fld116_n >= 1024000 & V4DATOFIN_pure_trans$fld116_n  < 5242880 & !is.na(V4DATOFIN_pure_trans$fld116_n), "alto",
                                                 ifelse(V4DATOFIN_pure_trans$fld116_n >= 5242880 & V4DATOFIN_pure_trans$fld116_n  < 999999999999999 & !is.na(V4DATOFIN_pure_trans$fld116_n), "muy-alto",
                                                        ifelse(V4DATOFIN_pure_trans$fld116_n >= 999999999999999 & !is.na(V4DATOFIN_pure_trans$fld116_n), "ilimitado", V4DATOFIN_pure_trans$fld116_n)))))

      
      # fld117_c: Cantidad de SMS que ofrece el plan.
      V4DATOFIN_pure_trans$fld117_n <- ifelse(V4DATOFIN_pure_trans$fld117_n >= 0 & V4DATOFIN_pure_trans$fld117_n  < 50 & !is.na(V4DATOFIN_pure_trans$fld117_n), "poco",
                                   ifelse(V4DATOFIN_pure_trans$fld117_n >= 50 & V4DATOFIN_pure_trans$fld117_n  < 100 & !is.na(V4DATOFIN_pure_trans$fld117_n),"normal", 
                                          ifelse(V4DATOFIN_pure_trans$fld117_n >= 100 & V4DATOFIN_pure_trans$fld117_n  < 999999 & !is.na(V4DATOFIN_pure_trans$fld117_n), "alto",
                                                 ifelse(V4DATOFIN_pure_trans$fld117_n >= 999999 & !is.na(V4DATOFIN_pure_trans$fld117_n), "ilimitado", V4DATOFIN_pure_trans$fld117_n))))
      

      #####
      #Eliminación de variables que fueron identificados en previos an?lisis.
      caracteristicasInutiles <- c("edad_n", "depart_c", "provi_c")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      ##### Contrato con renovacion de equipo ? - conren_c 
      # Eliminaci?n de conren, debido a la mala distribuci?n de cantidades
      caracteristicasInutiles <- c("conren_c")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      ##### Equipo asegurado? - eqaseg_c
      # Eliminaci?n de conren, debido a la mala distribuci?n de cantidades
      caracteristicasInutiles <- c("eqaseg_c")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      ##### Cantidad total de cambios de plan - cnttoc_n 
      ##### Cantidad de cambios de plan temporales - cnttmc_n
      # Eliminaci?n de las dos anteriores, ambas est?n correlacionas, y ambas est?n correlacionadas a "Cantidad de cortes por deuda" (?ste ?ltimo si quedar?)
      caracteristicasInutiles <- c("cnttoc_n", "cnttmc_n" )
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      ##### Cantidad de cambios de plan normales - cntnrc_n
      # Se formar? rangos y se cambiar? el nombre de la caracter?stica:
      cntnrc_n <- V4DATOFIN_pure_trans$cntnrc_n
      cntnrc_c <- ifelse(cntnrc_n == 0, "0", 
                         ifelse(cntnrc_n > 0 & cntnrc_n <= 1, "1", 
                                ifelse(cntnrc_n > 1 & cntnrc_n <= 2, "2",
                                       ifelse(cntnrc_n > 2 & cntnrc_n <= 4, "3-4",
                                              ifelse(cntnrc_n > 4, ">4", cntnrc_n)))))
      V4DATOFIN_pure_trans$cntnrc_n <- cntnrc_c
      
      
      #### Cantidad de cortes por deuda: cntctd_n
      # Se formar? rangos y se cambiar? el nombre de la caracter?stica:
      cntctd_n <- V4DATOFIN_pure_trans$cntctd_n
      cntctd_c <- ifelse(cntctd_n == 0, "0", 
                         ifelse(cntctd_n > 0 & cntctd_n <= 3, "1-3", 
                                ifelse(cntctd_n > 3 & cntctd_n <= 9, "4-9", 
                                       ifelse(cntctd_n > 9, ">9", cntctd_n))))
      
      V4DATOFIN_pure_trans$cntctd_n <- cntctd_c
      
      #### Cantidad de cortes por limite de consumo de voz - cntctl_n
      #Se eliminar? esta variable por tener demasiados ceros
      #Visualizaci?n
      
      #Eliminaci?n
      caracteristicasInutiles <- c("cntctl_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      #### Cantidad de cortes por limite de consumo de SMS - cntctl_1_n
      #Se eliminar? esta variable por tener demasiados ceros
      #Eliminaci?n
      caracteristicasInutiles <- c("cntctl_1_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      #### Cantidad de cortes por limite de consumo de datos - cntctl_2_n
      #Se eliminar? esta variable por tener demasiados ceros
      #Eliminaci?n
      caracteristicasInutiles <- c("cntctl_2_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      #### Cantidad de solicitudes de baja: cntsol_n
      #Se har? el binning
      cntsol_n <- V4DATOFIN_pure_trans$cntsol_n
      cntsol_c <- ifelse(cntsol_n == 0, "0", 
                         ifelse(cntsol_n > 0, ">0", cntsol_n))
      V4DATOFIN_pure_trans$cntsol_n <- cntsol_c
      #Se deber? cambiar la descripci?n a "Usuario solicit? alguna vez baja"?
      #Se conservar? esta variable.
      #caracteristicasInutiles <- c("cntsol_n").
      #V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      #Moviendo la cantidad de solicitudes al final, porque en ese orden estaba definido la salida.
      cntsol_n_temp <- V4DATOFIN_pure$cntsol_n
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, c("cntsol_n"))
      cntsol_c <- cntsol_n_temp
      V4DATOFIN_pure_trans <- cbind(V4DATOFIN_pure_trans, cntsol_c)
      rm(cntsol_c, cntsol_n_temp)
      
      
      
      #### Se crear? nueva variable    f19_n + f20_n + f21_n  -->  portant_c   (portaciones anteriores a cualquier operador)
      
      portant_n <- V4DATOFIN_pure$f19_n + V4DATOFIN_pure$f20_n  + V4DATOFIN_pure$f21_n 
      porant_c <-ifelse(portant_n == 0, "nunca_porto", ifelse(portant_n > 0, "alguna_vez_porto", portant_n))
      
      
      
      #Se deber? eliminar las variables f19_n, f20_n, f21_n Y a?adir la variable portant_c
      #Eliminaci?n de carater?ticas in?tiles.
      caracteristicasInutiles <- c("f19_n", "f20_n", "f21_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      #Agregando caracter?stica transformada.
      V4DATOFIN_pure_trans <- cbind(V4DATOFIN_pure_trans, porant_c)

      #### Total de minutos que recibieron de numeros de claro: fld041_n
      #Binning, se convertir? a variable dicot?mica
      fld041_n <- V4DATOFIN_pure_trans$fld041_n
      fld041_c <- ifelse(fld041_n == 0, "0",
                         ifelse(fld041_n > 0, ">0", fld041_n))
      
      V4DATOFIN_pure_trans$fld041_n <- fld041_c
      
      #Se eliminar? las variable fld054_n, fld055_n, fld056_n (desagregado de fld053_n - Total de minutos que hicieron a n?meros claro)
      caracteristicasInutiles <- c("fld054_n", "fld055_n", "fld056_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      #Se eliminar? las variable fld058_n, fld059_n, fld060_n (desagregado de fld057_n - Total de minutos que hicieron a n?meros entel)
      caracteristicasInutiles <- c("fld058_n", "fld059_n", "fld060_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      #Se eliminar? las variable fld062_n, fld063_n, fld064_n (desagregado de fld061_n - Total de minutos que hicieron a n?meros vittel)
      caracteristicasInutiles <- c("fld062_n", "fld063_n", "fld064_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      #Se eliminar? las variable fld066_n, fld067_n, fld068_n (desagregado de fld065_n - Total de minutos que hicieron a n?meros vittel)
      caracteristicasInutiles <- c("fld066_n", "fld067_n", "fld068_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      #Se eliminar? las variable fld070_n, fld071_n, fld072_n (desagregado de fld069_n - Cantidad de SMS enviados)
      caracteristicasInutiles <- c("fld070_n", "fld071_n", "fld072_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      #### Total de minutos que recibieron de numeros de ENTEL: fld045_n
      #Binning, se convertir? a variable dicot?mica
      fld045_n <- V4DATOFIN_pure_trans$fld045_n
      fld045_c <- ifelse(fld045_n == 0, "0",
                         ifelse(fld045_n > 0, ">0", fld045_n))
      V4DATOFIN_pure_trans$fld045_n <- fld045_c
      
      
      
      #### Total de minutos que recibieron de numeros de VITTEL: fld049_n
      #Binning, se convertir? a dicot?mica.
      #Binning, se convertir? a variable dicot?mica
      fld049_n <- V4DATOFIN_pure_trans$fld049_n
      fld049_c <- ifelse(fld049_n == 0, "0",
                         ifelse(fld049_n > 0, ">0", fld049_n))
      V4DATOFIN_pure_trans$fld049_n <- fld049_c
      
      
      #### Total de minutos que se hicieron a claro: fld053_n
      
      fld053_n <- V4DATOFIN_pure_trans$fld053_n
      fld053_c <- ifelse(fld053_n == 0, "0",
                         ifelse(fld053_n > 0 & fld053_n <= 100, "1-100", 
                                ifelse(fld053_n > 100 & fld053_n <= 200, "101-200", 
                                       ifelse(fld053_n > 200 & fld053_n <= 300, "201-300", 
                                              ifelse(fld053_n > 300, ">301", fld053_n)))))
      
      V4DATOFIN_pure_trans$fld053_n <- fld053_c
      
      
      
      #### Total de minutos que se hicieron a entel: fld057_n
      
      #Binning, se convertir? a dicot?mica.
      fld057_n <- V4DATOFIN_pure_trans$fld057_n
      fld057_c <- ifelse(fld057_n == 0, "0",
                         ifelse(fld057_n > 0 & fld057_n <= 100, "1-100", 
                                ifelse(fld057_n > 100 & fld057_n <= 200, "101-200", 
                                       ifelse(fld057_n > 200 & fld057_n <= 300, "201-300", 
                                              ifelse(fld057_n > 300, ">301", fld057_n)))))
      V4DATOFIN_pure_trans$fld057_n <- fld057_c
      
      
      #### Total de minutos que se hicieron a vittel: fld061_n
      
      #Binning, se convertir? a dicot?mica.
      fld061_n <- V4DATOFIN_pure_trans$fld061_n
      fld061_c <- ifelse(fld061_n == 0, "0",
                         ifelse(fld061_n > 0 & fld061_n <= 100, "1-100", 
                                ifelse(fld061_n > 100 & fld061_n <= 200, "101-200", 
                                       ifelse(fld061_n > 200 & fld061_n <= 300, "201-300", 
                                              ifelse(fld061_n > 300, ">301", fld061_n)))))
      V4DATOFIN_pure_trans$fld061_n <- fld061_c
      
      
      #### Total de KB consumidos  fld065_n
      #?Convertido a: alguna vez us? internet m?vil?
      
      #Binning, se convertir? a dicot?mica.
      fld065_n <- V4DATOFIN_pure_trans$fld065_n
      fld065_c <- ifelse(fld065_n == 0, "No",
                         ifelse(fld065_n > 0 , "Si", fld065_n)) 
      
      V4DATOFIN_pure_trans$fld065_n <- fld065_c
      
      
      
      #### Total de SMS consumidos
      # Binning
      
      fld069_n <- V4DATOFIN_pure_trans$fld069_n
      fld069_c <- ifelse(fld069_n == 0, "0",
                         ifelse(fld069_n > 0 & fld069_n <= 100 , "(0-100]", 
                                ifelse(fld069_n > 100 & fld069_n <= 300 , "(150-300]", 
                                       ifelse(fld069_n > 300 & fld069_n <= 700, "(300-700]", 
                                              ifelse(fld069_n > 700, ">700", fld069_n)))))
      V4DATOFIN_pure_trans$fld069_n <- fld069_c
      
      
      #### Total de llamadas calculadas fld073_n
      #Se eliminar? esta variable debido al grado de desbalanceo
      caracteristicasInutiles <- c("fld073_n")
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      
      
      #Eliminaci?n de las siguientes variables por correlaci?n con su totalizado:
      
      
      
      
      #Eliminaci?n de las otras variables mapeadas
      caracteristicasInutiles <- c("fld058_n", "fld059_n", "fld060_n", "fld062_n", "fld063_n", "fld064_n", "fld066_n", "fld067_n", "fld068_n", "fld070_n","fld071_n","fld072_n", "fld139_n","fld140_n","fld141_n","fld142_n","fld143_n","fld144_n", "fld145_n", "cant1m_n", "mont1m_n","cant3m_n", "mont3m_n","cant6m_n", "mont6m_n", "rec1to_3_n", "rec2to_3_n", "rec3to_3_n", "rec4to_3_n", "rep1to_3_n", "rep2to_3_n", "rep3to_3_n", "rep4to_3_n","cntsol_n","porant_c" )
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, caracteristicasInutiles)
      
      
      V4DATOFIN_pure_trans_save  <-V4DATOFIN_pure_trans 
      
      #Moviendo el Target al final:
      target_c_temp <- V4DATOFIN_pure$target_c
      V4DATOFIN_pure_trans <- eliminaCaracteristica(V4DATOFIN_pure_trans, c("target_c"))
      target_c <- target_c_temp
      V4DATOFIN_pure_trans <- cbind(V4DATOFIN_pure_trans, target_c)
      rm(target_c, target_c_temp)
            
      #Colocando nombres resultantes
      names(V4DATOFIN_pure_trans) <- c("antig_n","numlin_c","produc_c","gama_c","eqanti_n","linper_c","costpl_n","cntnrc_c","cntctd_c","fld041_c","fld045_c","fld049_c","fld053_c","fld057_c","fld061_c","fld065_c","fld069_c","fld115_c","fld116_c","fld117_c","fld119_c","fld120_c","fld121_c","fld122_c","fld138_n","fld146_n","pli_sn_c","ctotal_n","mtotal_n","cntsol_c","target_c")
      

      
      print("EXPORTANDO DATA")
      
      switch(opcion, 
             B = {
               #Caso 1: Se ingres? el path destino: opcion = "batch"
               #Genera excel:
               write.csv(V4DATOFIN_pure_trans, path_destino, row.names = FALSE)
               
             },
             
             O = {
               #Caso 2: Se ingres? la trama opcion = "batch"    
               #Muestra en consola
               print(V4DATOFIN_pure_trans)
               trama_transformada <- do.call(paste, c(V4DATOFIN_pure_trans, sep = token))
               
               return(trama_transformada)
             },
             
             {
               stop("OPCION INVÁLIDA")
               
             })   
      
      
    },error=function(cond) {
      message <- as.character(cond)
      return(message)
    }, warning=function(cond) {
      message <- as.character(cond)
      return(message)
    }
    
    
  ) 
  
  return(out)
  
}