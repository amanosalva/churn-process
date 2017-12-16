library("rpart")
library("rpart.plot")
library("C50")

V4DATOFIN_pure_final <- read_csv("C:/Byte/Machine Learning/Predictivo Port Out - Oficial/Dataset/v.Oficial.final/V4DATOFIN_pure_final.csv",
                                    col_types = list("antig_n" = col_integer(), "eqanti_n" = col_integer(), "costpl_n" = col_double(),
                                                     "fld138_n" = col_integer(), "fld146_n" = col_double(), "ctotal_n" = col_integer(),
                                                     "mtotal_n" = col_double(), "cntsol_c" = col_integer(), "target_c" = col_integer()))


#Creamos set de entrenamiento y de test
#80% entrenamiento - 20% validación
ind        <- sample(2,nrow(V4DATOFIN_pure_final), replace=TRUE, prob=c(0.8, 0.2))
trainData  <- V4DATOFIN_pure_final[ind==1, ]
testData   <- V4DATOFIN_pure_final[ind==2, ]

ArbolRpart <- rpart(target_c ~ ., method = "class", data = trainData)

print(ArbolRpart)
rpart.plot(ArbolRpart)  # extra=4:probabilidad de observaciones por clase

testPredRpart <- predict(ArbolRpart, newdata = testData, type = "class")

#Visualizamos una matriz de confusión
table(testPredRpart, testData$target_c)

#Estadística del modelo.
#Calculamos el % de aciertos.
sum(testPredRpart == testData$target_c)/length(testData$target_c)*100
