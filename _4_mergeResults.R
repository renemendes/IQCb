# ***************************************
# DATA:       05/11/2020
# AUTOR:      Rene de Avila Mendes
# DESCRICAO:  Junta os resultados

setwd("datafiles")

# obtem os metadados de complexidade
datasetsComplexity <- read.csv("results.csv", sep=",",header = TRUE)

datasetsComplexity <- datasetsComplexity[,!(names(datasetsComplexity) %in% c("processingType"))]

# obtem os valores de AUC
datasetsAUC <- read.csv("AUC.csv", sep=",",header = TRUE)

#junta os resultados
datasetsAttributes = merge(datasetsComplexity,datasetsAUC,by.x="datasetName",by.y="datasetName",all=FALSE)

####save dataset
write.csv(datasetsAttributes,"resultadoFinal.csv", row.names = FALSE)
