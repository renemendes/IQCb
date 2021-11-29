if(!require("plotly")) {
  install.packages("plotly")
}

#https://plot.ly/r/radar-chart/
library(plotly)

#define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


setwd("datafiles")

Dataset <- read.table("resultadoFinal.csv", header=TRUE, sep=",", na.strings="NA", dec=".", strip.white=TRUE)



# seta o nome das linhas
rownames(Dataset) <- Dataset$datasetName


#normalizando os valores
#https://www.statology.org/how-to-normalize-data-in-r/
#DatasetDCNorm.norm <- as.data.frame(lapply(DatasetDCNorm[2:23],min_max_norm))
#DatasetDCNorm.norm$datasetName <- DatasetDCNorm$datasetName
#rownames(DatasetDCNorm.norm) <- DatasetDCNorm.norm$datasetName

#########PLOTAGEM

#retira a coluna com o nome do dataset
#DatasetDQ <-DatasetDQ[2:25]
#
## escolhe um dataset
#Dataset2 <- DatasetDQ[(row.names(DatasetDQ) %in% c(1,2,"appendicitis")),]
#
##https://plot.ly/r/radar-chart/
##https://plot.ly/r/reference/#scatterpolar
#
#p <- plot_ly(
#  type = 'scatterpolar',
#  fill = 'toself'
#) %>%
#  add_trace(
#    r=c(Dataset2$DQCompleteness,Dataset2$DQValidity,Dataset2$balance.C1,Dataset2$balance.C2,Dataset2$neighborhood.N1,Dataset2$neighborhood.N2,Dataset2$neighborhood.N3,Dataset2$neighborhood.N4,Dataset2$neighborhood.T1,Dataset2$neighborhood.LSC,Dataset2$network.Density,Dataset2$network.ClsCoef,Dataset2$network.Hubs,Dataset2$overlapping.F1,Dataset2$overlapping.F1v,Dataset2$overlapping.F2,Dataset2$overlapping.F3,Dataset2$overlapping.F4,Dataset2$dimensionality.T2,Dataset2$dimensionality.T3,Dataset2$dimensionality.T4,Dataset2$linearity.class.L1,Dataset2$linearity.class.L2,Dataset2$linearity.class.L3),
#    theta = c("DQcompleteness","DQvalidity","DCbalance.C1","DCbalance.C2","DCneighborhood.N1","DCneighborhood.N2","DCneighborhood.N3","DCneighborhood.N4","DCneighborhood.T1","DCneighborhood.LSC","DCnetwork.Density","DCnetwork.ClsCoef","DCnetwork.Hubs","DCoverlapping.F1","DCoverlapping.F1v","DCoverlapping.F2","DCoverlapping.F3","DCoverlapping.F4","DCdimensionality.T2","DCdimensionality.T3","DCdimensionality.T4","DClinearity.class.L1","DClinearity.class.L2","DClinearity.class.L3"),
#    name = 'appendicitis'
#  ) %>%
#  layout(
#    polar = list(
#      radialaxis = list(
#        visible = T,
#        range = c(0,1)
#      )
#    )
#  )
#
#p
#
####################### EXPORTA O DATASET FINAL
write.csv(DatasetFinal,"datafiles\\dataset_plssem.csv", row.names = FALSE)


