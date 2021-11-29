# ***************************************
# DATA:       14/10/2020
# AUTOR:      Rene de Avila Mendes
# DESCRICAO:  Obtem as dimensoes dos dados
# https://www.fromthebottomoftheheap.net/2012/04/01/saving-and-loading-r-objects/
# ***************************************

#https://www.rdocumentation.org/packages/StatMeasures/versions/1.0
#install.packages("StatMeasures")

if(!require("StatMeasures")) {
  install.packages('https://cran.r-project.org/bin/windows/contrib/4.1/StatMeasures_1.0.zip', repos = NULL, type = "win.binary")
}

# https://cran.r-project.org/web/packages/ECoL/readme/README.html 	
#install.packages("ECoL")
#library(ECoL)

if(!require("ECoL")) {
  install.packages("ECoL")
}

#https://www.r-bloggers.com/converting-a-list-to-a-data-frame/
#install.packages("devtools")
 
if(!require("devtools")) {
  install.packages("devtools")
}


if(!require("foreign")) {
  install.packages("foreign")
}

#devtools::install_github("rivolli/mfe")

require("StatMeasures")
require("ECoL")
require("devtools")
require("foreign")
require("mfe")

# Carregando o algoritmo de deteccao de MV MCAR
# https://www.rdocumentation.org/packages/BaylorEdPsych/versions/0.5/topics/LittleMCAR
# https://rdrr.io/cran/BaylorEdPsych/f/
# https://cran.microsoft.com/snapshot/2016-10-29/web/packages/BaylorEdPsych/index.html
#source("LittleMCAR.R")


library(OutlierDetection)


#mode functions
#https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


# load datasets
# https://swcarpentry.github.io/r-novice-inflammation/03-loops-R/
datasets = list.files(path="datafiles",pattern=".arff", full.names=TRUE)

# preallocated data frame
# nomes dos atributos obtidos de https://github.com/lpfgarcia/ECoL/

#Feature-based measures
#F1: Fisher's discriminant ratio
#F1v: The directional-vector Fisher's discriminant ratio
#F2: Overlapping of the per-class bounding boxes
#F3: Maximum individual feature efficiency
#F4: Cllective feature efficiency
#
#Neighborhood information
#N1: Fraction of points lying on the class boundary
#N2: Average intra/inter class nearest neighbor distances
#N3: Leave-one-out error rate of the 1-nearest neighbor algorithm
#N4: Nonlinearity of the one-nearest neighbor classifier
#N5: Fraction of maximum covering spheres on data
#N6: Local-Set cardinality average
#
#Linearity
#L1: Distance of erroneous instances to a linear classifier
#L2: Training error of a linear classifier
#L3: Nonlinearity of a linear classifier
#
#Dimensionality
#D1: Average number of samples per dimension
#D2: Average intrinsic dimensionality per number of examples
#D3: Intrinsic dimensionality proportion
#
#Class balance
#B1: Entropy of class proportions
#B2: Multi-class imbalance ratio
#
#Structural representation
#G1: Average density of network
#G2: Clustering Coefficient
#G3: Average hub score
#
#Feature correlation
#C1: Feature correlation to the output
#C2: Average feature correlation to the output
#C3: Individual feature efficiency
#C4: Collective feature efficiency
#


qualityDF <- data.frame(datasetName=character(length(datasets)),
                        MDAttributes=numeric(length(datasets)),
                        MDElements=numeric(length(datasets)),
                        DQMissingValues=numeric(length(datasets)),
                        DQOutliers=numeric(length(datasets)),
                        DQCompleteness=numeric(length(datasets)),
                        DQValidity=numeric(length(datasets)),
                        processingType=character(length(datasets)),
                        #balance
                        B1=numeric(length(datasets)),
                        B2=numeric(length(datasets)),
                        #dimensionality
                        D1=numeric(length(datasets)),
                        D2=numeric(length(datasets)),
                        D3=numeric(length(datasets)),
                        #featurebased
                        F1=numeric(length(datasets)),
                        F1v=numeric(length(datasets)),
                        F2=numeric(length(datasets)),
                        F3=numeric(length(datasets)),
                        F4=numeric(length(datasets)),
                        #linearity
                        L1=numeric(length(datasets)),
                        L2=numeric(length(datasets)),
                        L3=numeric(length(datasets)),
                        #neighborhood
                        N1=numeric(length(datasets)),
                        N2=numeric(length(datasets)),
                        N3=numeric(length(datasets)),
                        N4=numeric(length(datasets)),
                        N5=numeric(length(datasets)),
                        N6=numeric(length(datasets)),
                        #network
                        G1=numeric(length(datasets)),
                        G2=numeric(length(datasets)),
                        G3=numeric(length(datasets)),
            						stringsAsFactors=FALSE)


setwd("datafiles")

###T E S T E
#set.seed(1)
#file = sample(datasets, 1)
#print(file)

#Extraindo as medidas de complexidade para a base:

#data = read.arff(file)
#featurebased = featurebased(class ~ ., data, summary="mean")
#neighborhood = neighborhood(class ~ ., data, summary="mean")
#linearity = linearity(class ~ ., data, summary="mean")
#network = network(class ~ ., data, summary="mean")
#dimensionality = dimensionality(class ~ ., data, summary="mean")
#balance = balance(class ~ ., data, summary="mean")


# LE TODOS OS DATASETS
for (i in 1:length(datasets))
{

  #armazena o nome do arquivo
	qualityDF[i,"datasetName"] <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datasets[i]))

	message("processando ",i, " ", qualityDF[i,"datasetName"])

	#load dataset
	dataset <- read.arff(datasets[i])
  
	#replace ? with NA
	dataset[dataset == "?"] <- NA
  
	#get the column names
	#columnNames = as.data.frame(dataset$attributeNames)
	#colnames(columnNames) <- c("columnNames")

	#dataset
	#converting list to dataframe
	#https://www.r-bloggers.com/converting-a-list-to-a-data-frame/
	# transpose de dataframe
	# https://stackoverflow.com/questions/6778908/transpose-a-data-frame
	#dataset <-as.data.frame(t(as.data.frame(dataset$data)))
	#dataset <-as.data.frame(dataset$data)
	#dataset <- dataset$data
	#dataset <- as.data.frame(t(dataset))
  
	#get the column names
	#colnames(dataset) <- columnNames$columnNames
  
	# number of rows
	qualityDF$MDElements[i] <- nrow(dataset)
  
	# number of columns
	# retirada da coluna de classe
	qualityDF$MDAttributes[i] <- ncol(dataset)-1
  
	#################################
	#MISSING VALUES
	#################################
	qualityDF$DQMissingValues[i] = sum(is.na(dataset))
  
	#################################
	# COMPLETENESS 
	# Metric for Completeness by Blake and Mangiameli [2011]
	#################################
  
	# 19/02/2021 - Implementar o teste MCAR
	# https://www.rdocumentation.org/packages/BaylorEdPsych/versions/0.5/topics/LittleMCAR
	
	# n - numero de ocorrencias de valores ausentes
	# N - número de exemplares do conjunto de dados p
	# A - número de atributos (exceto a classe)
	# Cp = 1- (n/(N*(1+A)))
	qualityDF$DQCompleteness[i] <- 1-(sum(is.na(dataset))/(nrow(dataset)*ncol(dataset)))
	

	#################################
	#OUTLIERS
	#################################
	# 19/02/2021 - RETIRAR COLUNAS DE TEXTO (o método atual não está funcionando)
	# tentar implementar q-quadrado, senão usar mahalanobis

		#extrai o atributo de classe
	#extrai os atributos categoricos e textuais
	#Fatores (factor) são usados para representar variáveis categóricas. As variáveis categóricas podem ser nominais (não ordenadas) ou ordinais (ordenadas).
	datasetNoChar = dataset[,!names(dataset)=="class"]
	datasetNoChar = datasetNoChar[, !sapply(datasetNoChar, is.character)]
	datasetNoChar = datasetNoChar[, !sapply(datasetNoChar, is.factor)]
	
	#se sobraram colunas no dataset
	if(!is.null(ncol(datasetNoChar)) && ncol(datasetNoChar)>1)
	{
	  
	  # https://cran.r-project.org/web/packages/OutlierDetection
	  #maha = maha(datasetNoChar, cutoff = 0.95, rnames = TRUE)
	  #qualityDF$DQOutliers[i]=length(maha$`Location of Outlier`)
	  
	  #https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d
	  # Finding the center point 
	  datasetNoChar.center = colMeans(datasetNoChar)
	  
	  # Finding the covariance matrix
	  datasetNoChar.cov = cov(datasetNoChar)
	  
	  #https://stackoverflow.com/questions/22134398/mahalonobis-distance-in-r-error-system-is-computationally-singular
	  	  distances <- mahalanobis(x = datasetNoChar, center = datasetNoChar.center ,cov = datasetNoChar.cov,tol=1e-20)
	  
	  
	  # Cutoff value for ditances from Chi-Sqaure Dist. 
	  # with p = 0.95 df = 2 which in ncol(air)
	  cutoff <- qchisq(p = 0.95 , df = ncol(datasetNoChar))
	  
	  ## Display observation whose distance greater than cutoff value
	  qualityDF$DQOutliers[i]=length(datasetNoChar[distances > cutoff ,])

  }

	
	#################################
	# CALCULO DA VALIDADE
	#################################
	# Validity = 1 ??? (Out/(Att × Obj))
	#qualityDF$DQValidity[i] <- 1-(qualityDF$DQOutliers[i]/(qualityDF$MDAttributes[i]*qualityDF$MDElements[i]))
	qualityDF$DQValidity[i] =  1-(qualityDF$DQOutliers[i]/qualityDF$MDElements[i])
	
	
	#################################
	# TRTAMENTO DE MISSING VALUES
	#################################
	
	#INCLUIR:
	# - o cálculo da distribuição dos valores ausentes, e 
	# - a tabulação de Hair (2014, p.45)
	# para decidir o método de imputação, se imputação, ou a exclusão de exemplares
	
	# 22/02/2021 - os valores ausentes nao serao tratados
	
	# replace missing values with mean or mode
	# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
	#for(j in colnames(dataset)[colSums(is.na(dataset)) > 0])
	#{
  #  
	#	dataset[[j]][is.na(dataset[[j]])] <- getmode(dataset[[j]])
	#}
  
	#################################
	#DATA COMPLEXITY
	#################################
	# ver documentacao do pacote ECOL
	# https://cran.r-project.org/web/packages/ECoL/readme/README.html	 	
  
	balance = balance(class ~ ., dataset, summary="mean")
	dimensionality = dimensionality(class ~ ., dataset, summary="mean")
	featurebased = featurebased(class ~ ., dataset, summary="mean")
	linearity = linearity(class ~ ., dataset, summary="mean")
	neighborhood = neighborhood(class ~ ., dataset, summary="mean")
	network = network(class ~ ., dataset, summary="mean")	
	
	
	

	# store complexity results  
	for (k in names(featurebased))
	{

		qualityDF[i,k]=featurebased[k]
		#print(featurebased[k])
	}
	

	for (k in names(neighborhood))
	{

		qualityDF[i,k]=neighborhood[k]
		#print(neighborhood[k])
	}

	for (k in names(linearity))
	{

		qualityDF[i,k]=linearity[k]
		#print(linearity[k])
	}

	for (k in names(network))
	{

		qualityDF[i,k]=network[k]
		#print(network[k])
	}
	
	for (k in names(dimensionality))
	{

		qualityDF[i,k]=dimensionality[k]
		#print(dimensionality[k])
	}
	
	for (k in names(balance))
	{

		qualityDF[i,k]=balance[k]
		#print(balance[k])
	}
	
	qualityDF[i,"processingType"]="classification"

  

}

#save dataset
write.csv(qualityDF,"datafiles\\results.csv", row.names = FALSE)
