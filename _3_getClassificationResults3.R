# ***************************************
# DATA:       27/10/2020
# AUTOR:      Rene de Avila Mendes
# DESCRICAO:  Obtem os valores AUC de classificacoes
# https://lpfgarcia.github.io/mtl/classificadores

#IMPLEMENTANDO AUC
#https://rdrr.io/cran/cvAUC/man/cvAUC.html
#https://stackoverflow.com/questions/37215366/plot-roc-curve-from-cross-validation-training-data-in-r
#https://stackoverflow.com/questions/41523761/how-to-compute-auc-with-rocr-package
#https://www.rdocumentation.org/packages/MLmetrics/versions/1.1.1/topics/AUC
#https://financetrain.com/measure-model-performance-in-r-using-rocr-package/
#https://www.rdocumentation.org/packages/pROC/versions/1.16.2/topics/roc

# ***************************************

#https://rdrr.io/cran/cvAUC/man/cvAUC.html
if(!require("ROCR")) {
  install.packages("ROCR")
}

if(!require("foreign")) {
  install.packages("foreign")
}

if(!require("rpart")) {
  install.packages("rpart")
}

if(!require("RWeka")) {
  install.packages("RWeka")
}

if(!require("kknn")) {
  install.packages("kknn")
}

#SVM
if(!require("e1071")) {
  install.packages("e1071")
}

if(!require("randomForest")) {
  install.packages("randomForest")
}

if(!require("devtools")) {
  install.packages("devtools")
}

require("foreign")
require("rpart")
require("RWeka")
require("kknn")
require("e1071")
require("randomForest")
require("devtools")
require("ROCR")

setwd("datafiles")

#Carregando o algoritmo k-fold cross-validation:
source("kfold.R")

#mode functions
#https://www.tutorialspoint.com/r/r_mean_median_mode.htm
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

C4.5 <- function(tran, test) {
  model = J48(class ~., tran)
  pred = predict(model, test[,-ncol(test)])
  return(pred)
}

ANN <- function(train, test) {
    mlp <- make_Weka_classifier("weka/classifiers/functions/MultilayerPerceptron")
    model <- mlp(class ~ ., train)
    predict(model, test[,-ncol(test)])
}

kNN3 <- function(tran, test) {
  pred = kknn(class ~., tran, test[,-ncol(test)], k=3)$fitted.values
  names(pred) = rownames(test)
  return(pred)
}

kNN1 <- function(tran, test) {
  pred = kknn(class ~., tran, test[,-ncol(test)], k=1)$fitted.values
  names(pred) = rownames(test)
  return(pred)
}

SVM <- function(tran, test) {
  model = svm(class ~., tran, kernel="radial")
  pred = predict(model, test[,-ncol(test)])
  return(pred)
}

RF <- function(train, test) {
    model <- randomForest::randomForest(class ~ ., train)
    predict(model, test[,-ncol(test)])
}

CART <- function(tran, test) {
  model = rpart(class ~., tran, method="class")
  pred = predict(model, test[,-ncol(test)], type="class")
  return(pred)
}

acuracia <- function(test, pred) {
  tab = table(test$class, pred)
  sum(diag(tab))/sum(tab)
}

#dica do prof. LUÃS PAULO F. GARCIA, por email em 28/10/2020
auc <- function(class, predicted) {
    
    auxAuc <- prediction(as.numeric(as.vector(predicted)), as.numeric(as.vector(class)))
    
    unlist(performance(auxAuc, "auc")@y.values)
}

#set.seed(1)

#datasets = sample(list.files(path="datafiles",pattern=".arff", full.names=TRUE), 1)

datasets = list.files(path="datafiles",pattern=".arff", full.names=TRUE)

## preallocated data frame
# 09/03/2021 - medidas completas
AUCDF <- data.frame(datasetName=character(length(datasets)),
                        kNN3=numeric(length(datasets)),
                        kNN1=numeric(length(datasets)),
                        C4.5=numeric(length(datasets)),
                        RF=numeric(length(datasets)),
                        SVM=numeric(length(datasets)),
                        ANN=character(length(datasets)),
                        CART=numeric(length(datasets)),
                        stringsAsFactors=FALSE)
#18/02/2021 - RETIRADA DOS ALGORITMOS NAO ADOTADOS NA PESQUISA
#AUCDF <- data.frame(datasetName=character(length(datasets)),
#                        C4.5=numeric(length(datasets)),
#                        RF=numeric(length(datasets)),
#                        CART=numeric(length(datasets)),
#                        stringsAsFactors=FALSE)

# LE TODOS OS DATASETS
for (i in 1:length(datasets))
{
	#armazena o nome do arquivo
	AUCDF[i,"datasetName"] <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datasets[i]))

	message("processando ",i, " ", AUCDF[i,"datasetName"])

	#load dataset
	data <- read.arff(datasets[i])
	
	#imprime o nome do arquivo
	#print(datasets[i])
  
	#replace ? with NA
	data[data == "?"] <- NA
  
	#25/01/2021 - Os dados devem ser normalizados para uma melhor atuação dos kNN3 e kNN1
	# para c4.5 os atributos contínuos devem ser discretizados 
  
  
	#MISSING VALUES
	#################################
	# replace missing values with mean or mode
	# https://www.tutorialspoint.com/r/r_mean_median_mode.htm
	# 27/10 - a moda pode ser NA, entao desconsiderar os NAs na moda
	#for(j in colnames(data)[colSums(is.na(data)) > 0])
	#{
    #
	#	data[[j]][is.na(data[[j]])] <- getmode(data[[j]][!is.na(data[[j]])])
	#}

	#particionamento em k-pastas
	data = kfold(data)
	

	#testado em 27/10/2020
	# para cada fold executa a função (10 vezes)
	aux = mapply(function(tran, test) {
	  
	  #print(tran)
	  #print(test)
		
		sapply(c("kNN3","kNN1", "C4.5","RF","SVM","ANN","CART"), function(clas) {
		#sapply(c("C4.5","RF","CART"), function(clas) {
		
		pred = do.call(clas, list(tran, test))
		
		#acuracia(test, pred)
		
		#29/10 - aplicando a funcao oferecida pelo Prof. LUIS PAULO
		auc(test$class,pred)
		
		})
	}, tran=data$tran, test=data$test)
		
	#print(aux)
	#calcula a media por algoritmo
	resultado=rowMeans(aux)
	
	#armazena os valores
	#18/02/2021 - RETIRADA DOS ALGORITMOS NAO UTILIZADOS NA PESQUISA
	AUCDF[i,"kNN3"] <- resultado[1]
	AUCDF[i,"kNN1"] <- resultado[2]
	AUCDF[i,"C4.5"] <- resultado[3]
	AUCDF[i,"RF"] <- resultado[4]
	AUCDF[i,"SVM"] <- resultado[5]
	AUCDF[i,"ANN"] <- resultado[6]
	AUCDF[i,"CART"] <- resultado[7]
	
	#armazena os valores
	#AUCDF[i,"C4.5"] <- resultado[1]
	#AUCDF[i,"RF"] <- resultado[2]
	#AUCDF[i,"CART"] <- resultado[3]
}

# get number of classes
#datasetsClasses <- read.csv("datafiles\\20190417\\RDS\\datasetClasses.csv", sep=";",header = TRUE)

# get classification results
# "Study on the Impact of Partition-Induced Dataset Shift on k-fold Cross-Validation"
#datasetsResults <- read.csv("datafiles\\20190417\\RDS\\classificationResults.csv", sep=";",header = TRUE)

#merge dataframes
#datasetsAttributes = merge(datasetsClasses,datasetsResults,by.x="datasetName",by.y="datasetName",all=TRUE)

####save dataset
write.csv(AUCDF,"datafiles\\AUC.csv", row.names = FALSE)
