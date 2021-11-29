# codigo R
# ERAMIA-SP 2020 - Introducao a meta-aprendizado
# Ana C. Lorena e Luis P. F. Garcia

if(!require("caret")) {
  install.packages("caret")
}

require("caret")

kfold <- function(data, folds=10) {

  #cria um vetor com do mesmo tamanho da classe, sorteando a pasta em que o item cairá
  id = createFolds(data$class, k=folds, list=FALSE)

  #cria uma lista com subconjuntos de treinamento
  tran = lapply(1:folds, function(i) {
    subset(data, id %in% setdiff(1:folds, i))
  })

  test = lapply(1:folds, function(i) {
    subset(data, id %in% i)
  })

  tmp = list()
  tmp$data = data
  tmp$tran = tran
  tmp$test = test
  return(tmp)
}