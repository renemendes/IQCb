# ***************************************
# DATA:       16/10/2020
# AUTOR:      Rene de Avila Mendes
# DESCRICAO:  Obtem as bases de dados
# https://lpfgarcia.github.io/mtl/coletando_bases
# ***************************************

setwd("datafiles")

if(!require("OpenML")) {
  install.packages("OpenML")
}

## Loading required package: OpenML

if(!require("farff")) {
  install.packages("farff")
}

## Loading required package: farff

if(!require("foreign")) {
  install.packages("foreign")
}

## Loading required package: foreign

if(!require("dplyr")) {
  install.packages("dplyr")
}

require("OpenML")
require("foreign")


bases = listOMLDataSets()

## Downloading from 'http://www.openml.org/api/v1/json/data/list/limit/5000/status/active' to '<mem>'.

dim(bases)

## [1] 3125   16

colnames(bases)

# duas classes
# aceita nulos

ids = which(
  bases$status == "active" &
  bases$number.of.instances >= 100 &
  bases$number.of.instances <= 3000 &
  #bases$number.of.instances <= 50000 &  
  bases$number.of.features >= 2 &
  #bases$number.of.features <= 10 & #18/02/2021 - TENTAR MAIS ATRIBUTOS
  bases$number.of.features <= 20 &
  bases$number.of.classes == 2 &
  bases$number.of.missing.values >= 0
)

bases = bases[ids,]
dim(bases)


for(i in 1:nrow(bases)) {
  id = bases$data.id[i]
  
  # baixando as bases
  dataset = OpenML::getOMLDataSet(data.id=id, verbosity=0)
  name = bases$name[i]

  aux = dataset$data

  # evitando problemas com os nomes dos atributos
  colnames(aux) = make.names(colnames(aux))

  # evitando problemas com a classe
  if(dataset$target.feature != "class") {
    aux$class = dataset$data[,dataset$target.features]
    aux[,dataset$target.features] = NULL
  }

  # remover atributos pre-catalogados como inuteis
  if(!is.na(dataset$desc$ignore.attribute)) {
    aux[,dataset$desc$ignore.attribute] = NULL
  }

  # evitando problemas de formatação
  aux$class = as.factor(as.numeric(aux$class))

  # colocando a classe na última coluna
  aux = aux %>% select(-class,class)
  
  if(min(summary(aux$class)) > 10) {
    # salvando no arquivo
    write.arff(x=aux, file=paste0(id, "_", name, ".arff"))
  }
}

#18/02/2021 - COMENTARIO PARA TESTAR SE ESSAS BASES PASSAM NO TESTE DE AUC
system(paste("rm 41496_DRSongsLyrics.arff"))
system(paste("rm 42665_ricci_vs_destefano.arff"))
system(paste("rm 924_humandevel.arff"))
system(paste("rm 470_profb.arff"))
system(paste("rm 784_newton_hema.arff"))
system(paste("rm 796_cpu.arff"))
system(paste("rm 799_fri_c0_1000_5.arff"))
system(paste("rm 839_kdd_el_nino-small.arff"))
system(paste("rm 961_analcatdata_broadwaymult.arff"))
system(paste("rm 967_cars.arff"))
system(paste("rm 996_prnn_fglass.arff"))
system(paste("rm 997_balance-scale.arff"))
system(paste("rm 42638_titanic.arff"))
system(paste("rm 914_balloon.arff"))
system(paste("rm 1046_mozilla4.arff"))
system(paste("rm 1220_Click_prediction_small.arff"))
system(paste("rm 137_BNG(tic-tac-toe).arff"))
system(paste("rm 151_electricity.arff"))
system(paste("rm 251_BNG(breast-w).arff"))
system(paste("rm 310_mammography.arff"))
system(paste("rm 4135_Amazon_employee_access.arff"))
system(paste("rm 42493_airlines.arff"))
system(paste("rm 42717_Click_prediction_small.arff"))
system(paste("rm 725_bank8FM.arff"))
system(paste("rm 803_delta_ailerons.arff"))
system(paste("rm 807_kin8nm.arff"))
system(paste("rm 816_puma8NH.arff"))
system(paste("rm 819_delta_elevators.arff"))
system(paste("rm 823_houses.arff"))
system(paste("rm 843_house_8L.arff"))
system(paste("rm 923_visualizing_soil.arff"))
system(paste("rm 959_nursery.arff"))
system(paste("rm 1489_phoneme.arff"))
system(paste("rm 1007_bridges"))
system(paste("rm 1046_mozilla4.arff"))
system(paste("rm 40945_Titanic.arff"))
system(paste("rm 4135_Amazon_employee_access.arff"))
system(paste("rm 41434_Click_prediction_small.arff"))
system(paste("802_pbcseq.arff"))


#Verificando as bases de dados baixadas. Imprimindo características básicas como número de linhas, colunas e classes:

files = list.files(pattern=".arff", full.names=TRUE)
aux = lapply(files, function(file) {
  print(file)
  data = foreign::read.arff(file)
  c(nrow=nrow(data), ncol=ncol(data), 
  classes=nlevels(data$class))
})
