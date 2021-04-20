library(tidyverse)
library(data.table)

#' Dados de ocupação do SUS por município
#' consulte https://opendatasus.saude.gov.br/en/dataset/registro-de-ocupacao-hospitalar
#' 
dt <- data.table::fread("https://s3-sa-east-1.amazonaws.com/ckan.saude.gov.br/Leitos/2021-03-23/esus-vepi.LeitoOcupacao.csv")
