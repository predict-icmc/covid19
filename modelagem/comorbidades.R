library(tidyverse)
#' leitura dos microdados de hospitalizacao
#' Casos, óbitos e doenças pré-existentes

url <- "https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/casos_obitos_doencas_preexistentes.csv.zip"

temp <- tempfile()
download.file(url,temp)

comorb <- readr::read_csv2(unz(temp,"casos_obitos_doencas_preexistentes.csv"))

