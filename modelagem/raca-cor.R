library(tidyverse)
#' leitura dos microdados de hospitalizacao
#' Casos, óbitos por raça/cor e município.

url <- "https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/casos_obitos_raca_cor.csv.zip"

temp <- tempfile()
download.file(url,temp)

racacor <- readr::read_csv2(unz(temp,"casos_obitos_raca_cor.csv"))

