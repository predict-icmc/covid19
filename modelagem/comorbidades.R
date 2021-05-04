library(tidyverse)
#' leitura dos microdados de hospitalizacao
#' Casos, óbitos e doenças pré-existentes

url <- "https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/casos_obitos_doencas_preexistentes.csv.zip"

temp <- tempfile()
download.file(url,temp)

comorb <- readr::read_csv2(unz(temp,"casos_obitos_doencas_preexistentes.csv"))
#mort <- comorb %>% filter(obito == 1)

fit <- glm(obito  ~ idade + cs_sexo + asma + cardiopatia + diabetes +
    doenca_hematologica +      doenca_hepatica +        
    doenca_neurologica      + doenca_renal +           
    imunodepressao          + obesidade +
    outros_fatores_de_risco + pneumopatia +
    puerpera                + sindrome_de_down,
    family = binomial(link=logit), data = comorb)
