dados <- data.table::fread("http://www.giscard.com.br/coronavirus/arquivos/painel-covid19-giscard-pais6.csv")
library(tidyverse)

dados %>% ggplot() + geom_bar(aes(x ="Data Casos", y =  "Media Movel Casos 7 dias"))
dados$`Data Casos`
dados$`Media Movel Casos 7 dias`