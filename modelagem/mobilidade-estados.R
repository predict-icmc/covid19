library(tidyverse)
library(COVID19)
#' Script que le os dados estaduais do GMR

url <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

temp <- tempfile()
download.file(url,temp)

mobilidade_estados <- readr::read_csv(unz(temp, "2020_BR_Region_Mobility_Report.csv"))
