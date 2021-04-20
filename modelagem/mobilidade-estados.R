library(tidyverse)
library(COVID19)

url <- "https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip"

temp <- tempfile()
download.file(url,temp)

mobilidade_estados <- readr::read_csv(unz(temp, "2020_BR_Region_Mobility_Report.csv"))

dados_estados   <- COVID19::covid19(start = "2021-01-01",
                           #end = "2020-04-22".
                           gmr = mobilidade_estados, 
                           country = "BR", 
                           level = 2,
                           raw = FALSE,
                           cache = FALSE)
