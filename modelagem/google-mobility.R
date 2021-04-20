#' Obtendo dados do covid-19 no brasil com auxilio do covid-19 data hub.
#' @consulte https://covid19datahub.io/articles/api/r.html
#' @warning: esse pacote usa memória pra caramba
#' 
# install the package
#install.packages("COVID19")

library("COVID19")

# incorporando google mobility report
gmr <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

dados_brasil   <- covid19(start = "2021-01-01",
                          #end = "2020-04-22".
                          gmr = gmr, 
                          country = "BR",
                          raw = FALSE,
                          cache = FALSE)
