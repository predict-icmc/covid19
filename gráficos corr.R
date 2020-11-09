#bancos de dados a serem utilizados + inicialização 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("merge-data.R")
pegaCorona()
demografico <- read.csv2("Dados_demograficos.csv",sep=",")