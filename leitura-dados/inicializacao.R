####################Inicialização############################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #
if(!require(tidyverse)) install.packages("tidyverse")       #
if(!require(feather)) install.packages("feather")           #
if(!require(data.table)) install.packages("data.table")     #
source("merge-data.R")                                      #
pegaCorona()                                                #
#############################################################
latlong <- "latlong-covid.feather"                          #
dadoslat <- read_feather(latlong)                           #
#############################################################
caso_full <- "full-covid.feather"                           #
df<-read_feather(caso_full)                                 #
#############################################################
cartorio <- "ob-cartorio.feather"                           #
df.cartorio <- read_feather(cartorio)                       #
#############################################################
demografico <- read.csv2("Dados_demograficos.csv",sep=",")  #
#############################################################
