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

#####pegando apenas o último dia#############################
df$date <- as.Date(df$date)                                 #
df <- subset(df, date == max(date))                         #
#############################################################

###tirando acentos##############################################
df$city <- iconv(df$city, from ="UTF-8",to = "ASCII//TRANSLIT")#
df$city <- toupper(df$city)
################################################################

# renomeando as colunas

df <- df %>% 
  rename(
    uf = state,
    MUNICIPIO = city,
    tipo = place_type)

df$tipo <- str_replace_all(df$tipo, "city", "cidade")
df$tipo <- str_replace_all(df$tipo, "state", "estado")

last.data <- full_join(df, demografico, by = c('uf','MUNICIPIO','tipo'))
