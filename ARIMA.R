# Chamando o arquivo de inicialização antes desse projeto para carregar os bancos de dados
# Os bancos de dados utilizados nesse trabalho são: "df"

library(stats)
library(tidyverse)

#Separando o banco de dados para apenas casos confirmados no estado de são paulo
ts_sampa <-df%>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)

# para utilizar o modelo ARIMA, temos que trabalhar encima de algumas pressuposições

# Os dados precisam ser estacionários
# Os dados precisam ser univariados
#

#transformando os dados para o formato de "time series"

ts_sampa <- ts(ts_sampa$new_confirmed)
view(ts_sampa)

arima(ts_sampa, seasonal = c())
