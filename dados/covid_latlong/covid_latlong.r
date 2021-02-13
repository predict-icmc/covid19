
# Leitura de dados brasileiros - COVID19

# Execute se necessário
# install.packages("tidyverse")

library(tidyverse)
library(csv)

library(dplyr)
library(hflights)
library(dplyr)


covid<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)
#View(covid)
names(covid)
nrow(covid)

covid$tempo<- as.numeric(as.Date(covid$date) - min(as.Date(covid$date)))

# setwd('/home/cibele/Google Drive/PREDICT COVID-19/covid19/dados/latitude-longitude/')

latitude_longitude_estados <- read.csv("latitude-longitude-estados.csv", header=TRUE, sep = ";", encoding = "UTF-8")
latitude_longitude_cidades <- read.csv("latitude-longitude-cidades.csv", header=TRUE, sep = ";", encoding = "UTF-8")

latitude_longitude_estados <- latitude_longitude_estados %>% 
  rename(state = uf) %>% 
  cbind(place_type = "state") %>% 
  cbind(city='NA') %>% 
  cbind(id_city='NA')

latitude_longitude_cidades <- latitude_longitude_cidades %>% 
  rename(state = uf) %>% 
  rename(id_city = id_municipio) %>% 
  rename(city = municipio) %>% 
  cbind(place_type = "city")
  

#unindo os bancos de dados de localização

localizacao_br <- rbind(latitude_longitude_cidades, latitude_longitude_estados)
localizacao_br <- localizacao_br[,-1]

# Junta covid + latlong

covid <- merge(covid,localizacao_br, by=c('state','city','place_type'))

covid <- covid %>% 
  filter(is_last == 'True')

View(covid)

write.csv(covid, file='/home/cibele/Google Drive/PREDICT COVID-19/covid19/dados/covid_latlong/covid_latlong.csv')

 