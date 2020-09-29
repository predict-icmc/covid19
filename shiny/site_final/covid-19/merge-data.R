library(tidyverse)
library(feather)
library(data.table)


# Criacao de arquivo de dados otimizado para o dashboard
# baixe o arquivo de https://data.brasil.io/dataset/covid19/caso_full.csv.gz e coloque-o na mesma pasta que este codigo funcione.
# escolha a pasta
# copie tambem os arquivos latitude-longitude-cidades.csv e latitude-longitude-estados.csv para a pasta


#setwd("~/predict-covid19/shiny/site_final/covid-19")


# otimizando a leitura de todos os casos por municipio


# dropbox token
#token <- readRDS(file = "token.rds")

# função que lê diretamente o gzip do servidor
downCorona <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}

# funcao que pega o arquivo de casos do brasil.io, faz um pequeno tratamento e envia para o dropbox os arquivos "latlong-covid.feather" e "full-covid.feather"
# ATENÇÃO: necessário possuir os arquivos 'latitude-longitude-cidades.csv' e 'latitude-longitude-estados.csv' na working directory


pegaCorona <- function(baixar = TRUE){

  if(baixar){
    print("Fazendo o download....")
    dados <- downCorona("https://data.brasil.io/dataset/covid19/caso_full.csv.gz")
    cart <- downCorona("https://data.brasil.io/dataset/covid19/obito_cartorio.csv.gz")
  }
    # caso já tenha o arquivo na pasta
  else
    dados<-read.csv(file = "caso_full.csv",header=TRUE)
  
  print("Download concluido. Transformando os dados")
dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))
dados$date <- as.Date(dados$date)

# acrescentar a media_movel
write_feather(cart,sprintf("ob-cartorio.feather"))
write_feather(dados,sprintf("full-covid.feather"))
#drop_upload("full-covid.feather", dtoken = token)


# acrescentando latitude e longitude nos ultimos casos

latlong_cidade<-read.csv('latitude-longitude-cidades.csv', sep=';', header=TRUE)
latlong_cidade$city <-latlong_cidade$municipio
latlong_cidade$state <-latlong_cidade$uf

latlong_estado<-read.csv('latitude-longitude-estados.csv', sep=';', header=TRUE)
latlong_estado$state <-latlong_estado$uf
latlong_estado$place_type='state'

dados <- dados %>% filter(is_last == "True")

dados2 <- merge(dados,latlong_cidade,by=c('state','city'), all.x=TRUE, all.y=FALSE)
dados <- merge(dados2,latlong_estado,by=c('state','place_type'), all.x=TRUE, all.y=FALSE)

dados <-dados %>%
  mutate(latitude = ifelse(place_type=='city', latitude.x, latitude.y),
         longitude = ifelse(place_type=='city',longitude.x, longitude.y))
  
dados <-select(dados, -c('uf.x','uf.y','latitude.x','longitude.x','latitude.y','longitude.y'))

dados <- dados %>% drop_na(latitude,longitude)

write_feather(dados,sprintf("latlong-covid.feather"))

print("Dados baixados e salvos com sucesso.")
#drop_upload("latlong-covid.feather", dtoken = token)
}