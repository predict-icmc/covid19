library(tidyverse)
library(feather)

# otimizando a leitura de todos os casos por municipio
dados<-read.csv(file = "caso_full.csv",header=TRUE)
dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))
dados$date <- as.Date(dados$date)

write_feather(dados,sprintf("%s_full-covid.feather", Sys.Date()))


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

write_feather(dados,sprintf("%s_latlong-covid.feather", Sys.Date()))
