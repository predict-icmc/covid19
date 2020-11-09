library(tidyverse)
library(zoo)
#####Grafico Final#########
##Porcentagens Capital#####



##Adicionando a uma coluna dizendo se a cidade é capital ou não#########
novodf<-df %>% mutate(capital= case_when(
  city_ibge_code %in% c(1100205,1302603,1200401,5002704,1600303,5300108,1400100,
                        5103403,1721000,3550308,2211001,3304557,1501402,5208707,2927408,
                        4205407,2111300,2704302,4314902,4106902,3106200,
                        2304400,2611606,2507507,2800308,2408102,3205309) ~1,
  TRUE~0
))
########################################################################
#Pegando o estado e sua capital
cidade<-novodf%>% 
  dplyr::filter(state=="SP"& capital==1)
estado<-novodf %>% 
  dplyr::filter(state=="SP" & place_type=="state")

#Transformando em uma série temporal
estado_serie<-zoo(estado$new_confirmed,estado$date)
cidade_serie<-zoo(cidade$new_confirmed,cidade$date)

#Calculando a média movel para o estado e a capital 
estado_media_movel<-rollmean(estado_serie,7,align = c("left"))
cidade_media_movel<-rollmean(cidade_serie,7,align = c("left"))

#Fazendo a taxa da cidade pelo estado
taxa<-data.frame(dia=index(cidade_media_movel),porcento=cidade_media_movel/estado_media_movel)

#Gráfico
taxa %>% 
  ggplot(aes(x=dia,y=porcento))+ 
  geom_point()+
  geom_line()+
  xlab("Semana Epidemiologica")+
  scale_y_continuous("Taxa da Cidade de São Paulo",breaks = seq(0,1,0.1),limits = c(0,1))
############################################################################