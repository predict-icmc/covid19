library(sf)

#install.packages("dplyr")
library(dplyr)

library(tmap)  
library(brazilmaps)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(spDataLarge)
#install.packages('leaflet')
library(leaflet)
library(tidyverse)

dados1<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)
dados1$tempo<- as.numeric(as.Date(dados1$date) - min(as.Date(dados1$date)))

latlong_cidade<-read.csv('latitude-longitude-cidades.csv', sep=';', header=TRUE)
latlong_cidade$city <-latlong_cidade$municipio
latlong_cidade$state <-latlong_cidade$uf

latlong_estado<-read.csv('latitude-longitude-estados.csv', sep=';', header=TRUE)
latlong_estado$state <-latlong_estado$uf
latlong_estado$place_type='state'

#Juntando as bases de dados
dados2 <- merge(dados1,latlong_cidade,by=c('state','city'), all.x=TRUE, all.y=FALSE)
dados <- merge(dados2,latlong_estado,by=c('state','place_type'), all.x=TRUE, all.y=FALSE)

dados <-dados %>%
  mutate(latitude = ifelse(place_type=='city', latitude.x, latitude.y),
         longitude = ifelse(place_type=='city',longitude.x, longitude.y)) 


dados <-select(dados, -c('uf.x','uf.y','latitude.x','longitude.x','latitude.y','longitude.y'))

# Criando variável semanas
dados$semanas<- floor(dados$tempo/7)
#View(dados)

# Variável que é igual a 0 no final de cada semana. 
dados$final_semana <- dados$tempo%%7

# Pegando somente valores no final de cada semana 
teste = subset(dados, dados$final_semana==0)

# filtrando os dados, selecionando apenas os estados

Estados <- filter(teste, teste$place_type == 'state')
View(Estados)

# adicionando a coluna de mortes diária, dado que as mortes estavam acumuladas

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(semanas)%>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))


# Comparando estado por estado

Estados <- filter(teste, place_type == 'state')

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(tempo) %>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))

ggplot(Estados, aes(x = semanas, y = deaths)) + 
  geom_line (size = 1, color = "blue") +
  labs(title = 'Total de Mortes por COVID em semanas', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

ggplot(Estados, aes(x = semanas, y = deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais confirmadas por COVID-19', 
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()


ggplot(Estados, aes(x = semanas, y = Estados$confirmed_per_100k_inhabitants, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais de COVID-19', 
       x = 'Semanas', y = 'Número de óbitos/100 mil habitantes') +
  theme_bw()

ggplot(Estados, aes(x = semanas, y = log(Estados$confirmed_per_100k_inhabitants), group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais de COVID-19 (escala logarítmica)', 
       x = 'Semanas', y = 'Número de óbitos/100 mil habitantes') +
  theme_bw()

ggplot(Estados, aes(x = semanas, y = death_rate, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais/casos confirmados de COVID-19', 
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()






#---------------------------------------------------------------------------------
#separando em regiões
# Conferir se fiz as substituições certas para o gráfico em semanas. 
Sudeste <- filter(Estados, state == 'SP'|state == 'MG'|state == 'RJ'| state == 'ES')

ggplot(Sudeste, aes(x = semanas, y =deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em semanas', subtitle = "Sudeste",
       x = 'Semanas', y = 'Número de óbitos', fill = 'estados')  +
  theme_bw()

ggplot(Sudeste, aes(x = semanas, y =death_rate , group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais/casos confirmados de COVID-19', subtitle = "Sudeste",
       x = 'Semanas', y = 'Número de óbitos', fill = 'estados')  +
  theme_bw()

# Sudeste Log scale
Sudeste <- filter(Estados, state == 'SP'|state == 'MG'|state == 'RJ'| state == 'ES')
for(i in Sudeste$state){
  Sudeste$tempo_2[(Sudeste$state==i)&(Sudeste$deaths>0)] = 
    Sudeste$semanas[(Sudeste$state==i)&(Sudeste$deaths>0)] -
    min(Sudeste$semanas[(Sudeste$state==i)&(Sudeste$deaths>0)])
}
  
sud = ggplot(Sudeste, aes(x = tempo_2, y =deaths, group = state)) + 
    geom_line(aes(col = state), size = 1) +
    labs(title = 'Total de Mortes por COVID em Log Scale', subtitle = "Sudeste",
         x = 'Tempo', y = 'Óbitos', fill = 'estados')  +
    theme_bw()
sud + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')

##############

Norte<-filter(Estados,state=="AC"|state=="AP"| state=="AM" | state=="PA"| state=="RO"| state=="RR"| state=="TO")

ggplot(Norte, aes(x = semanas, y = deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em semanas', subtitle = "Norte", 
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()

# Norte Log scale
Norte<-filter(Estados,state=="AC"|state=="AP"| state=="AM" | state=="PA"| state=="RO"| state=="RR"| state=="TO")
for(i in Norte$state){
  Norte$tempo_2[(Norte$state==i)&(Norte$deaths>0)] = 
    Norte$semanas[(Norte$state==i)&(Norte$deaths>0)] -
    min(Norte$semanas[(Norte$state==i)&(Norte$deaths>0)])
}

norte = ggplot(Norte, aes(x = tempo_2, y =deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em Log Scale', subtitle = "Norte",
       x = 'Tempo', y = 'Óbitos', fill = 'estados')  +
  theme_bw()
norte + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')
####################

Sul<-filter(Estados,state=="PR"|state=="RS"| state=="SC")

ggplot(Sul, aes(x = semanas, y = deaths, group = state)) + 
  geom_line(aes(col = state), size = 1, alpha = 0.8) +
  labs(title = 'Total de Mortes por COVID em semanas', subtitle = "Sul",
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()

ggplot(Sul, aes(x = semanas, y = death_rate, group = state)) + 
  geom_line(aes(col = state), size = 1, alpha = 0.8) +
  labs(title = 'Mortes semanais/casos confirmados de COVID-19', subtitle = "Sul",
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()

# Sul Log scale
Sul<-filter(Estados,state=="PR"|state=="RS"| state=="SC")
for(i in Sul$state){
  Sul$tempo_2[(Sul$state==i)&(Sul$deaths>0)] = 
    Sul$semanas[(Sul$state==i)&(Sul$deaths>0)] -
    min(Sul$semanas[(Sul$state==i)&(Sul$deaths>0)])
}

sul = ggplot(Sul, aes(x = tempo_2, y =deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em Log Scale', subtitle = "Sul",
       x = 'Tempo', y = 'Óbitos', fill = 'estados')  +
  theme_bw()
sul + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')
####################


Centroeste<-filter(Estados,state=="DF"|state=="GO"| state=="MT"|state=="MS")

ggplot(Centroeste, aes(x = semanas, y = deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em semanas',subtitle = "Centro Oeste", 
       x = 'Semanas', y = 'Número de óbitos', fill = 'estados') +
  theme_bw()

ggplot(Centroeste, aes(x = semanas, y = death_rate, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais/casos confirmados de COVID-19',subtitle = "Centro Oeste", 
       x = 'Semanas', y = 'Número de óbitos', fill = 'estados') +
  theme_bw()

# Centroeste Log scale
Centroeste<-filter(Estados,state=="DF"|state=="GO"| state=="MT"|state=="MS")
for(i in Centroeste$state){
  Centroeste$tempo_2[(Centroeste$state==i)&(Centroeste$deaths>0)] = 
    Centroeste$semanas[(Centroeste$state==i)&(Centroeste$deaths>0)] -
    min(Centroeste$semanas[(Centroeste$state==i)&(Centroeste$deaths>0)])
}

cent = ggplot(Centroeste, aes(x = tempo_2, y =deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em Log Scale', subtitle = "Centroeste",
       x = 'Tempo', y = 'Óbitos', fill = 'estados')  +
  theme_bw()
cent + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')
####################



Nordeste<-filter(Estados,state=="AL"|state=="BA"| state=="CE" | state=="MA"| state=="PB"| state=="PI"| state=="PE"|state=="RN"| state=="SE")

ggplot(Nordeste, aes(x = semanas, y = deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em semanas', subtitle = "Nordeste", 
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()


ggplot(Nordeste, aes(x = semanas, y = death_rate, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Mortes semanais/casos confirmados de COVID-19', subtitle = "Nordeste", 
       x = 'Semanas', y = 'Número de óbitos') +
  theme_bw()


# Nordeste Log scale
Nordeste<-filter(Estados,state=="AL"|state=="BA"| state=="CE" | state=="MA"| state=="PB"| state=="PI"| state=="PE"|state=="RN"| state=="SE")
for(i in Nordeste$state){
  Nordeste$tempo_2[(Nordeste$state==i)&(Nordeste$deaths>0)] = 
    Nordeste$semanas[(Nordeste$state==i)&(Nordeste$deaths>0)] -
    min(Nordeste$semanas[(Nordeste$state==i)&(Nordeste$deaths>0)])
}

nord = ggplot(Nordeste, aes(x = tempo_2, y =deaths, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Total de Mortes por COVID em Log Scale', subtitle = "Nordeste",
       x = 'Tempo', y = 'Óbitos', fill = 'estados')  +
  theme_bw()
nord + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')

#---------------------------------------------------------------------------------

# Novo gráfico teste - Luna 

# Leitura pelo import Dataset dos dados de isolamento do estado de SP
# Tempo com base no primeiro caso
isolamento$tempo<- as.numeric(as.Date(isolamento$DATA) - min(as.Date(dados$date)))

# Colocando em semanas
isolamento$semanas<-floor(isolamento$tempo/7)
isolamento$final_semana <- isolamento$tempo%%7

# Pegando somente dados do final da semana
isola = subset(isolamento, final_semana==0,select = c("semanas","UF","Índice.de.isolamento"))

# Agregando e fazendo a média para o estado de SP, posteriormente conseguimos fazer por cidade
isola_mean = aggregate(isola, by=list(isola$semanas), FUN = mean)
View(isola_mean)


sp <- filter(Estados, state == 'SP')

compara = merge(sp, isola_mean, all.x=TRUE)
compara$Índice.de.isolamento[is.na(compara$Índice.de.isolamento)] <- 0

# Gráfico de Índice de Isolamento vs número de mortes
par()
opar <- par()
par(mar=c(5,4,4,5)+.1)
plot(x=compara$semanas, y=compara$deaths,type='l',col='red', lty=2, lwd=2,
     xlab='', ylab='Deaths')
par(new=T)
plot(x=compara$semanas,y = compara$Índice.de.isolamento, type='l', col='black',
     xaxt='n',yaxt='n', lty=1, lwd=2,
     xlab='', ylab='', ylim=c(0,100))
axis(4)
mtext('Isolation',side=4,line=3)
legend('bottomright', col=c('red','black'), lty=c(2,1), lwd=2,
       legend=c('SP', 'Isolation'))
grid(lty=2, lwd=1, col='darkgrey')
mtext('Deaths vs Isolation',side=3,line=1, col='black',
      font=2)
par(opar)

# Gráfico de Índice de Isolamento vs número de casos 
par()
opar <- par()
par(mar=c(5,4,4,5)+.1)
plot(x=compara$semanas, y=compara$confirmed,type='l',col='red', lty=2, lwd=2,
     xlab='', ylab='Confirmed Cases')
par(new=T)
plot(x=compara$semanas,y = compara$Índice.de.isolamento, type='l', col='black',
     xaxt='n',yaxt='n', lty=1, lwd=2,
     xlab='', ylab='', ylim=c(0,100))
axis(4)
mtext('Isolation',side=4,line=3)
legend('bottomright', col=c('red','black'), lty=c(2,1), lwd=2,
       legend=c('SP', 'Isolation'))
grid(lty=2, lwd=1, col='darkgrey')
mtext('Confirmed Cases vs Isolation',side=3,line=1, col='black',
      font=2)
par(opar)

#---------------------------------------------------------------------------------


#---------------------------------------------------------------------------------

# Ainda não está funcionando com semanas - Luna
#Comparando a porcentagem tomada por estado dentro da região

## Sudeste

ggplot(Sudeste, aes(x = semanas, y= deaths, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(20,max(Sudeste$semanas))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em semanas', x = 'Semanas', 
       y = 'Número de óbitos', subtitle = 'Sudeste') +
  theme_bw()

## Nordeste

#trocar a paleta
ggplot(Nordeste, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Nordeste$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Nordeste') +
  theme_bw()

## Sul

ggplot(Sul, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Sul$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Sul') +
  theme_bw()

## Centro oeste

ggplot(Centroeste, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Centroeste$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'porcentagem', subtitle = 'Centro Oeste') +
  theme_bw()

## Norte

ggplot(Norte, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Norte$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Norte') +
  theme_bw()

#---------------------------------------------------------------------------------

# Aplicar semanas aqui, não consigui instalar os pacotes ainda - Luna
# Proporção de óbitos por região dentro do Brasil

Regioes <- Estados %>%
  mutate(Região = case_when(
    state %in% c("SP","RJ","MG","ES")~"Sudeste",
    state %in% c("PR","RS","SC")~"Sul",
    state %in% c("AC","AP","AM","PA","RO","RR","TO")~"Norte",
    state %in% c("AL","CE","BA","MA","PB","PI","PE","RN","SE")~"Nordeste",
    state %in% c("DF","GO","MT","MS")~"Centro-Oeste"
  ))

p <-ggplot(Regioes,aes(x = tempo, y = obitos_dia, fill = Região)) 

p+geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(20,max(Regioes$tempo))) +
  labs(title = 'Proporção de óbitos por COVID-19 por região', 
       x = 'Dias', y = 'Proporção de óbitos') +
  theme_bw()


#---------------------------------------------------------------------------------
 # mapa estático COVID - 19

 # filtrando os dados para o numero de mortes (acumulado) atualizado

mortes <- Estados %>% 
  filter(is_last == "True") %>%
  select(state,city_ibge_code, deaths)

sum(mortes$deaths)

 # plotando o mapa 

mapa <- get_brmap("State") %>%
  inner_join(mortes, c("State" = "city_ibge_code"))

mapa %>% 
  ggplot() +
  geom_sf(aes(fill = deaths),
          col = "black", size = 0.23)+
  scale_fill_viridis_c(option = 8) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())






#Estados[Estados$semanas==12,]





#install.packages('rworldmap','RgoogleMaps')
library(rworldmap)
library(RgoogleMaps)
library(leaflet)


