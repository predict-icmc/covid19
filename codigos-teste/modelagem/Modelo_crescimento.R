#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(tidyverse)
install.packages('nls.multstart')
library(nls.multstart)
library(minpack.lm)


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

# Modelo crescimento logístico estado de SP
SaoPaulo <- filter(Estados, state == 'SP')

fit <- nlsLM(deaths ~ SSlogis(semanas, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10),
             data = subset(Estados,state=='SP'))

ggplot(SaoPaulo, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=semanas,y=fitted(fit)))+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(SaoPaulo$semanas , max(SaoPaulo$semanas)+(1:4))

Asym<-coef(fit)[1]
xmid<-coef(fit)[2]
scal<-coef(fit)[3]

yp <-Asym/(1+exp((xmid-XX)/scal))

predict<-data.frame(XX,yp)

ggplot(SaoPaulo, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()

# Previsão para a próxima semana em SP
yp[max(SaoPaulo$semanas)+1]

# Curva de novos casos

SaoPaulo$obitos_semana <- c(0,diff(SaoPaulo$deaths))

predict$obitos_semana_preditos <-c(0,diff(yp))

ggplot(SaoPaulo, aes(x = semanas, y = obitos_semana)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=XX, y = obitos_semana_preditos),
            data = predict,
            colour = "red")+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)







# Modelo crescimento logístico estado de PE
Pernambuco <- filter(Estados, state == 'PE')

fit <- nlsLM(deaths ~ SSlogis(semanas, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10),
             data = subset(Estados,state=='PE'))

ggplot(Pernambuco, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=semanas,y=fitted(fit)))+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(Pernambuco$semanas , max(Pernambuco$semanas)+(1:4))

Asym<-coef(fit)[1]
xmid<-coef(fit)[2]
scal<-coef(fit)[3]

yp <-Asym/(1+exp((xmid-XX)/scal))

predict<-data.frame(XX,yp)

ggplot(Pernambuco, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()

# Previsão para a próxima semana em PE
yp[max(Pernambuco$semanas)+1]

# Curva de novos casos

Pernambuco$obitos_semana <- c(0,diff(Pernambuco$deaths))

predict$obitos_semana_preditos <-c(0,diff(yp))

ggplot(Pernambuco, aes(x = semanas, y = obitos_semana)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=XX, y = obitos_semana_preditos),
            data = predict,
            colour = "red")+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)
