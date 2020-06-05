#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(tidyverse)
#install.packages('nls.multstart')
library(nls.multstart)
library(minpack.lm)
library(tibble)


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



# Modelo crescimento logístico estado de SP
SaoPaulo <- filter(dados, dados$city == 'São Paulo')
SaoPaulo <- SaoPaulo[order(SaoPaulo$tempo),]

fit <- nlsLM(deaths ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10),
             data = subset(dados,city=='São Paulo'))

fit.Gompertz <- nlsLM(deaths ~ SSgompertz(tempo, Asym, b2, b3),
             start = c(Asym=1500,b2=1,b3=1),
             data = subset(dados,city=='São Paulo'))

summary(fit)
summary(fit.Gompertz)

# Predição próximas 4 semanas 
XX = (0:(max(SaoPaulo$tempo)+120))

Asym<-coef(fit)[1]
xmid<-coef(fit)[2]
scal<-coef(fit)[3]

Asym.G<-coef(fit.Gompertz)[1]
b2.G<-coef(fit.Gompertz)[2]
b3.G<-coef(fit.Gompertz)[3]

yp<-0
yp.G<-0

yp <-Asym/(1+exp((xmid-XX)/scal))
yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)

predict<-data.frame(x=XX,y=yp,model='Logistic')
predict.G<-data.frame(x=XX,y=yp.G,model='Gompertz')

previsao<-rbind(predict,predict.G)


ggplot(SaoPaulo, aes(x = tempo, y = deaths)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = y,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Mortes acumuladas por COVID-19 em dias', subtitle='Cidade de SP', x = 'Dias', 
       y = 'Número de óbitos', fill = '') +
theme_bw()


# Previsão para a próximo dia em SP
yp[max(SaoPaulo$tempo)+1]
yp.G[max(SaoPaulo$tempo)+1]

# Curva de novos casos
SaoPaulo$obitos_dia<- c(0,diff(SaoPaulo$deaths))
predict$obitos_dia_preditos <-c(0,round(diff(yp),5))
predict.G$obitos_dia_preditos <-c(0,round(diff(yp.G),5))

previsao<-rbind(predict,predict.G)

ggplot(SaoPaulo, aes(x = tempo, y = obitos_dia)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = obitos_dia_preditos,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Mortes por COVID-19 em dias', subtitle='Cidade de São Paulo', x = 'Dias',
       y = 'Número de óbitos', fill='Model') +theme_bw()


