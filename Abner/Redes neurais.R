#install.packages('forecast', 'fpp')
library(forecast)
library(fpp)
library(tidyverse)
library(scales)
library(modelr)
library(plotly)
library(Metrics)
library(gather.covid)
library(zoo)
df<-pegaCorona(tipo="caso_full",baixar = T)

#Separando o banco de dados para apenas o estado de são paulo
redes <-df%>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)

max(df$date)
#Não precisa padronizar os dados eu acho(?)
#redes$new_confirmed<-rescale(redes$new_confirmed,to=c(0,1))

#Dados visualização
summary(redes)
View(redes)

#Transformar os dados em series temporais
#Não consegui colocar na função de uma maneira facil
#redes<- ts(redes)

#Essa função usar para tentar achar o melhor lambda porem não tenho certeza 
#Se é essas função que deve ser usada
?BoxCox.lambda
BoxCox.lambda(redes$new_confirmed)

#Fitar o modelo com 270 linhas dos casos confirmados

treino<-redes[1:306,]
test<-redes[307:320,]
?nnetar
treinots<-ts(treino)

fit <- nnetar(treinots[,"new_confirmed"],p=7)

summary(fit)

#Plot dos dados previsto
#não como colocar os dados test e o verdadeiros com cores diferentes

autoplot(forecast(fit,h=14))

#Teste de normalidade 
#shapiro.test(redes$new_confirmed)
##################################################

#Prevendo os dados 
predict(fit,14)
v<-data.frame(forecast(fit,14))

#EQM
rmse(v$Point.Forecast,test$new_confirmed)
#plot dados treinos


grafico<-data.frame(predito=v,test=test)

treino<-data.frame(treino=treino)
p<-ggplot()+
  geom_line(aes(x=seq(1:276),y=treino$treino),col="Blue")+
  geom_line(aes(x=seq(277,290),y=grafico$Point.Forecast),col="Red")+
  geom_line(aes(x=seq(277,290),y=grafico$test),col="Orange")+
  scale_color_discrete(name = "Y series", labels = c("Y3","Y2", "Y1"))

ggplotly(p)  

################################################################
fit <- nnetar(redes$new_confirmed,p=7)
fit3 <- Arima(redes$new_confirmed, order=c(0,1,1), seasonal=c(0,0,8))

rmse(fit3$fitted,redes$new_confirmed)
rmse(fit$fitted[8:325],redes$new_confirmed[8:325])


class(redes$new_confirmed)
class(as.integer(fit$fitted))

##################################################################


novodf<-teste %>% 
  group_by(DRS,date) %>%
  summarise(sum(new_confirmed)) %>%
  

names(novodf)<-c("DRS","date","novos_casos")

min(leitos3$datahora)
 

 
  novodf$media <- rollmean(ts(novodf$),7,fill=NA,align="right")


novodf <-novodf %>% 
  filter(date>="2020-10-08")

redes_novo<-novodf %>% 
  filter(DRS==1)

fit <- nnetar(redes_novo$media,p=7)

v <- data.frame((forecast(fit,h=14)))


# Primeiro: A forma como estão os dados faz diferença na previsão (média movel e  DRS). 
# Segundo: Devemos colocara data do começo dos casos para a mesma data dos leitos.
# Terceiro: Como vamos transformar novos casos para leitos ocupados.

# Numero de casos com a previsão X percentual ocupação.


modelodistritos<- function(drs){
  C <- teste %>% 
    filter(DRS==drs)
  uc <- unique(C$city)
  model<-vector()
  previsao<-c(rep(0,14))
  for (i in uc){
    banco <- teste %>% 
      filter(city==i) %>% 
      select(date,new_confirmed)
    model <- nnetar(rollmean(ts(banco$new_confirmed),7,fill=NA,align="right"),p=7)
    pre <- data.frame(forecast(model,h=14))
    previsao <- previsao+pre$Point.Forecast
  }
return(previsao)
}

prev<-modelodistritos(1)

d1<-novodf %>% 
  filter(DRS==1)

p<- ggplot()+
  geom_line(aes(x=seq(1,345),y=d1$media[7:351]),col="Black")+
  geom_line(aes(x=seq(346,359),y=prev,col="Função"),)+
  geom_line(aes(x=seq(346,359),y=v$Point.Forecast,col="DRS"))
p




max(leitos3$total_covid_uti_mm7d)

leitos3 %>% 
  filter(pacientes_uti_mm7d==5985)

novodf %>% 
  filter(date=="2021-01-21" & DRS==1)
prev

4126=66.38889
=100
4126*100/66.38889


#################################################################################

  
redes <-teste%>%
  filter(state=="SP" & place_type=="city")

model1<-nnetar(redes$new_confirmed,xreg=redes$DRS,p=7)

a<-c(202106,202106,202106,202106,202107,202107,202107,202107,202107,202107,202107,202108,202108,202108)


autoplot(forecast(model1,h=14,xreg=))

autoplot(model1$fitted)
