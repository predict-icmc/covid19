#install.packages('forecast', 'fpp')
library(forecast)
library(fpp)
library(tidyverse)
library(scales)
library(modelr)
library(plotly)

#Separando o banco de dados para apenas o estado de são paulo
redes <-df%>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)

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
treino<-redes$new_confirmed[1:276]
test<-redes$new_confirmed[277:290]
fit <- nnetar(treino,lambda ="auto",p=7)

summary(fit)

#Plot dos dados previsto
#não como colocar os dados test e o verdadeiros com cores diferentes

autoplot(forecast(fit,h=15))

#Teste de normalidade 
#shapiro.test(redes$new_confirmed)
##################################################

#Prevendo os dados 
predict(fit,14)
v<-data.frame(forecast(fit,14))

#EQM
sqrt(sum((v$Point.Forecast-test)^2))

#plot dados treinos

grafico<-data.frame(predito=v,test=test)

treino<-data.frame(treino=treino)
p<-ggplot()+
  geom_line(aes(x=seq(1:276),y=treino$treino),col="Blue")+
  geom_line(aes(x=seq(277,290),y=grafico$Point.Forecast),col="Red")+
  geom_line(aes(x=seq(277,290),y=grafico$test),col="Orange")+
  scale_color_discrete(name = "Y series", labels = c("Y3","Y2", "Y1"))

ggplotly(p)  
  
