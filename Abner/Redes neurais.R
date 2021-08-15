#install.packages('forecast', 'fpp')
 library(forecast)
# library(fpp)
 library(tidyverse)
  library(scales)
# library(modelr)
# library(plotly)
library(Metrics)
 library(gather.covid)
library(zoo)
# library(caret)

# library(reshape)
 library(neuralnet)
 library(spatstat)
 library(tsfgrnn)
 library(Hmisc)
 library(nnet)
df<-pegaCorona(tipo="caso_full",baixar = T)
pegaCorona


#Separando o banco de dados para apenas o estado de são paulo
redes <- df %>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)


df
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
460-14
treino<-redes[1:446,]
test<-redes[447:460,]
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

  
redes <-df%>%
  filter(state=="SP" & place_type=="city")

model1<-nnetar(redes$new_confirmed,xreg=redes$DRS,p=7)

a<-c(202106,202106,202106,202106,202107,202107,202107,202107,202107,202107,202107,202108,202108,202108)


autoplot(forecast(model1,h=14,xreg=))

autoplot(model1$fitted)

#######################################################################################
#Modelo aplicado para dados da MEDIA MOVEL 

mediannetar<-rollmean(ts(redes$new_confirmed),7,fill=NA,align="right")

mediannetar<-data.frame(redes$date,mediannetar)


mediannetar<- na.omit(mediannetar)
colnames(mediannetar)<- c("date","mediamovel")

treino<-mediannetar[1:446,]
test<-mediannetar[447:460,]
modelmedia <-nnetar(treino$mediamovel, repeats = 20)
#help(nnetar)



autoplot(forecast(modelmedia,h=24))
v <- data.frame((forecast(modelmedia,h=14)))

ajust_mm <- data.frame(mediannetar$date[1:446],modelmedia$fitted)
colnames(ajust_mm) <- c("date","ajustados")
ajust_mm<-drop_na(ajust_mm)
previsao_mm <- data.frame(mediannetar$date[447:460],v$Point.Forecast)
colnames(previsao_mm) <- c("date","previsao")


p<- ggplot()+
  geom_line(aes(x=ajust_mm$date,y=ajust_mm$ajustados,col="Valores Ajustados"),linetype=1)+
  geom_line(aes(x=mediannetar$date,y=mediannetar$mediamovel,col="Valores Reais"),linetype=1)+
  geom_line(aes(x=previsao_mm$date,y=previsao_mm$previsao,col="Previsão"),linetype=1)+
  scale_color_manual(values = c("Valores Ajustados" = "Blue", "Valores Reais" = "Black","Previsão" = "Red")) +
  theme(legend.position=c(0.8, 0.1))+
  xlab("Data")+
  ylab("Media Movel de Novos Casos")+
  ggtitle("Previsão de 14 dias da media movel de 7 dias de novos casos de covid-19 para o Estado de São Paulo")
p


########################################################################################
#Comparação Modelos da MEdia movel e serie original

treino<-redes[1:452,]
test<-redes[453:466,]
model <-nnetar(treino$new_confirmed, repeats = 20,p=7)

autoplot(forecast(model,h=14))
v <- data.frame((forecast(model,h=14)))


ajust <- data.frame(redes$date[1:452],model$fitted)
colnames(ajust) <- c("date","ajustados")
ajust<-drop_na(ajust)
previsao <- data.frame(redes$date[453:466],v$Point.Forecast)
colnames(previsao) <- c("date","previsao")

p<- ggplot()+
  geom_line(aes(x=ajust$date,y=ajust$ajustados,col="Valores Ajustados"),linetype=1)+
  geom_line(aes(x=redes$date,y=redes$new_confirmed,col="Valores Reais"),linetype=1)+
  geom_line(aes(x=previsao$date,y=previsao$previsao,col="Previsão"),linetype=1)+
  scale_color_manual(values = c("Valores Ajustados" = "Blue", "Valores Reais" = "Black","Previsão" = "Red")) +
  theme(legend.position=c(0.94, 0.9))+
  xlab("Data")+
  ylab("Media Movel de Novos Casos")+
  ggtitle("Previsão de 14 dias de novos casos de Covid-19 para o estado de São Paulo")
p

model <- nnetar(ts(redes$new_confirmed))
autoplot(forecast(model,h=14))

#######################################################################3
#Visualização redes neurais
table(df$date)

str(fit)
str(fit$model[[20]]$wts)
library(devtools)
source_url('https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r')
require(devtools)
source_gist('5086859')

fit$model[[1]]$wts
scales::rescale(fit$model[[20]]$wts)
fit$model[[20]]
wts.in <-fit$model[[20]]$wts
struct<-fit$model[[20]]$n
wts.in<-data.frame(wts=wts.in)

tiff("Abner/Relatorio/redes_neurais_sp.jpg", units="in", width=5, height=5, res=300)
plot.nnet(wts.in,struct=struct)
dev.off()


neuralnet(wts.in,struct)
plot.nn(wts.in,struct=struct)
plot(x=model$model[[20]])

mpl(wts.in)
plot(model)
class(model$model[[20]])
# nueralModel <- neuralnet(formula = wts.in, hidden = c(4,2), linear.output = T, data = train_nn)
# mod1<-nnet(redes$new_confirmed,redes$new_confirmed,data=wts.in,size=10,linout=T)
# plot.nnet(mod1)
# 
# mod1<- neuralnet(formula = model$model[[20]]$call,hidden=c(7,4,1),linear.output = ,data=redes$new_confirmed)
# 
# model$model[[20]]$call
#model$model[[20]]$

lag_data<- data.frame(Lag(redes$new_confirmed,1),Lag(redes$new_confirmed,2),Lag(redes$new_confirmed,3),
                      Lag(redes$new_confirmed,4),Lag(redes$new_confirmed,5),Lag(redes$new_confirmed,6)
                      ,Lag(redes$new_confirmed,7),Lag(redes$new_confirmed,8),redes$new_confirmed,redes$date)

lag_data<- lag_data %>% drop_na()

colnames(lag_data)<-c("lag1","lag2","lag3","lag4","lag5","lag6","lag7","lag8","new_confirmed","date")
lag_data


nn <- neuralnet(new_confirmed~lag1+lag2+lag3+lag4+lag5+lag6+lag7+lag8,lag_data, hidden = 4, linear.output = F )
plot(nn)

grnn_weights(model)
grnn_weights(model)
model<-grnn_forecasting(redes$new_confirmed, h = 4)
model<-grnn_forecasting(redes$new_confirmed, h = 2, lags =7)
plot(model)



