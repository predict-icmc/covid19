#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(tidyverse)
#install.packages('nls.multstart')
library(nls.multstart)
library(minpack.lm)
library(tibble)


dados1<-read.csv("https://covid.ourworldindata.org/data/ecdc/total_deaths.csv",header=TRUE)
dados1$tempo<- as.numeric(as.Date(dados1$date) - min(as.Date(dados1$date)))


dados <-dados1



Brazil<-dados1$Brazil


fit <- nlsLM(Brazil ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10), data=dados)

fit.Gompertz <- nlsLM(Brazil ~ SSgompertz(tempo, Asym, b2, b3),
             start = c(Asym=1500,b2=1,b3=1), data=dados)

summary(fit)
summary(fit.Gompertz)

# Predição próximas 4 semanas 
XX = (0:(max(dados$tempo)+30))

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


ggplot(dados, aes(x = tempo, y = Brazil)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = y,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Cumulative deaths by COVID-19 ', subtitle='Brazil', x = 'Days', 
       y = 'deaths', fill = '') +
theme_bw()


# Previsão para a próximo dia em SP
yp[max(dados$tempo)+1]
yp.G[max(dados$tempo)+1]

# Curva de novos casos
dados$Brazil_deathsday <- c(0,diff(Brazil))

predict$obitos_dia_preditos <-c(0,round(diff(yp),5))
predict.G$obitos_dia_preditos <-c(0,round(diff(yp.G),5))

previsao<-rbind(predict,predict.G)

ggplot(dados, aes(x = tempo, y = Brazil_deathsday)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = obitos_dia_preditos,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Deaths by COVID-19', subtitle='Brazil', x = 'Days',
       y = 'Deaths', fill='Model') +theme_bw()





###################################

# Italy



Italy<-dados1$Italy


fit <- nlsLM(Italy ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=15000,xmid=50,scal=10), data=dados)

fit.Gompertz <- nlsLM(Italy ~ SSgompertz(tempo, Asym, b2, b3),
                      start = c(Asym=15000,b2=1,b3=1), data=dados)

summary(fit)
summary(fit.Gompertz)

# Predição próximas 4 semanas 
XX = (0:(max(dados$tempo)+60))

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


ggplot(dados, aes(x = tempo, y = Italy)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = y,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Cumulative deaths by COVID-19 ', subtitle='Italy', x = 'Days', 
       y = 'deaths', fill = '') +
  theme_bw()


# Previsão para a próximo dia em SP
yp[max(dados$tempo)+1]
yp.G[max(dados$tempo)+1]

# Curva de novos casos
dados$Italy_deathsday <- c(0,diff(Italy))

predict$obitos_dia_preditos <-c(0,round(diff(yp),5))
predict.G$obitos_dia_preditos <-c(0,round(diff(yp.G),5))

previsao<-rbind(predict,predict.G)

ggplot(dados, aes(x = tempo, y = Italy_deathsday)) + 
  geom_line(size = 1, color = "blue") +
  geom_line(aes(x=x, y = obitos_dia_preditos,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Deaths by COVID-19', subtitle='Italy', x = 'Days',
       y = 'Deaths', fill='Model') +theme_bw()







###################################

# Spain


dados$Spain[149]<-dados$Spain[148]
dados$Spain[146]<-dados$Spain[145]
Spain<-dados$Spain


fit <- nlsLM(Spain ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=15000,xmid=50,scal=10), data=dados)

fit.Gompertz <- nlsLM(Spain ~ SSgompertz(tempo, Asym, b2, b3),
                      start = c(Asym=15000,b2=1,b3=1), data=dados)

summary(fit)
summary(fit.Gompertz)

# Predição próximas 4 semanas 
XX = (0:(max(dados$tempo)+60))

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


ggplot(dados, aes(x = tempo, y = Spain)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = y,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Cumulative deaths by COVID-19 ', subtitle='Spain', x = 'Days', 
       y = 'deaths', fill = '') +
  theme_bw()


# Previsão para a próximo dia em SP
yp[max(dados$tempo)+1]
yp.G[max(dados$tempo)+1]

# Curva de novos casos

dados$Spain_deathsday <- c(0,diff(Spain))
dados$Spain_deathsday[147]<-dados$Spain_deathsday[146]

predict$obitos_dia_preditos <-c(0,round(diff(yp),5))
predict.G$obitos_dia_preditos <-c(0,round(diff(yp.G),5))

previsao<-rbind(predict,predict.G)



ggplot(dados, aes(x = tempo, y = Spain_deathsday, na.rm=TRUE)) + 
  geom_line(size = 1, color = "blue") +
  geom_line(aes(x=x, y = obitos_dia_preditos,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Deaths by COVID-19', subtitle='Spain', x = 'Days',
       y = 'Deaths', fill='Model') +theme_bw()






###################################

# United.States


United.States<-dados$United.States


fit <- nlsLM(United.States ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=15000,xmid=50,scal=10), data=dados)

fit.Gompertz <- nlsLM(United.States ~ SSgompertz(tempo, Asym, b2, b3),
                      start = c(Asym=15000,b2=1,b3=1), data=dados)

summary(fit)
summary(fit.Gompertz)

# Predição próximas 4 semanas 
XX = (0:(max(dados$tempo)+60))

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


ggplot(dados, aes(x = tempo, y = United.States)) + 
  geom_point(size = 1, color = "blue") +
  geom_line(aes(x=x, y = y,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Cumulative deaths by COVID-19 ', subtitle='United.States', x = 'Days', 
       y = 'deaths', fill = '') +
  theme_bw()


# Previsão para a próximo dia em SP
yp[max(dados$tempo)+1]
yp.G[max(dados$tempo)+1]

# Curva de novos casos

dados$United.States_deathsday <- c(0,diff(United.States))
dados$United.States_deathsday[147]<-dados$United.States_deathsday[146]

predict$obitos_dia_preditos <-c(0,round(diff(yp),5))
predict.G$obitos_dia_preditos <-c(0,round(diff(yp.G),5))

previsao<-rbind(predict,predict.G)



ggplot(dados, aes(x = tempo, y = United.States_deathsday, na.rm=TRUE)) + 
  geom_line(size = 1, color = "blue") +
  geom_line(aes(x=x, y = obitos_dia_preditos,  color=model),
            data = previsao,
            show.legend = TRUE)+
  labs(title = 'Deaths by COVID-19', subtitle='United.States', x = 'Days',
       y = 'Deaths', fill='Model') +theme_bw()

