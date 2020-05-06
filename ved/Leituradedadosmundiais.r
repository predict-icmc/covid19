# Leitura de dados mundiais - COVID19
# Execute se necessário
# install.packages("tidyverse")

library(tidyverse)
library(minpack.lm)

dados<-read.csv("https://covid.ourworldindata.org/data/ecdc/total_cases.csv",header=TRUE)
View(dados)
names(dados)
nrow(dados)

dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))

#dados$date<-as.Date.numeric(dados$date, min(dados$date))

ggplot(dados) +
  geom_point(aes(x = tempo, y = dados$Brazil, colour='red'))

# Modelo de crescimento logístico

y<-dados$Brazil
x<-dados$tempo

# Ajuste do modelo de crescimento logístico de três parâmetros SSlogis
fit<-nls(y ~ SSlogis(x,Asym,xmid,scal))
summary(fit)

# Visualização com ggplot

ggplot(data = dados)

ggplot(dados) +
  geom_point(aes(x = tempo, y = Brazil))+
  geom_line(aes(x = tempo, y = fitted(nlsLM(Brazil ~ SSlogis(tempo,Asym,xmid,scal), start = c(Asym=15000,xmid=50,scal= 5))), colour='red'))+
  ggtitle('COVID-19 in Brazil') + # for the main title
  xlab('Tempo (days since 2020-12-31)') +# for the x axis label
  ylab('Confirmed cases') # for the y axis label



ggplot(dados) +
  geom_point(aes(x = tempo, y = United.States))+
  geom_line(aes(x = tempo, y = fitted(nlsLM(United.States ~ SSlogis(tempo,Asym,xmid,scal), start = c(Asym=200000,xmid=50,scal= 5))), colour='red'))+
  ggtitle('COVID-19 in the United States') + # for the main title
  xlab('Tempo (days since 2020-12-31)') +# for the x axis label
  ylab('Confirmed cases') # for the y axis label
