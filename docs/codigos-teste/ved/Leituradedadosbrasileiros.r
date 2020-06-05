
# Leitura de dados brasileiros - COVID19

# Execute se necessário
# install.packages("tidyverse")

library(tidyverse)

dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)
View(dados)
names(dados)
nrow(dados)

dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))

linhas.SP <- which(dados$city=='São Paulo')
dados.SaoPaulo <- dados[linhas.SP,]

View(dados.SaoPaulo)

ggplot(data = dados.SaoPaulo)

ggplot(dados.SaoPaulo) +
  geom_point(aes(x = tempo, y = confirmed, colour='red'))



# Modelo de crescimento logístico de três parâmetros

y<-dados.SaoPaulo$confirmed
x<-as.numeric(dados.SaoPaulo$tempo)

# Ajuste do modelo
fit<-nls(y ~ SSlogis(x,Asym,xmid,scal))



# Visualização com ggplot

ggplot(data = dados.SaoPaulo)

ggplot(dados.SaoPaulo) +
  geom_point(aes(x = x, y = confirmed))+
  geom_line(aes(x = x, y = fitted(fit), colour='red'))+
  ggtitle('COVID-19 em São Paulo SP') + # for the main title
  xlab('Tempo (dias desde 25-02-2020)') +# for the x axis label
  ylab('Número de casos confirmados') # for the y axis label


