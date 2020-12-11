#install.packages('forecast', 'fpp')
library(forecast)
library(fpp)
library(tidyverse)
library(scales)

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
fit <- nnetar(redes$new_confirmed,lambda=0.07,subset =redes$new_confirmed[1:270])
summary(fit)

#Plot dos dados previsto
#não como colocar os dados test e o verdadeiros com cores diferentes
autoplot(forecast(fit))


#Teste de normalidade 
#shapiro.test(redes$new_confirmed)
##################################################

