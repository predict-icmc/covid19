library(gather.covid)
library(tidyverse)
library(forecast)

model<-nnetar(csp_data$new_confirmed,p=7)
model
autoplot(forecast(model,h=14))



treino<-redes[1:446,]
test<-redes[447:460,]
?nnetar
treinots<-ts(treino)

fit <- nnetar(treinots[,"new_confirmed"],p=7)

summary(fit)

?nnetar
