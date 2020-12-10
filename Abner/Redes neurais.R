install.packages('forecast', 'fpp')
library(forecast)
library(fpp)
library(tidyverse)
library(scales)
View(df)

redes <-df%>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)

#redes$new_confirmed<-rescale(redes$new_confirmed,to=c(0,1))
summary(redes)
View(redes)
#redes<- ts(redes)

?BoxCox.lambda
BoxCox.lambda(redes$new_confirmed)
fit <- nnetar(redes$new_confirmed,p=24,subset =redes$new_confirmed[1:270])
?nnetar
summary(fit)
autoplot(forecast(fit))
?forecast
shapiro.test(redes$new_confirmed)
?nnetar
##################################################
?ts


?forecast.nnetar

