library(tidyverse)
library(nls.multstart)
library(minpack.lm)

dt<-read.csv(file = "2020-08-11_caso_full.csv",header=TRUE)
dt$tempo<- as.numeric(as.Date(dt$date) - min(as.Date(dt$date)))
dt$date <- as.Date(dt$date)

#ajuste ao modelo
SaoPaulo <- dt %>% filter(state ==  "SP" &
                            city == "São Paulo" &
                            place_type == "city")

SaoPaulo <- SaoPaulo[order(SaoPaulo$tempo),]

fit <- nlsLM(last_available_confirmed ~ SSlogis(tempo, Asym, xmid, scal),
             start = c(Asym=150,xmid=1,scal=1),
             data = SaoPaulo)

fit.Gompertz <- nlsLM(last_available_confirmed ~ SSgompertz(tempo, Asym, b2, b3),
                      start = c(Asym=1500,b2=0.9,b3=0.9),
                      data = SaoPaulo)

#summary(fit)
#summary(fit.Gompertz)

XX = (0:(max(SaoPaulo$tempo)+10))

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

previsao %>% ggplot(aes(x = x, y=y,color = model)) + geom_line()
SaoPaulo %>% ggplot() + geom_point(aes(x = date +10, y = last_available_confirmed))
  geom_line(aes(x = XX, y=yp.G))
              
              