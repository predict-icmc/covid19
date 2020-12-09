library(feather)
library(tidyverse)
library(plotly)
library(minpack.lm)
library(data.table)

# leitura dos dados na pasta
caso_full <- "full-covid.feather"
dt<-read_feather(caso_full)

write.csv(dt,"caso_full.csv")
selectedCity <- dt %>% filter(state == "SP" &
                                city == "São Paulo" &
                                place_type == "city")
# média móvel

a <- frollmean(selectedCity$new_deaths, 7) 
variacao <- (a[length(a)] - a[length(a)-1])/a[length(a)-1]
scales::percent(variacao)

p <- selectedCity %>% ggplot(aes(x = date, y= new_deaths)) + geom_bar(stat="identity") + geom_line(aes(y = a))
ggplotly(p)

# casos

b <- frollmean(selectedCity$new_confirmed, 7) 

#salvando o que a prof pediu (descomentar)
#write.csv(selectedCity, file = "sp.csv")

# autoarima
library(forecast)

fit <- forecast::auto.arima(selectedCity$new_confirmed)

selectedCity$new_confirmed %>% 
Arima(order=c(3,0,1), seasonal=c(0,1,2), lambda=0) %>%
  forecast() %>%
  autoplot() +
  ylab("H02 sales (million scripts)") + xlab("Year")

autoplot(forecast(fit,20))



# modelo antigo: Gompertz
selectedCity %>% ggplot(aes(x = date, y= new_confirmed)) + geom_bar(stat="identity") + geom_line(aes(y = b))

selectedCity %>% plot_ly() %>% add_bars(x = ~date, y = ~new_confirmed) %>% add_lines(x = ~date, y = ~b) %>% config(displayModeBar = F) %>% hide_legend()

# ajuste do modelo

fit.Gompertz.cases <- nlsLM(last_available_confirmed ~ SSgompertz(tempo, Asym, b2, b3),
                        #start = c(Asym=1,b2=0,b3=0),
                        data = selectedCity)
  XX = (0:(max(selectedCity$tempo)+10))
  Asym.G<-coef(fit.Gompertz.cases)[1]
  b2.G<-coef(fit.Gompertz.cases)[2]
  b3.G<-coef(fit.Gompertz.cases)[3]
  
  yp.G<-0
 
  yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)

  
  predict.G<-data.frame(x=XX,y=yp.G)

  predict.filtra <- predict.G %>% filter(x > max(selectedCity$tempo-1))
  
  selectedCity.filtra <- selectedCity %>% filter(tempo > max(selectedCity$tempo)-30)
  
  
  
    # gera o grafico
 p<-  ggplot(selectedCity.filtra) + 
    geom_line(aes(x = tempo, y = last_available_confirmed), size = 1, color = "blue") +
     geom_point(aes(x=x, y = y, color = "red"),
              data = predict.filtra) +
      labs(title = 'Previsão de casos confirmados de COVID-19 nos próximos 10  dias', #subtitle=paste(input$cities,"-",input$state), x = 'Dias', 
         y = 'Total de casos confirmados', fill = '') +
    theme_bw()
 ggplotly()
 
 
 
 selectedCity <- selectedCity %>% filter(tempo > max(selectedCity$tempo)-60)
 # ajuste do modelo
 
 fit.Gompertz.cases <- nlsLM(last_available_deaths ~ SSgompertz(tempo, Asym, b2, b3),
                             #start = c(Asym=2*10^3,b2=1,b3=0.1),
                             data = selectedCity)
 XX = (0:(max(selectedCity$tempo)+10))
 Asym.G<-coef(fit.Gompertz.cases)[1]
 b2.G<-coef(fit.Gompertz.cases)[2]
 b3.G<-coef(fit.Gompertz.cases)[3]
 
 yp.G<-0
 
 yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)
 
 inter <- confint(fit.Gompertz.cases)
 
 yp.G.min<-inter[1,1]*exp((-b2.G)*(b3.G)^XX)
 yp.G.max<-inter[1,2]*exp((-b2.G)*(b3.G)^XX)
 
 predict.G<-data.frame(x=XX,y=yp.G, min = yp.G.min, max = yp.G.max)
 
 predict.filtra <- predict.G %>% filter(x > max(selectedCity$tempo))
 
 selectedCity.filtra <- selectedCity %>% filter(tempo > max(selectedCity$tempo)-30)
 # gera o grafico
 p<-  ggplot() + 
   geom_line(aes(x = tempo, y = last_available_deaths), size = 1, color = "blue", data = selectedCity.filtra) +
   geom_point(aes(x=x, y = y, color = "red"),
              data = predict.filtra) +
   geom_errorbar(aes(x = x, ymin=min, ymax=max), colour="black", data = predict.filtra) +
   labs(title = 'Previsão de casos confirmados de COVID-19 nos próximos 10  dias', #subtitle=paste(input$cities,"-",input$state), x = 'Dias', 
        y = 'Total de casos confirmados', fill = '') +
   theme_bw()
 ggplotly()
 
 