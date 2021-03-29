library(feather)
library(tidyverse)
library(plotly)
library(minpack.lm)
library(data.table)
<<<<<<< HEAD
library(forecast)
library(lubridate)
=======
>>>>>>> b1dbcdbdd162b67ff70d8af49986dda7862d67f2

# leitura dos dados na pasta
caso_full <- "full-covid.feather"
dt<-read_feather(caso_full)

<<<<<<< HEAD
#write.csv(dt,"caso_full.csv")
selectedCity <- dt %>% filter(state == "RJ" &
                                #city == "São Paulo" &
                                place_type == "state")
  
  mm <- frollmean(selectedCity$new_confirmed,7)
  fit <- nnetar(selectedCity$new_confirmed,p=7)

  pred <- predict(fit,14, PI = T, level = c(0.95, 0.80))
 
 
 trace1 <- list(
   line = list(
     color = "rgba(0,0,0,1)", 
     fillcolor = "rgba(0,0,0,1)"
   ), 
   mode = "lines", 
   name = "observed", 
   type = "scatter", 
   x =  selectedCity$date,
   y =  round(frollmean(selectedCity$new_confirmed, 7)), 
   xaxis = "x", 
   yaxis = "y"
 )
 trace2 <- list(
   fill = "toself", 
   line = list(
     color = "rgba(242,242,242,1)", 
     fillcolor = "rgba(242,242,242,1)"
   ), 
   mode = "lines", 
    name = "95% confidence", 
   type = "scatter", 
   x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev( min( selectedCity$date)  - ddays(1) + ddays( time( pred$mean)))),
   y = round( c( pred$upper[,1], rev(pred$lower[,1]))),
   xaxis = "x", 
   yaxis = "y", 
   hoveron = "points"
 )
 trace3 <- list(
   fill = "toself", 
   line = list(
     color = "rgba(204,204,204,1)", 
     fillcolor = "rgba(204,204,204,1)"
   ), 
   mode = "lines", 
   name = "80% confidence", 
   type = "scatter", 
   x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev( min( selectedCity$date)  - ddays(1) + ddays( time( pred$mean)))),
   y = round( c( pred$upper[,2], rev(pred$lower[,2]))),
   xaxis = "x", 
   yaxis = "y", 
   hoveron = "points"
 )
 trace4 <- list(
   line = list(
     color = "rgba(0,0,255,1)", 
     fillcolor = "rgba(0,0,255,1)"
   ), 
   mode = "lines", 
   name = "prediction", 
   type = "scatter", 
   x = min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)),
   y = round( pred$mean), 
   xaxis = "x", 
   yaxis = "y"
 )
 data <- list(trace1, trace2, trace3, trace4)
 layout <- list(
   title = "Forecast from NNAR", 
   xaxis = list(
     title = "Date", 
     domain = range(selectedCity$date)
   ), 
   yaxis = list(
     title = "Novos Casos Confirmados (Média Móvel de 7 dias)", 
     domain = c(0, 1)
   ), 
   margin = list(
     b = 40, 
     l = 60, 
     r = 10, 
     t = 25
   )
 )
 p <- plot_ly()
 p <- add_trace(p, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, xaxis=trace1$xaxis, yaxis=trace1$yaxis)
 p <- add_trace(p, fill=trace2$fill, line=trace2$line, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, xaxis=trace2$xaxis, yaxis=trace2$yaxis, hoveron=trace2$hoveron)
 p <- add_trace(p, fill=trace3$fill, line=trace3$line, mode=trace3$mode, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, xaxis=trace3$xaxis, yaxis=trace3$yaxis, hoveron=trace3$hoveron)
 p <- add_trace(p, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type, x=trace4$x, y=trace4$y, xaxis=trace4$xaxis, yaxis=trace4$yaxis)
 p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin)
 p %>%  add_bars(y = selectedCity$new_confirmed, x = selectedCity$date, name = "Novos casos diários")
p 


 
=======
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
 
 
>>>>>>> b1dbcdbdd162b67ff70d8af49986dda7862d67f2
