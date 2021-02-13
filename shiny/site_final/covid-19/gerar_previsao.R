library(feather)
library(tidyverse)
library(plotly)
library(minpack.lm)
library(data.table)
library(forecast)
library(lubridate)

# leitura dos dados na pasta
caso_full <- "full-covid.feather"
dt<-read_feather(caso_full)

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


 
