library(dplyr)
library(magrittr)
library(data.table)
library(lubridate)
library(tidyr)

read_seade <- function(){
  # Lendo a base de dados do SEADE com os casos em SP
  casos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", sep= ";")
  leitos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova_variacao_semanal.csv", sep = ";")
  
  # incluindo os codigos das DRS na base de leitos
  leitos <- leitos %>% mutate(cod_drs = extract_numeric(nome_drs))
  
  
  #ajustando manualmente os codigos das DRS
  leitos$cod_drs[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13 ,14, 15, 16, 17)] <- c(10, 7, 8, 13, 16, 12, 3, 4, 5, 11, 2, 9, 1, 14, 15, 6, 17)
  
  casos_drs <- casos %>% arrange(desc(datahora)) %>% # filter(nome_drs == "Grande São Paulo") %>%
      group_by(cod_drs, datahora) %>% summarise(total_novos_casos = sum(casos_novos),
                                                total_novos_obitos = sum(obitos_novos)) %>%
    mutate(mm7d_casos = frollmean(total_novos_casos, 7),
           mm7d_obitos = frollmean(total_novos_obitos, 7)) %>% 
    left_join(leitos, by=c("datahora", "cod_drs"))
    
  casos_drs$datahora <- casos_drs$datahora %>% lubridate::as_date()
  
  #trocando as virgulas por pontos
  as.numeric(gsub(",", ".", gsub("\\.", "", leitos$ocupacao_leitos)))
  
  
  return (casos_drs)  
}

a <- read_seade()

library(forecast)
library(plotly)

sp <- a %>% filter(cod_drs == 10)

sp$ocupacao_leitos <- as.numeric(gsub(",", ".", gsub("\\.", "", sp$ocupacao_leitos)))

fit <- nnetar(sp$total_novos_casos,p=7)

pred <- predict(fit,21, PI = T, level = c(0.95, 0.80))


fit2 <- nnetar(sp$ocupacao_leitos,p=7)

pred2 <- predict(fit2,21, PI = T, level = 0.80)


trace1 <- list(
  line = list(
    color = "rgba(0,0,0,1)", 
    fillcolor = "rgba(0,0,0,1)"
  ), 
  mode = "lines", 
  name = "Média móvel (7 dias)", 
  type = "scatter", 
  x =  sp$datahora,
  y =  round(sp$mm7d_casos), 
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
  x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev( min( sp$datahora)  - ddays(1) + ddays( time( pred$mean)))),
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
  x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev( min(sp$datahora)  - ddays(1) + ddays( time( pred$mean)))),
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
  x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
  y = round( pred$mean), 
  xaxis = "x", 
  yaxis = "y"
)
trace5 <- list(
  line = list(
    color = "rgba(0,155,155,1)", 
    fillcolor = "rgba(0,155,155,1)"
  ), 
  mode = "lines", 
  name = "% leitos ocupados", 
  type = "scatter", 
  x = sp$datahora,#min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
  y = sp$ocupacao_leitos %>% as.numeric(), 
  xaxis = "x", 
  yaxis = "y2"
)
trace6 <- list(
  fill = "toself", 
  line = list(
    color = "rgba(004,204,204,1)", 
    fillcolor = "rgba(004,204,204,1)"
  ), 
  mode = "lines", 
  name = "80% confidence", 
  type = "scatter", 
  x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev( min(sp$datahora)  - ddays(1) + ddays( time( pred$mean)))),
  y = round( c( pred2$upper[,1], rev(pred2$lower[,1]))),
  xaxis = "x", 
  yaxis = "y2", 
  hoveron = "points"
)
trace7 <- list(
  line = list(
    color = "rgba(0,0,055,1)", 
    fillcolor = "rgba(0,0,055,1)"
  ), 
  mode = "lines", 
  name = "previsão de internações", 
  type = "scatter", 
  x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
  y = round( pred2$mean), 
  xaxis = "x", 
  yaxis = "y2"
)
data <- list(trace1, trace2, trace3, trace4, trace5)
layout <- list(
  title = "Forecast from NNAR", 
  xaxis = list(
    title = "Date", 
    domain = range(sp$datahora)
  ), 
  yaxis = list(
    title = "Novos Casos Confirmados (Média Móvel de 7 dias)", 
    domain = c(0, 1)
  ),
  yaxis2 = list(
    title = "Leitos ocupados", 
    domain = c(150, 800),
    overlaying = "y",
    side = "right"
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
p <- add_bars(p, y = sp$total_novos_casos, x = sp$datahora, name = "Novos casos diários")
p <- add_trace(p, line=trace5$line, mode=trace5$mode, name=trace5$name, type=trace5$type, x=trace5$x, y=trace5$y, xaxis=trace5$xaxis, yaxis=trace5$yaxis)
p <- add_trace(p, fill=trace6$fill, line=trace6$line, mode=trace6$mode, name=trace6$name, type=trace6$type, x=trace6$x, y=trace6$y, xaxis=trace6$xaxis, yaxis=trace6$yaxis, hoveron=trace6$hoveron)
p <- add_trace(p, line=trace7$line, mode=trace7$mode, name=trace7$name, type=trace7$type, x=trace7$x, y=trace7$y, xaxis=trace7$xaxis, yaxis=trace7$yaxis)
p <- add_segments(p, yaxis = "y2", name = "100% de ocupação", x = as_date("2020-12-31"), xend = as_date("2021-03-15"), y = 100, yend = 100)
p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, yaxis2=layout$yaxis2, margin=layout$margin)

p 

