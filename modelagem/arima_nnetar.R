library(gather.covid)
library(data.table)
library(xts)
library(forecast)
library(tidyverse)

dados <- pegaCorona(tipo = "caso_full", baixar = F)

sp <- dados %>% filter(city == "São Paulo") 

sp_ts <- xts(x = sp$new_confirmed, order.by = sp$date, frequency = 7)

fit <- auto.arima(sp_ts, seasonal = T)

xxreg <- forecast(fit, 14)

xxreg_nn <- forecast(fit_nnetar, 14)

fit_nnetar <- nnetar(sp_ts, p=7, xreg = fit$fitted)

fit2 <- auto.arima(sp_ts, xreg = fit_nnetar$fitted, seasonal = T)

autoplot(forecast(fit_nnetar, 14, PI = T))
         , 14, xreg = xxreg$mean))

autoplot(forecast(fit, 14))

autoplot(forecast(fit2, 14, xreg = xxreg_nn$mean))



fit_nnetar2 <- nnetar(sp_ts, p=7, xreg = fit$fitted)
forecast(fit_nnetar2, 14, xreg = xxreg$mean) %>% autoplot
