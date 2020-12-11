
library(feather)
library(tidyverse)
library()
library(plotly)
library(minpack.lm)
library(data.table)

?rmse

setwd("~/Documentos/USP/predict/covid19/shiny/site_final/covid-19")

# leitura dos dados na pasta
caso_full <- "full-covid.feather"
dt<-read_feather(caso_full)


ts_sampa <-df %>%
  filter(state=="SP" & place_type=="state") %>% 
  select(new_confirmed,date)

#write.csv(dt,"caso_full.csv")
selectedCity <- dt %>% filter(state == "SP" &
                                
                                place_type == "state")
View(selectedCity)
# média móvel
a <- frollmean(selectedCity$new_deaths, 7) 
print(a)
variacao <- (a[length(a)] - a[length(a)-1])/a[length(a)-1]
scales::percent(variacao)

p <- selectedCity %>% ggplot(aes(x = date, y= new_deaths)) + geom_bar(stat="identity") + geom_line(aes(y = a))
ggplotly(p)

# casos

b <- frollmean(selectedCity$new_confirmed, 7) 

#salvando o que a prof pediu (descomentar)
#write.csv(selectedCity, file = "sp.csv")

#---- testes Fla
library(forecast)

#--- transformando em série tmp
#tsCasosConf =  ts(casosConfirmados,
#               start = c(2020,5),
#               frequency = 52) # freq semanal

modelo = auto.arima(selectedCity$new_confirmed,
                    trace = T, # habilitando o display para acompanhar
                    stepwise = F, # permitindo uma busca mais profunda
                    approximation = F)
print(modelo) # exibindo os parametros do modelo

## avaliando os resíduos
checkresiduals(modelo) # o resultado do teste de hipotese, 
# p-value = 0.004438, aponta que há correlação entre os resíduos
# assim, esses residuais não podem ser considerados ruídos branco. 
# Com o diagrama de autocorrelação observa-se que várias legs passaram do limiar.
# com o histograma, observa-se na linha o acumulado da distribuição
# que os resíduos não estão estão distribuídos normalmente.
# fazendo-se o teste de normalidade, com o shapiro-teste
shapiro.test(modelo$residuals)# saida =====> p-value = 1.24e-12 << 0.05, 
# dessa forma, o entendimento é que de fato os dados não estão normalmente disribuídos
var(modelo$residuals) # variancia alta  
mean(modelo$residuals)# 
# com isso pode-se concluir que não foi criado um bom modelo de previsão 

previsao = forecast(modelo, h = 12) 
print(previsao)
autoplot(previsao)


treino<-redes$new_confirmed[1:276]
test<-redes$new_confirmed[277:290]
