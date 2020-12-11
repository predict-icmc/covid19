
library(feather)
library(tidyverse)
library(plotly)
library(minpack.lm)
library(data.table)

setwd("~/Documentos/USP/predict/covid19/shiny/site_final/covid-19")

# leitura dos dados na pasta
caso_full <- "full-covid.feather"
dt<-read_feather(caso_full)


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

#--- Conjuntos Treino e Teste
treino<-selectedCity$new_confirmed[1:276]
test<-selectedCity$new_confirmed[277:290]

modelo = auto.arima(treino,
                    trace = T, # habilitando o display para acompanhar
                    stepwise = F, # permitindo uma busca mais profunda
                    approximation = F)
print(modelo) # exibindo os parametros do modelo

## avaliando os resíduos
checkresiduals(modelo) 
shapiro.test(modelo$residuals)
var(modelo$residuals) # variancia alta  
mean(modelo$residuals)# 
# com isso pode-se concluir que não foi criado um bom modelo de previsão 

previsao = forecast(modelo, h = 14) #duas semanas
print(previsao)
autoplot(previsao)

#- comparar a previsao com o conj teste

