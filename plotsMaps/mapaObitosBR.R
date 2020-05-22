
setwd("/home/louzeiro/Documentos/USP/predict/covid19/plotsMaps/")

install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)

dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))

# filtrando os dados, selecionando apenas os estados

Estados <- filter(dados, place_type == 'state')

# adicionando a coluna de mortes diária, dado que as mortes estavam acumuladas

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(tempo) %>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))
