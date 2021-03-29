library(sf)
library(dplyr)
library(tmap)  
library(brazilmaps)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")

#dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)

dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))

# filtrando os dados, selecionando apenas os estados

Estados <- filter(dados, place_type == 'state')

# adicionando a coluna de mortes diária, dado que as mortes estavam acumuladas

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(tempo) %>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))

#---------------------------------------------------------------------------------

#separando em regiões

Sudeste <- filter(Estados, state == 'SP'|state == 'MG'|state == 'RJ'| state == 'ES')

ggplot(Sudeste, aes(x = tempo, y = obitos_dia, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias', subtitle = "Sudeste",
       x = 'Dias', y = 'Número de óbitos', fill = 'estados') +
  scale_x_discrete(limits=c(7,14,21,28,35,42,49,56,63)) +
  theme_bw()

Norte<-filter(Estados,state=="AC"|state=="AP"| state=="AM" | state=="PA"| state=="RO"| state=="RR"| state=="TO")

ggplot(Norte, aes(x = tempo, y = obitos_dia, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias', subtitle = "Norte", 
       x = 'Dias', y = 'Número de óbitos') +
  scale_x_discrete(limits=c(7,14,21,28,35,42,49,56,63)) +
  theme_bw()

Sul<-filter(Estados,state=="PR"|state=="RS"| state=="SC")

ggplot(Sul, aes(x = tempo, y = obitos_dia, group = state)) + 
  geom_line(aes(col = state), size = 1, alpha = 0.8) +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias', subtitle = "Sul",
       x = 'Dias', y = 'Número de óbitos') +
  scale_x_discrete(limits=c(7,14,21,28,35,42,49,56,63)) +
  theme_bw()

Centroeste<-filter(Estados,state=="DF"|state=="GO"| state=="MT"|state=="MS")

ggplot(Centroeste, aes(x = tempo, y = obitos_dia, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias',subtitle = "Centro Oeste", 
       x = 'Dias', y = 'Número de óbitos', fill = 'estados') +
  scale_x_discrete(limits=c(7,14,21,28,35,42,49,56,63)) +
  theme_bw()

Nordeste<-filter(Estados,state=="AL"|state=="BA"| state=="CE" | state=="MA"| state=="PB"| state=="PI"| state=="PE"|state=="RN"| state=="SE")

ggplot(Nordeste, aes(x = tempo, y = obitos_dia, group = state)) + 
  geom_line(aes(col = state), size = 1) +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias', subtitle = "Nordeste", 
       x = 'Dias', y = 'Número de óbitos') +
  scale_x_discrete(limits=c(7,14,21,28,35,42,49,56,63)) +
  theme_bw()

#---------------------------------------------------------------------------------

# Comparando estado por estado

Estados <- filter(dados, place_type == 'state')

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(tempo) %>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))

ggplot(Estados, aes(x = tempo, y = obitos_dia)) + 
  geom_line (size = 1, color = "blue") +
  labs(title = 'Crescimento dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Número de óbitos', fill = 'estados') +
  scale_x_discrete(limits=c(15,30,45,60)) +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

#---------------------------------------------------------------------------------

#Comparando a porcentagem tomada por estado dentro da região

## Sudeste

ggplot(Sudeste, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(20,max(Sudeste$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Número de óbitos', subtitle = 'Sudeste') +
  theme_bw()

## Nordeste

#trocar a paleta
ggplot(Nordeste, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Nordeste$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Nordeste') +
  theme_bw()

## Sul

ggplot(Sul, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Sul$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Sul') +
  theme_bw()

## Centro oeste

ggplot(Centroeste, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Centroeste$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'porcentagem', subtitle = 'Centro Oeste') +
  theme_bw()

## Norte

ggplot(Norte, aes(x = tempo, y= obitos_dia, group = state)) +
  geom_bar(aes(fill = state), position = 'fill', stat = "identity")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(27,max(Norte$tempo))) +
  labs(title = 'Proporção dos casos de óbito por COVID-19 em dias', x = 'Dias', 
       y = 'Porcentagem', subtitle = 'Norte') +
  theme_bw()

#---------------------------------------------------------------------------------

# Proporção de óbitos por região dentro do Brasil

Regioes <- Estados %>%
  mutate(Região = case_when(
    state %in% c("SP","RJ","MG","ES")~"Sudeste",
    state %in% c("PR","RS","SC")~"Sul",
    state %in% c("AC","AP","AM","PA","RO","RR","TO")~"Norte",
    state %in% c("AL","CE","BA","MA","PB","PI","PE","RN","SE")~"Nordeste",
    state %in% c("DF","GO","MT","MS")~"Centro-Oeste"
  ))

p <-ggplot(Regioes,aes(x = tempo, y = obitos_dia, fill = Região)) 

p+geom_bar(stat = "identity", position = "fill")+
  scale_y_continuous(labels = scales::percent) + 
  xlim(c(20,max(Regioes$tempo))) +
  labs(title = 'Proporção de óbitos por COVID-19 por região', 
       x = 'Dias', y = 'Proporção de óbitos') +
  theme_bw()


#---------------------------------------------------------------------------------
 # mapa estático COVID - 19

 # filtrando os dados para o numero de mortes (acumulado) atualizado

mortes <- Estados %>% 
  filter(is_last == "True") %>%
  select(state,city_ibge_code, deaths)

sum(mortes$deaths)

 # plotando o mapa 

mapa <- get_brmap("State") %>%
  inner_join(mortes, c("State" = "city_ibge_code"))

mapa %>% 
  ggplot() +
  geom_sf(aes(fill = deaths),
          col = "black", size = 0.23)+
  scale_fill_viridis_c(option = 8) +
  theme(panel.grid = element_line(colour = "transparent"),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


