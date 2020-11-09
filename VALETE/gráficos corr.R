# pacotes 

library(ggplot2)
library(corrplot)
library(tidyverse)

# rodar bancos de dados "caso_full" e "demografico"
# do arquivo "inicialização"

#----Filtrando apenas os dados necessários----

#pegando apenas o último dia

df$date <- as.Date(df$date)
df <- subset(df, date == max(date))

#realizando devidas mudanças para 'encaixe' de comparação

#tirando acentos
df$city <- iconv(df$city, from ="UTF-8",to = "ASCII//TRANSLIT")
df$city <- toupper(df$city)

# renomeando as colunas

df <- df %>% 
  rename(
    uf = state,
    MUNICIPIO = city,
    tipo = place_type
  )

df$tipo <- str_replace_all(df$tipo, "city", "cidade")
df$tipo <- str_replace_all(df$tipo, "state", "estado")
bdcorr <- full_join(df, demografico, by = c('uf','MUNICIPIO','tipo'))

#banco de dados para correlações - cidades
bdcorrc <- filter(bdcorr, tipo == "cidade")

#banco de dados para correlação - estado
bdcorre <- filter(bdcorr, tipo == "estado")

# deixando IDH em crescente

demografico <- demografico[order(demografico$IDH),]

#----Estudando as correlações---

## comparação superficial de:

# teoria: queremos testar a correlação entre duas variaveis, sendo elas numéricas;
# para realizar o coeficiente de correlação de pearson temos que testar a normalidade
# da distribuiçao conjunta, dessa forma:

## para o caso de IDH vs Numeros de casos por 100 mil hab.

bdcorrc$IDH <- as.numeric(bdcorrc$IDH)
a <- data.frame('a' = bdcorrc$last_available_confirmed_per_100k_inhabitants,'b'= bdcorrc$IDH)
mvn(a)
### Numero de casos x IDH

ggplot(bdcorrc, aes(x= IDH,y = last_available_confirmed_per_100k_inhabitants, colour = uf))+
  geom_point()+
  scale_x_discrete(breaks = seq(0,1,0.1))+
  ylab("total de casos confirmados por 100 mil habitantes")

ggplot(bdcorrc, aes(x= IDH,y = last_available_deaths/100000))+
  geom_point()+
  scale_x_discrete(breaks = c(0.4,0.45,0.5,0.55,0.6,0.65,0.7,0.75,0.80,0.85,0.9))+
  ylab("total de mortes confirmados por 100 mil habitantes")
  




