# pacotes 

library(ggplot2)
library(corrplot)
library(tidyverse)
library(mvShapiroTest)

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

#----Estudando as correlações----

## comparação superficial de:

# teoria: queremos testar a correlação entre duas variaveis, sendo elas numéricas;
# para realizar o coeficiente de correlação de pearson temos que testar a normalidade
# da distribuiçao conjunta, dessa forma:

## para o caso de IDH vs Numeros de casos por 100 mil hab.

# a função do teste multivariado se limita a uma amostra de 5000 obs. e a ser uma matriz

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$IDH), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ IDH vs Casos não tem normalidade mult.

# IDH vs Mortes
x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_deaths), 5000), 
                     sample(na.omit(bdcorrc$IDH), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ IDH vs mortes não tem normalidade mult.

# Gini vs Casos por 100k

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$gini), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ gini vs casos por 100k não tem normalidade mult.

# Gini vs mortes

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_deaths), 5000), 
                     sample(na.omit(bdcorrc$gini), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ gini vs mortes não tem normalidade mult.

# espectativa de vida vs casos

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$espvida), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ espectativa de vidas vs casos por 100k não tem normalidade mult.

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_deaths), 5000), 
                     sample(na.omit(bdcorrc$espvida), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ expectativa de vidas vs casos por 100k não tem normalidade mult.

# tratamento de água vs casos
x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$t_agua), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ tratamento de agua vs casos por 100k não tem normalidade mult.

# tratamento de água vs mortes

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_deaths), 5000), 
                     sample(na.omit(bdcorrc$t_agua), 5000)), ncol = 2) 

mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ tratamento de agua vs mortes não tem normalidade mult.

# renda *per capita* vs casos por 100 k

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$rdpc), 5000)), ncol = 2) 


mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ renda per capita vs casos por 100k não tem normalidade mult.

# renda *per capita* vs mortes

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_deaths), 5000), 
                     sample(na.omit(bdcorrc$rdpc), 5000)), ncol = 2) 

mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ renda per capita vs mortes não tem normalidade mult.

# densidade vs casos por 100k

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$densidade), 5000)), ncol = 2) 

mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ densidade vs casos por 100k não tem normalidade mult.

# densidade vs mortes

x <- matrix(data = c(sample(na.omit(bdcorrc$last_available_confirmed_per_100k_inhabitants), 5000), 
                     sample(na.omit(bdcorrc$densidade), 5000)), ncol = 2) 

mvShapiro.Test(x)

# p-value < 2.2e-16 (rejeito H0)/ densidade vs mortes não tem normalidade mult.

# conclusão: como nenhum dos casos tem normalidade multivariada(2 a 2), fazemos o 
# cálculo da correlação por meio do cálculo do coeficiente de Spearman

# para todos os casos

