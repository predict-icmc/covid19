library(sf)
library(dplyr)
library(readxl)
library(csv)
library(tidyverse)
library(ggplot2)
dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)


#--------------------------------------------------------------------------
#Unificando localizações

latitude_longitude_estados <- read.csv(file = "C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/latitude-longitude-estados.csv", header=TRUE, sep = ";", encoding = "UTF-8")
latitude_longitude_cidades <- read.csv("C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/latitude-longitude-cidades.csv", header=TRUE, sep = ";", encoding = "UTF-8")

#Inserindo "tipo"  para identificação dos mesmos no banco de dados unificado

latitude_longitude_estados <- latitude_longitude_estados %>%
  cbind(tipo = "estado")
latitude_longitude_cidades <- latitude_longitude_cidades %>%
  cbind(tipo = "cidade")

# inserindo colunas vazias  para união dos datasets

latitude_longitude_estados$municipio <- NA
latitude_longitude_estados$id_municipio <- NA

#unindo os bancos de dados de localização

localizacao_br <- rbind(latitude_longitude_cidades, latitude_longitude_estados)



#--------------------------------------------------------------------------
#Unificando as estimativas popculacionais (2019)
#Unindo todas as planilhas em uma unica tabela
popc <- excel_sheets("C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/dadosIBGE.xlsx") %>%
  map_df(~read_xlsx("C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/dadosIBGE.xlsx",.))

#filtrando as linhas e colunas de interesse

popc <- popc[-c(1:27),]

popc <- popc[, -c(2:8)]

popc <- popc[, -c(4:7)]

# incluindo tipo "cidade"

popc <- popc %>% 
  cbind(tipo = "cidade")

# criando um dataset com a estimativa populacional por estado pela soma dos municipios
# do bd anterior


pope <- tapply(popc$ESTIMATIVA2019, popc$ESTADO, FUN=sum)

pope <- pope %>% as.data.frame()

pope <- pope %>% 
  cbind(tipo = "estado")

pope <- rename(pope, ESTIMATIVA2019 = .)

aaa <- row.names(pope)

pope <- mutate(pope, ESTADO = aaa)
pope <- pope[, c(3,1,2)]
pope$MUNICIPIO <- NA

pop <- rbind(popc, pope)

#--------------------------------------------------------------------------
# Selecionando vetor de IDH e outros fatores
# banco de dados retirado do Kaggle para cidades, dados 2010 (desatualizado)

explicacao <- read.csv(file = "C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/desc.csv", header=TRUE, sep = ",", encoding = "UTF-8")
atlasc <- read.csv(file = "C:/Users/Yuri Reis Valete/Desktop/Projetos/PREDICT/dadosdemograficos/atlas.csv", header=TRUE, sep = ",", encoding = "UTF-8")

# filtrando apenas para o ano mais recente

atlasc <- atlasc %>% filter(ano == 2010) %>% 
  select(município , uf, idhm, espvida, gini, t_agua, rdpc)

# obs: reforçando que os dados são de 2010, foram criadas novas cidades, o indice de agua
# potavel melhorou, dentre outras "desatualizações" que possivel

# banco de dados retirado do wikipedia para estados, dados 2017 (feito em uma associação de IBGE e PNUD)

uf <- c(11:17, 21:29,31:33, 35, 41:43, 50:53)

atlase <- data.frame(uf, IDH = c(0,725, 0,719, ))


#--------------------------------------------------------------------------
# juntando a pora toda