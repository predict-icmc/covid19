library(sf)
library(dplyr)
library(readxl)
library(csv)
library(tidyverse)
library(ggplot2)
dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)

# Para rodar o código é preciso usar o comando 
#$ setwd("caminho-do-repositorio/covid19/dados/unificado/")
# Assim, os relative paths vão funcionar e o código será executado sem erros

#-----------------------Unificando localizações----------------------------------------------------------------------------------------------
latitude_longitude_estados <- read.csv(file = "../latitude-longitude/latitude-longitude-estados.csv", header=TRUE, sep = ";", encoding = "UTF-8")
latitude_longitude_cidades <- read.csv("../latitude-longitude/latitude-longitude-cidades.csv", header=TRUE, sep = ";", encoding = "UTF-8")

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
localizacao_br <- localizacao_br[,-1]

#--------------Unificando as estimativas popculacionais-------------------------------------------------------------------------------------------

#Unindo todas as planilhas em uma unica tabela
popc <- excel_sheets("../dadosIBGE/dadosIBGE.xlsx") %>%
  map_df(~read_xlsx("../dadosIBGE/dadosIBGE.xlsx",.))

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

pop <- rename(pop, uf = ESTADO, municipio = MUNICIPIO)

#-------------Selecionando vetor de IDH e outros fatores--------------------------------------
# banco de dados retirado do Kaggle para cidades, dados 2010 (desatualizado)

explicacao <- read.csv(file = "../kaggle/desc.csv", header=TRUE, sep = ",", encoding = "UTF-8")
atlasc <- read.csv(file = "../kaggle/atlas.csv", header=TRUE, sep = ",", encoding = "UTF-8")

# filtrando apenas para o ano mais recente

atlasc <- atlasc %>% filter(ano == 2010) %>% 
  select(município , uf, idhm, espvida, gini, t_agua, rdpc)

atlasc <- atlasc %>%
  cbind(tipo = "cidade")

atlasc<- atlasc %>% rename(IDH = idhm)

# obs: reforçando que os dados são de 2010, foram criadas novas cidades, o indice de agua
# potavel melhorou, dentre outras "desatualizações" que possivel

# banco de dados retirado do wikipedia para estados, dados 2017 (feito em uma associação de IBGE e PNUD)

uf <- c(11:17, 21:29,31:33, 35, 41:43, 50:53)

atlase <- data.frame(uf, IDH = c(0.725, 0.719, 0.733, 0.752, 0.698, 0.740, 0.743, 0.687, 0.697, 0.735, 0.731, 0.722, 0.727,
                                 0.683, 0.702, 0.714, 0.787, 0.772, 0.796, 0.826, 0.792, 0.808, 0.787, 0.766, 0.774, 0.769,
                                 0.850))

atlase <- atlase %>%
  cbind(tipo = "estado")

atlas_br <- full_join(atlasc, atlase, by= c("uf", "tipo", "IDH")) 

# adicionando vetor com abreviaçoes dos estados para juntar ao bd final

atlas_br <- atlas_br %>% mutate(estado = case_when(uf == 11 ~ "RO", 
                                                   uf == 12 ~ "AC",
                                                   uf == 13 ~ "AM",
                                                   uf == 14 ~ "RR",
                                                   uf == 15 ~ "PA",
                                                   uf == 16 ~ "AP",
                                                   uf == 17 ~ "TO",
                                                   uf == 21 ~ "MA",
                                                   uf == 22 ~ "PI",
                                                   uf == 23 ~ "CE",
                                                   uf == 24 ~ "RN",
                                                   uf == 25 ~ "PB",
                                                   uf == 26 ~ "PE",
                                                   uf == 27 ~ "AL",
                                                   uf == 28 ~ "SE",
                                                   uf == 29 ~ "BA",
                                                   uf == 31 ~ "MG",
                                                   uf == 32 ~ "ES",
                                                   uf == 33 ~ "RJ",
                                                   uf == 35 ~ "SP",
                                                   uf == 41 ~ "PR",
                                                   uf == 42 ~ "SC",
                                                   uf == 43 ~ "RS",
                                                   uf == 50 ~ "MS",
                                                   uf == 51 ~ "MT",
                                                   uf == 52 ~ "GO",
                                                   uf == 53 ~ "DF",
                                                   TRUE ~ as.character(uf)))

atlas_br <- atlas_br %>% rename(uf = estado, cod_uf = uf, municipio = município)

atlas_br$municipio <- toupper(atlas_br$municipio)


#----------------calculando a densidade populacional-----------------------------------------------------------------
# calculando a densidade populacional de estados e cidades baseada na estimativa de 2019

# recebendo banco de dados com as extensoes dos municipios

extmun <- read_excel("../dadosIBGE/extmun.xls")

extmun <- extmun %>%
  cbind(tipo = "cidade")

# filtrando o que será usado

extmun <- extmun %>% rename(ESTADO = NM_UF_SIGLA, MUNICIPIO = NM_MUN_2019, uf = CD_GCUF) %>%
  select(ESTADO, MUNICIPIO, uf, AR_MUN_2019)

extmun <- extmun[-c(5573:5575), ]

# transformando os nomes dos municipios no "popc" em letras maiusculas para mesclar
# com o banco de dados extmun

popc$MUNICIPIO <- toupper(popc$MUNICIPIO)

densidadec <- full_join(extmun, popc, by = c("MUNICIPIO", "ESTADO", "tipo"))

densidadec <- densidadec[-5573,]

densidadec$densidade <- (densidadec$ESTIMATIVA2019)/(densidadec$AR_MUN_2019)

# Fazendo o mesmo, agora para estados
exte <- read_excel("../dadosIBGE/extmun.xls", sheet = 6)
exte <- exte %>% rename(ESTADO = NM_UF_SIGLA, uf = CD_GCUF)
exte <- exte %>%
  cbind(tipo = "estado")

exte <- exte[,-c(1,3)]

densidadee <- full_join(exte, pope, by = c( "ESTADO", "tipo"))

densidadee <- densidadee[-c(28:30),]

densidadee$densidade <- (densidadee$ESTIMATIVA2019)/(densidadee$AR_MUN_2019)

densidade_pop <- rbind(densidadec,densidadee)

densidade_pop <- densidade_pop %>% rename(uf = ESTADO, cod_uf = uf)
#-------------------------juntando os bd-------------------------------------------------
 
# Por fim, juntando todos os datasets em um unico dataset 

total.1 <- full_join(pop, localizacao_br, by = c("uf", "municipio", "tipo"))

total.1$municipio <- toupper(total.1$municipio)

total.2 <- full_join(total.1, atlas_br, by = c("uf", "municipio", "tipo"))

total.2 <- total.2 %>% rename(MUNICIPIO = municipio)
total.2$cod_uf <- as.character(total.2$cod_uf)

total <-  full_join(total.2, densidade_pop, by = c("uf", "MUNICIPIO", "tipo", "ESTIMATIVA2019",
                                                   "cod_uf"))
#-------------------------------gerando bd final----------------------------


write.csv(total,"./\\Dados_demograficos.csv", row.names = F)





