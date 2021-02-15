# Rodar 'inicialização.R' em nosso github antes desse código

# pacotes necessários para stepwise
library(tidyverse)
library(caret)
library(leaps)
library(MASS)

## Como o código demorou cerca de 30minutos para rodar "populacionalmente", realizarei uma
## amostragem baseada nos estados ou em outros fatores

# Selecionando a amostra por UF
# selecionando o banco de dados juntamente com a coluna "uf" para realização de amostragem

swall <- last.data %>% 
  select(uf,IDH,gini,t_agua,rdpc,densidade,last_available_confirmed) %>% 
  na.omit()

# inserindo uma semente para reprodutibilidade do código
set.seed(2020)

# realizando a amostragem de tamanho 10 para cada estado

## removendo DF 

swall <- swall[swall$uf != "DF",]

# realizando a amostragem de tamanho 10 para cada estado
swall_am <- do.call(rbind,lapply(split(swall, swall$uf), function(i)
          i[sample(1:nrow(i), size = 20, replace = T),]))  

swall_am <- swall_uf %>% na.omit() 


# renomeando last_available_confirmed para Y

swall_am <- swall_am %>% 
  rename(
    Y = last_available_confirmed
  )

## Stepwise - metodo foward
a <- step(lm(Y~uf+IDH+gini+rdpc+densidade,data=swall_am),direction="forward")
a

## Stepwise - metodo backward (do modelo geral para o "melhor")
b <- step(lm(Y~.,data=swall_am),direction="backward")
b

## Stepwise - metodo backward
c <- step(lm(Y~.,data=swall_am),direction="both")
c

lm(Y~IDH+t_agua)
