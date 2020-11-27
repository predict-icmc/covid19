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

## removendo DF para adciona-lo posteriormente

swall <- swall[swall$uf != "DF",]

swall_uf <- do.call(rbind,lapply(split(swall, swall$uf), function(i)
          i[sample(1:nrow(i), size = 10, replace = T),]))  

swall_uf <- swall_uf %>% na.omit() 

swall <- swall_uf %>% 
  select(uf,IDH,gini,t_agua,rdpc,densidade,last_available_confirmed) %>% 
  na.omit()
# renomeando last_available_confirmed para Y

swall <- swall %>% 
  rename(
    Y = last_available_confirmed
  )

do.call(rbind,
        lapply(split(df, df$type1), function(i)
          i[sample(1:nrow(i), size = 10, replace = TRUE),]))

a <- step(lm(Y~.,data=swall),direction="forward")
a

a <- step(lm(Y~.,data=swall),direction="backward")
a

a <- stepAIC(lm(Y~.,data=swall),direction="both")
a