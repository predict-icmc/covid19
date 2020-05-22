### Descrição: Codigo para o levantamento e organização da base de dados de IDM dos municípios BR
### Usando a base simplificada do PNUD (Programa das Nações Unidas para o Desenvolvimento), que
### contendo informações socioeconômicas de todos os municípios do país. 
### Os resultados foram obtidos a partir dos Censos de 1991, 2000 e 2010.
###
### Fonte: http://material.curso-r.com/manip/
### Data: 22/05/2020



#### Executar se necessário
#install.packages("dplyr")
#install.packages("dplyr")
#install.packages("tibble")
#install.packages("devtools)"

library(dplyr)
library(tidyr)
library(tibble)


devtools::install_github("abjur/abjData")  # importando a base PNUD
library(abjData)                           # carregando a base PNUD

### criando o DF com os IDHs de 2010
### idhm_e - IDH municipal - educação.
### idhm_l - IDH municipal - longevidade.
### idhm_r - IDH municipal - renda.


tabMun = pnud_muni %>%
  select(ano,municipio, uf, codmun7, starts_with('idhm')) %>%
  filter(ano=="2010")
dfMun = as.data.frame(tabMun)
view(dfMun)

