library(tidyverse)
library(plotly)
library(forecast)

# Lendo a base de dados do SEADE com os casos em SP
casos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", sep= ";")
leitos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova_variacao_semanal.csv", sep = ";")


leitos_sp <- leitos %>% filter (nome_drs == "DRS 01 Grande São Paulo")

casos_drs = casos %>% filter(nome_drs == "Grande São Paulo") %>%
    group_by(datahora) %>% summarise(total_novos_casos = sum(casos_novos)) 
  
fit <- nnetar(casos_novos, data = casos_drs)

  casos_drs %>% plot_ly(x = ~datahora) %>% add_bars( y = ~total_novos_casos) %>% 
    #add_lines(y = ~data.table::frollmean(total_novos_casos, 7)) %>% 
    add_lines(y = ~ocupacao_leitos, data = leitos_sp)
