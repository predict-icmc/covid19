library(data.table)
library(tidyverse)

casos_regiao <- dados %>% mutate(regiao =
                          ifelse(state %in% c("AM", "TO", "PA", "RO", "RR", "AP", "AC"), "NORTE",
                          ifelse(state %in% c("MA","PI","CE","RN","PB","PE","AL","SE","BA"), "NORDESTE",
                          ifelse(state %in% c("PR", "RS", "SC"), "SUL",
                          ifelse(state %in% c("MG", "RJ", "ES", "SP"), "SUDESTE",
                          ifelse(state %in% c("DF", "MT", "MS", "GO"),"CENTRO-OESTE", "NA")))))) %>% 
  arrange(desc(date)) %>% 
  group_by(regiao, date) %>% summarise(new_confirmed = sum(new_confirmed),
                                       new_deaths = sum(new_deaths)) %>%
  mutate(mm7d_casos = frollmean(new_confirmed, 7),
         mm7d_obitos = frollmean(new_deaths, 7))


casos_br <- casos_regiao %>% 
  group_by(date) %>% summarise(new_confirmed = sum(new_confirmed),
                               new_deaths = sum(new_deaths)) %>%
  mutate(mm7d_casos = frollmean(new_confirmed, 7),
         mm7d_obitos = frollmean(new_deaths, 7),
         regiao = "BRASIL")

bind_rows(casos_br,casos_regiao)