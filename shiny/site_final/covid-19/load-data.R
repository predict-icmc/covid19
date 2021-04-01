library(shiny)
library(leaflet)
library(RColorBrewer)
#trocar pelas funcoes
#library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(memoise)
library(feather)
library(plotly)
library(data.table)
#library(minpack.lm)
library(forecast)
#remotes::install_github("predict-icmc/gather-data")
library(gather.covid)
library(shinybusy)
library(brazilmaps)
library(viridis)
library(sf)
library(lubridate)


# arquivo feather a ser lido. Consulte merge-data.R para saber como gerar novos arquivos
latlong <- "latlong-covid.feather"
caso_full <- "full-covid.feather"
obcartorio <- "ob-cartorio.feather"

seade <- "seade-covid.feather"

# Variaveis a serem exibidas
vars <- c(
  "Variação da média móvel de casos" = "var_mm_confirmed",
  "Variação da média móvel de óbitos" = "var_mm_deaths",
  "Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants",
  "Total de Casos Confirmados" = "last_available_confirmed",
  "Total de Óbitos" = "last_available_deaths",
  "Letalidade" = "last_available_death_rate",
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths",
  "Populacão Estimada 2019" = "estimated_population_2019"
)

vars_plot <- c(
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths",
  "Total de Casos Confirmados" = "last_available_confirmed",
  "Total de Óbitos" = "last_available_deaths",
  "Letalidade" = "last_available_death_rate",
  "Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants"
)


vars_plot_mm <- c(
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths"
)

vars_plot_pred <- c(
  "Casos Confirmados" = "last_available_confirmed",
  "Óbitos" = "last_available_deaths"
)

# leitura dos dados.
dados <- read_feather(latlong)

drs_seade <- read_feather(seade)

cleantable <- dados %>% select(state,city,estimated_population_2019,last_available_confirmed, last_available_deaths, last_available_death_rate,latitude,longitude,city_ibge_code)

dt <- read_feather(caso_full)# %>% filter(estimated_population_2019 > 200000)
estados <- dados$state %>% unique %>% as.character() 
cart <- read_feather(obcartorio)



