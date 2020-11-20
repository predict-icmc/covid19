library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(feather)
library(plotly)
library(data.table)
library(minpack.lm)


# arquivo feather a ser lido. Consulte merge-data.R para saber como gerar novos arquivos
latlong <- "latlong-covid.feather"
caso_full <- "full-covid.feather"
obcartorio <- "ob-cartorio.feather"

# Variaveis a serem exibidas
vars <- c(
  "Total de Casos Confirmados" = "last_available_confirmed",
  "Total de Óbitos" = "last_available_deaths",
  "Letalidade" = "last_available_death_rate",
  "Populacão Estimada 2019" = "estimated_population_2019",
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths"#,
  #"Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants"
)

vars_plot <- c(
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

estados <- dados$state %>% unique %>% as.character()
cleantable <- dados %>% select(state,city,estimated_population_2019,last_available_confirmed, last_available_deaths, last_available_death_rate,latitude,longitude,city_ibge_code)

dt <- read_feather(caso_full)
 
cart <- read_feather(obcartorio)