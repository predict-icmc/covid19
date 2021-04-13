#' @PREDICT-ICMC merge-data.R

#'* rotinas para criação dos arquivos de dados do dashboard*

library(magrittr)
library(tidyr)
library(dplyr)
library(feather)
library(data.table)

# lembre-se de possuir os arquivos latitude-longitude-cidades.csv
# e latitude-longitude-estados.csv na pasta do projeto

# dropbox token
# token <- readRDS(file = "token.rds")

# função que lê diretamente o gzip do servidor
downCorona <- function(file_url) {
  con <- gzcon(url(file_url))
  txt <- readLines(con)
  return(read.csv(textConnection(txt)))
}

# funcao que pega o arquivo de casos do brasil.io, faz um pequeno tratamento e envia para o dropbox os arquivos "latlong-covid.feather" e "full-covid.feather"
# ATENÇÃO: necessário possuir os arquivos 'latitude-longitude-cidades.csv' e 'latitude-longitude-estados.csv' na working directory


pegaCorona <- function(baixar = TRUE) {
  if (baixar) {
    print("Fazendo o download....")
    dados <- downCorona("https://data.brasil.io/dataset/covid19/caso_full.csv.gz")
    cart <- downCorona("https://data.brasil.io/dataset/covid19/obito_cartorio.csv.gz")
  }
  # caso já tenha o arquivo na pasta
  else {
    dados <- read.csv(file = "caso_full.csv", header = TRUE)
  }

  print("Download concluido. Transformando os dados")
  dados$tempo <- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))
  dados$date <- as.Date(dados$date)

  # acrescentar a media_movel

  dados <- dados %>%
    arrange(desc(city_ibge_code)) %>%
    group_by(city_ibge_code) %>%
    mutate(
      mm7d_confirmed = frollmean(new_confirmed, 7),
      mm7d_deaths = frollmean(new_deaths, 7)
    ) %>%
    mutate(
      var_mm_confirmed = replace_na((mm7d_confirmed / lag(mm7d_confirmed, 14) - 1) * 100, 0),
      var_mm_deaths = replace_na((mm7d_deaths / lag(mm7d_deaths, 14) - 1) * 100), 0
    ) %>%
    ungroup()

  write_feather(cart, sprintf("ob-cartorio.feather"))
  write_feather(dados, sprintf("full-covid.feather"))
  # drop_upload("full-covid.feather", dtoken = token)

  # separando por regioes e calculando os casos do brasil


  casos_regiao <- dados %>% filter(place_type == "state") %>% 
    mutate(
      regiao =
        ifelse(state %in% c("AM", "TO", "PA", "RO", "RR", "AP", "AC"), "NORTE",
          ifelse(state %in% c("MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA"), "NORDESTE",
            ifelse(state %in% c("PR", "RS", "SC"), "SUL",
              ifelse(state %in% c("MG", "RJ", "ES", "SP"), "SUDESTE",
                ifelse(state %in% c("DF", "MT", "MS", "GO"), "CENTRO-OESTE", "NA")
              )
            )
          )
        )
    ) %>%
    arrange(desc(date)) %>%
    group_by(regiao, date) %>%
    summarise(
      new_confirmed = sum(new_confirmed),
      new_deaths = sum(new_deaths)
    ) %>%
    mutate(
      mm7d_confirmed = frollmean(new_confirmed, 7),
      mm7d_deaths = frollmean(new_deaths, 7)
    )


  casos_br <- casos_regiao %>%
    group_by(date) %>%
    summarise(
      new_confirmed = sum(new_confirmed),
      new_deaths = sum(new_deaths)
    ) %>%
    mutate(
      mm7d_confirmed = frollmean(new_confirmed, 7),
      mm7d_deaths = frollmean(new_deaths, 7),
      regiao = "BRASIL"
    )

  casos_total <- bind_rows(casos_br, casos_regiao)
  write_feather(casos_total, sprintf("dados-regiao.feather"))



  # acrescentando latitude e longitude nos ultimos casos

  latlong_cidade <- read.csv("latitude-longitude-cidades.csv", sep = ";", header = TRUE)
  latlong_cidade$city <- latlong_cidade$municipio
  latlong_cidade$state <- latlong_cidade$uf

  latlong_estado <- read.csv("latitude-longitude-estados.csv", sep = ";", header = TRUE)
  latlong_estado$state <- latlong_estado$uf
  latlong_estado$place_type <- "state"

  dados <- dados %>% filter(is_last == "True")

  dados2 <- merge(dados, latlong_cidade, by = c("state", "city"), all.x = TRUE, all.y = FALSE)
  dados <- merge(dados2, latlong_estado, by = c("state", "place_type"), all.x = TRUE, all.y = FALSE)

  dados <- dados %>%
    mutate(
      latitude = ifelse(place_type == "city", latitude.x, latitude.y),
      longitude = ifelse(place_type == "city", longitude.x, longitude.y)
    )

  dados <- select(dados, -c("uf.x", "uf.y", "latitude.x", "longitude.x", "latitude.y", "longitude.y"))

  dados <- dados %>% drop_na(latitude, longitude)

  write_feather(dados, sprintf("latlong-covid.feather"))

  print("Dados baixados e salvos com sucesso.")
  # drop_upload("latlong-covid.feather", dtoken = token)
}

baixar_seade <- function() {
  # Lendo a base de dados do SEADE com os casos em SP
  casos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/dados_covid_sp.csv", sep = ";")
  leitos <- read.csv("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova_variacao_semanal.csv", sep = ";")

  # incluindo os codigos das DRS na base de leitos
  leitos <- leitos %>% mutate(cod_drs = extract_numeric(nome_drs))


  # ajustando manualmente os codigos das DRS
  leitos$cod_drs[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)] <- c(10, 7, 8, 13, 16, 12, 3, 4, 5, 11, 2, 9, 1, 14, 15, 6, 17)

  casos_drs <- casos %>%
    arrange(desc(datahora)) %>%
    # filter(nome_drs == "Grande São Paulo") %>%
    group_by(cod_drs, datahora) %>%
    summarise(
      total_novos_casos = sum(casos_novos),
      total_novos_obitos = sum(obitos_novos)
    ) %>%
    mutate(
      mm7d_casos = frollmean(total_novos_casos, 7),
      mm7d_obitos = frollmean(total_novos_obitos, 7)
    ) %>%
    left_join(leitos, by = c("datahora", "cod_drs"))

  casos_drs$datahora <- casos_drs$datahora %>% lubridate::as_date()

  # trocando as virgulas por pontos
  casos_drs$ocupacao_leitos <- as.numeric(gsub(",", ".", gsub("\\.", "", casos_drs$ocupacao_leitos)))


  write_feather(casos_drs, sprintf("seade-covid.feather"))
}
