#' Script que recebe um arquivo .csv dos dados de vacinacao DATASUS
#' e computa o numero de vacinados por faixa etaria e sexo
#' 
#' mais informações e download dos datasets em https://opendatasus.saude.gov.br/dataset/covid-19-vacinacao

library(disk.frame)
library(tidyverse)

# inicializa o disk frame
setup_disk.frame()

# this will allow unlimited amount of data to be passed from worker to worker
options(future.globals.maxSize = Inf)

#' lê o arquivo [vc.csv] e carrega num disk frame
some.df <- csv_to_disk.frame("vc.csv", outdir = "some.df", in_chunk_size = 1e6) # %>%
#   select(vacina_dataAplicacao,
#          vacina_fabricante_nome,
#          paciente_enumSexoBiologico,
#          paciente_idade)

# computa a frequencia de cada um dos grupos e converte no formato usual do R
vacinados <- some.df %>%
  group_by(paciente_enumsexobiologico, paciente_idade)  %>%
  summarise(n = n()) %>%
  collect()

# computa a frequencia de vacinados em cada municipio e converte no formato usual do R
vacinados_municip <- some.df %>%
  group_by(estabelecimento_municipio_nome, vacina_dataaplicacao)  %>%
  summarise(n = n()) %>%
  collect()


# computa a frequencia de vacinados em cada municipio e converte no formato usual do R
vacinados_por_grupo <- some.df %>%
  group_by(vacina_dataaplicacao, vacina_grupoatendimento_nome, vacina_descricao_dose)  %>%
  summarise(n = n()) %>%
  collect()

# computa a frequencia de vacinados em cada municipio e converte no formato usual do R
vacinados_sp <- some.df %>%
  group_by(vacina_dataaplicacao, vacina_nome, vacina_descricao_dose)  %>%
  summarise(n = n()) %>%
  collect()


colnames(vacinados_municip) <- c("municip","data","n")
colnames(vacinados_por_grupo) <- c("data","grupo","dose","n")
colnames(vacinados_sp) <- c("data","vacina","dose","n")

# para salvar
write_csv(vacinados, "vacinados.csv")
write_csv(vacinados_municip, "vacinados_munic.csv")
write_csv(vacinados_por_grupo, "vacinados_grupo.csv")
write_csv(vacinados_sp, "vacinados_sp.csv")
