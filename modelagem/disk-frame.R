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
  group_by(paciente_enumSexoBiologico, paciente_idade)  %>%
  summarise(n = n()) %>%
  collect()

# para salvar
write_csv(vacinados, "vacinados.csv")
