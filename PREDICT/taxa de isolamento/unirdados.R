library(rstudioapi)
library(tidyverse)
library(dplyr)
# - - - - - - - - - - --  - - - -
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#função para arrumar o indice de isolamento para um valor possivel de contas
arrumar_dados <- function(df){
  df$Índice.de.isolamento <- str_replace_all(df$Índice.de.isolamento,"%","")
  df$Índice.de.isolamento <- as.numeric(df$Índice.de.isolamento)
  return(df)
}

#Quero pegar todos os datasets do diretório junta-los num grande data.frame(eles tem mesmas
#colunas), e também quero fazer uma identificação de qual data-frame está vindo cada um.

# 1. change wd - Isso é pra quem não usa o rstudio api
#setwd('new_file_path')

# 2. pegando meu arquivos do diretório, se eu não coloco padrão .csv ele acha o código em R 
#na pasta também, e outros tipos de arquivos

my_files <- list.files(pattern = ".csv")#sai em formato de lista

# 3. Lendo todos os arquivos, o lapply aplica a mesma função para todos os arquivos da lista
BDiso <- lapply(my_files, read_csv)

#6 juntando em um dataset, uso a função do dplyr::bind_rows(data(ou lista de datasets), 
#id = ("nome da coluna de identificação")), o id é usado para identificar de qual dataset está vindo
BDiso <- bind_rows(BDiso, .id = "dataset(dia)")

#Uso a função separate para que eu possa separar as colunas da banco de dados, já que elas vieram numa só
BDiso <- BDiso %>% separate(`Município;UF;Cdufmun;DATA;Escala de cor;Número de registros;População;Índice de isolamento`,
                                  into = c("cidade","UF","Cdufmun","DATA","Escala.de.cor","Número de registros","População","Índice.de.isolamento"),
                                  sep = ";")
BDiso <- arrumar_dados(BDiso) #arrumei a coluna indice de isolamento.

BDiso$DATA <- as.Date(BDiso$DATA, "%d/%m/%y")

BDiso <- BDiso %>% 
  arrange(DATA)

write.csv(BDiso, "C:\\Users\\Yuri Reis Valete\\Desktop\\Projetos\\PREDICT\\taxa de isolamento\\isolamento.csv", row.names = F)


