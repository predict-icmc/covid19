library(tidyverse)
#Temos 3 bancos de dados a serem usados

#Começarei pela "Série antiga (variação mensal)".

leitos1<-read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova.csv",
                   encoding = "UTF-8")

#Começo e fim da Séries
leitos1$datahora<-as.Date(leitos1$datahora)

max(leitos1$datahora)
min(leitos1$datahora)

#Os dados estão atualizados porem o começo de sua série é apartir do dia 8 do mes de outubro
#de 2020


#Os drs do banco de dados 
table(leitos1$nome_drs)

#Temos dados sobre 17 drs e o estado de são paulo

#Variável "pacientes_uti_mm7d"
#Média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados 
#para  COVID-19 no dia

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
ggplot(aes(x=datahora,y=pacientes_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"paciente_uti_mm7d\" para a Grande São Paulo")+
  theme_bw()


#Variável "total_covid_uti_mm7d"
#Média móvel para 7 dias do Total de Leitos de UTI Destinados para COVID-19 no dia

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=total_covid_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"total_covid_uti_mm7d\" para a Grande São Paulo")+
  theme_bw()


#Variável "ocupacao_leitos"
#Ocupação de leitos de UTI destinados para COVID-19 
#(pacientes_uti_mm7d / total_covid_uti_mm7d)


leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=ocupacao_leitos))+
  geom_line()+
  ggtitle("Série temporal da Variável \"ocupacao_leitos\" para a Grande São Paulo")+
  theme_bw()+
  scale_y_continuous(breaks=seq(1,100,5))

#Variável pop
#População da DRS ou região da Grande São Paulo (Fonte: SEADE)
#É uma informação estatica porem no banco de dados ela se repete para cada dia

filtro<-leitos1 %>% 
  filter(datahora==max(leitos1$datahora)) %>% 
  select(nome_drs,pop)
View(filtro)

#Variável leitos_pc
#Leitos Covid-19 UTI por 100 mil habitantes (total_covid_uti_mm7d/pop)

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=leitos_pc))+
  geom_line()+
  ggtitle("Série temporal da Variável \"leitos_pc\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_28d
#Número de novas internações (UTI e Enfermaria) 
#de pacientes confirmados ou com suspeita de COVID-19 nos últimos 28 dias

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_28d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_28d\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_28d_l
#Número de novas internações (UTI e Enfermaria)
#de pacientes confirmados ou com suspeita de COVID-19 nos 28 dias anteriores

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_28d_l))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_28d_l\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_28v28
#Variação no número de novas internações
#((internacoes_28d - internacoes_28d_l) / internacoes_28d_l)

leitos1 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_28v28))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_28v28\" para a Grande São Paulo")+
  theme_bw()+
  geom_hline(yintercept = 0)


#Agora vamos para o banco de dados chamado de "Série antiga (variaçõa semanal)"

leitos2<-read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes.csv",
                   encoding = "UTF-8")

#Começo e fim das Séries
leitos2$datahora<-as.Date(leitos2$datahora)

min(leitos2$datahora)
max(leitos2$datahora)

#Os dados estão atualizados e começam apartir do dia 19 do mês 5 de 2020.

#Os drs do banco de dados 
table(leitos2$nome_drs)

#Temos dados sobre 17 drs e o estado de são paulo porem a grande são paulo
#Esta dividida em Leste, norte, oeste, sudeste, sudoeste. 

#Variável "pacientes_uti_mm7d"

#Média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados 
#para  COVID-19 no dia


leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=pacientes_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"paciente_uti_mm7d\" para o Estado de São Paulo")+
  theme_bw()


#Variável "total_covid_uti_mm7d"
#Média móvel para 7 dias do Total de Leitos de UTI Destinados para COVID-19 no dia

leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=total_covid_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"total_covid_uti_mm7d\" para o Estado de São Paulo")+
  theme_bw()


#Variável "ocupacao_leitos"
#Ocupação de leitos de UTI destinados para COVID-19 
#(pacientes_uti_mm7d / total_covid_uti_mm7d)


leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=ocupacao_leitos))+
  geom_line()+
  ggtitle("Série temporal da Variável \"ocupacao_leitos\" para o Estado de São Paulo")+
  theme_bw()+
  scale_y_continuous(breaks=seq(1,100,5))

#Variável pop
#População da DRS ou região da Grande São Paulo (Fonte: SEADE)
#É uma informação estatica porem no banco de dados ela se repete para cada dia

filtro<-leitos2 %>% 
  filter(datahora==max(leitos2$datahora)) %>% 
  select(nome_drs,pop)
View(filtro)

#Variável leitos_pc
#Leitos Covid-19 UTI por 100 mil habitantes (total_covid_uti_mm7d/pop)

leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=leitos_pc))+
  geom_line()+
  ggtitle("Série temporal da Variável \"leitos_pc\" para a Estado de São Paulo")+
  theme_bw()

#Variável internacoes_7d
#Número de novas internações (UTI e Enfermaria) 
#de pacientes confirmados ou com suspeita de COVID-19 nos últimos 7 dias

leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7d\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_7d_l
#Número de novas internações (UTI e Enfermaria) 
#de pacientes confirmados ou com suspeita de COVID-19 nos 7 dias anteriores

leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7d_l))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7d_l\" para o Estado de São Paulo")+
  theme_bw()

#Variável internacoes_7v7
#Variação no número de novas internações 
#((internacoes_7d - internacoes_7d_l) / internacoes_7d_l)

leitos2 %>%
  filter(nome_drs=="Estado de São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7v7))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7v7\" para o Estado de São Paulo")+
  theme_bw()+
  geom_hline(yintercept = 0)


#Agora vamos para o banco de dados chamado de "Série em uso (variaçõa semanal e Grande São Paulo unificada)"

leitos3<-read.csv2("https://raw.githubusercontent.com/seade-R/dados-covid-sp/master/data/plano_sp_leitos_internacoes_serie_nova_variacao_semanal.csv",
                   encoding = "UTF-8")

#Começo e fim das Séries
leitos3$datahora<-as.Date(leitos3$datahora)

min(leitos3$datahora)
max(leitos3$datahora)

#Os dados estão atualizados porem o começo de sua série é apartir do dia 8 do mes de outubro
#de 2020


#Os drs do banco de dados 
table(leitos3$nome_drs)

#Temos dados sobre 17 drs e o estado de são paulo

#Variável "pacientes_uti_mm7d"

#Média móvel para 7 dias do Pacientes Internados em Leitos de UTI Destinados 
#para  COVID-19 no dia


leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=pacientes_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"paciente_uti_mm7d\" para a Grande São Paulo")+
  theme_bw()


#Variável "total_covid_uti_mm7d"
#Média móvel para 7 dias do Total de Leitos de UTI Destinados para COVID-19 no dia

leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=total_covid_uti_mm7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"total_covid_uti_mm7d\" para a Grande São Paulo")+
  theme_bw()


#Variável "ocupacao_leitos"
#Ocupação de leitos de UTI destinados para COVID-19 
#(pacientes_uti_mm7d / total_covid_uti_mm7d)


leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=ocupacao_leitos))+
  geom_line()+
  ggtitle("Série temporal da Variável \"ocupacao_leitos\" para a Grande São Paulo")+
  theme_bw()+
  scale_y_continuous(breaks=seq(1,100,5))

#Variável pop
#População da DRS ou região da Grande São Paulo (Fonte: SEADE)
#É uma informação estatica porem no banco de dados ela se repete para cada dia

filtro<-leitos3 %>% 
  filter(datahora==max(leitos1$datahora)) %>% 
  select(nome_drs,pop)
View(filtro)

#Variável leitos_pc
#Leitos Covid-19 UTI por 100 mil habitantes (total_covid_uti_mm7d/pop)

leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=leitos_pc))+
  geom_line()+
  ggtitle("Série temporal da Variável \"leitos_pc\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_28d
#Número de novas internações (UTI e Enfermaria) 
#de pacientes confirmados ou com suspeita de COVID-19 nos últimos 7 dias

leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7d))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7d\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_7d_l
#Número de novas internações (UTI e Enfermaria) 
#de pacientes confirmados ou com suspeita de COVID-19 nos 7 dias anteriores

leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7d_l))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7d_l\" para a Grande São Paulo")+
  theme_bw()

#Variável internacoes_7v7
#Variação no número de novas internações 
#((internacoes_7d - internacoes_7d_l) / internacoes_7d_l)

leitos3 %>%
  filter(nome_drs=="DRS 01 Grande São Paulo") %>% 
  ggplot(aes(x=datahora,y=internacoes_7v7))+
  geom_line()+
  ggtitle("Série temporal da Variável \"internacoes_7v7\" para a Grande São Paulo")+
  theme_bw()+
  geom_hline(yintercept = 0)

#Grandes Diferenças 

#Começo das bases de dados a base leitos1 e leitos3 começam de outubro enquanto a leitos2 
#começa no dia 19 de Maio

#Todos banco de são diarios, suas colunas finais aparentam ter medidas semanais para as 
#bases leitos2 e leitos3 enquanto leito1 é mensal

#A base de dados leitos2 dividi a grande são paulo em mais setores enquanto as outra consideram
#grande são paulo como um unico drs


#################################################

#Calculando o total de leitos

sp<-leitos3 %>% 
  filter(nome_drs=="DRS 01 Grande São Paulo")

plot(sp$datahora,sp$pacientes_uti_mm7d)  
plot(sp$total_covid_uti_mm7d,add=T)

(max(sp$pacientes_uti_mm7d))

sp %>% 
  filter(pacientes_uti_mm7d==3320.43)

max(sp$total_covid_uti_mm7d)


