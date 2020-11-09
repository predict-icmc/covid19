library(tidyverse)
#Função para transformar onde não tem nome para "Estado" 
mudar<-function(dados){
  for (i in (1:length(dados$city))) {
    if(dados$city[i]==""){
      dados$city[i] <- "Estado"
    } 
  }
  return(dados)
}

cidade<-df %>% 
  dplyr::filter(city=="São Paulo"& place_type=="city")

plot(cidade$date,cidade$last_available_confirmed_per_100k_inhabitants)


estado<-df %>% 
  dplyr::filter(city=="São Paulo" | (state=="SP" & place_type=="state"))

abline(estado$date,estado$last_available_confirmed_per_100k_inhabitants)

estado<-mudar(estado)

estado %>% 
ggplot(aes(x=date,y=last_available_confirmed))+ 
  geom_line(aes(colour=place_type))+
  xlab("Data")+
  ylab("Dados confirmados por 100k habitantes")

  
ggplot(aes(x=da,y=porcento))+ 
geom_bar(stat="identity")+
xlab("Data")+
ylab("Dados confirmados por 100k habitantes")

da<-unique(estado$date)
porcento <- estado$new_confirmed[estado$place_type=="city"] / estado$new_confirmed[estado$place_type=="state"]
taxa<-data.frame(dias="da",taxa=porcento)

menor<-taxa %>% 
  filter(taxa)
plot(menor)

#################################################################################################
#Medias com semana epidemiologica ~ São Paulo
cidade<-df %>% 
  dplyr::filter(city=="São Paulo"& place_type=="city")
estado<-df %>% 
  dplyr::filter(state=="SP" & place_type=="state")

estado$epidemiological_week<-as.factor(estado$epidemiological_week)
cidade$epidemiological_week<-as.factor(cidade$epidemiological_week)

mediaestado<-with(estado, tapply(new_confirmed, epidemiological_week, mean))
mediacidade<-with(cidade, tapply(new_confirmed, epidemiological_week, mean))

taxa<-data.frame(semana=9:45,porcento=mediacidade/mediaestado)

taxa %>% 
  ggplot(aes(x=semana,y=porcento))+ 
  geom_point()+
  geom_line()+
  xlab("Semana Epidemiologica")+
  scale_y_continuous("Taxa da Cidade de São Paulo",breaks = seq(0,1,0.1),limits = c(0,1))

#################################################################################################
cidade<-df %>% 
  dplyr::filter(state=="Porto Alegre"& place_type=="city")
estado<-df %>% 
  dplyr::filter(state=="RS" & place_type=="state")

estado$epidemiological_week<-as.factor(estado$epidemiological_week)
cidade$epidemiological_week<-as.factor(cidade$epidemiological_week)

mediaestado<-with(estado, tapply(new_confirmed, epidemiological_week, mean))
mediacidade<-with(cidade, tapply(new_confirmed, epidemiological_week, mean))


cidade$date==estado[-1,]$date
intersect(cidade$date,cidade$date)
setequal(cidade$date,estado[-1,]$date)



intersecttaxa<-data.frame(semana=min(numeric(estado$epidemiological_week)):max(numeric(estado$epidemiological_week)),porcento=mediacidade/mediaestado)

taxa %>% 
  ggplot(aes(x=semana,y=porcento))+ 
  geom_point()+
  geom_line()+
  xlab("Semana Epidemiologica")+
  scale_y_continuous("Taxa da Cidade do Rio Grande do Sul",breaks = seq(0,1,0.1),limits = c(0,1))
#################################################################################################

estado.ibg<-unique(df$city_ibge_code[df$state=="RS"&df$place_type=="state"])

View(df %>% 
  filter(state=="MS"))

novodf<-df %>% mutate(cabrito = case_when(
  city %in% c("Porto Velho","Manaus","Rio Branco", "Campo Grande","Macapá","Brasília","Boa Vista",
              "Cuiabá","Palmas","São Paulo","Teresina","Rio de Janeiro","Belém","Goiânia","Salvador",
              "Florianópolis","São Luís","Maceió","Porto Alegre","Curitiba","Belo Horizonte",
              "Fortaleza","Recife","João Pessoa","Aracaju","Natal","Vitória") ~ 1,
  TRUE~0
))




novodf<-novodf %>% 
  filter(place_type=="state" | cabrito=="1")

mediaspeidemi <- novodf %>% 
  group_by(city_ibge_code,epidemiological_week) %>% 
  mutate(media=mean(new_confirmed)) %>%
  ungroup(city_ibge_code,epidemiological_week) %>% 
  group_by(state,epidemiological_week) %>% 
  mutate()
  
mediaspeidemi$media[mediaspeidemi$cabrito=="1" & mediaspeidemi$city_ibge_code_ib]

capitais<- novodf %>% 
  filter(place_type=="city" & capital=="1") %>% 
  select(city,date,epidemiological_week,state,new_confirmed,new_deaths)

estado<- novodf %>% 
  filter(place_type=="state") %>% 
  select(date,epidemiological_week,state,new_confirmed,new_deaths)



mediaestado<-with(capitais, tapply(new_confirmed, INDEX = c(epidemiological_week,state), mean))
mediacidade<-with(estado, tapply(new_confirmed, INDEX = c(epidemiological_week,state), mean))

mediaspeidemi <- novodf %>% 
  group_by(place_type,epidemiological_week,state) %>% 
  mutate(media=mean(new_confirmed))

#########################################################################################
novodf<-df %>% mutate(capital= case_when(
  city_ibge_code %in% c(1100205,1302603,1200401,5002704,1600303,5300108,1400100,
                        5103403,1721000,3550308,2211001,3304557,1501402,5208707,2927408,
                        4205407,2111300,2704302,4314902,4106902,3106200,
                        2304400,2611606,2507507,2800308,2408102,3205309) ~1,
  TRUE~0
))


################################################################################
#tester nomarlidade multivariada
#padronizar variaveis demograficas e fazer pca(padrinizados) ou stepwise 
#corplot das variaveis [IDH,espvida,gini,t_agua,rdpc,densidade]
#analise fatorial
#analise exploratoria 
#mapa do idh, maga das variaveis demograficas
#200> pessoas correlação acumulado 100k
#################################################################################

qqnorm(sp$new_confirmed)
library(MASS)

box<-boxcox(sp$new_confirmed[sp$new_confirmed>0] ~ 1)



library(mvShapiroTest)
d<-sample_n(sp,5000)
sp<-df %>% 
  filter(city=="São Paulo" & place_type=="city")


mvShapiro.Test((sp$new_confirmed[sp$new_confirmed>0])^0.4)
qqnorm((sp$new_confirmed[sp$new_confirmed>0])^0.4)
