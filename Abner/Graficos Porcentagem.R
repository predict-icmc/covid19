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

taxa<-data.frame(semana=9:44,porcento=mediacidade/mediaestado)

taxa %>% 
  ggplot(aes(x=semana,y=porcento))+ 
  geom_point()+
  geom_line()+
  xlab("Semana Epidemiologica")+
  scale_y_continuous("Taxa da Cidade de São Paulo",breaks = seq(0,1,0.1),limits = c(0,1))

#################################################################################################
cidade<-df %>% 
  dplyr::filter(city=="Porto Alegre"& place_type=="city")
estado<-df %>% 
  dplyr::filter(state=="RS" & place_type=="state")

estado$epidemiological_week<-as.factor(estado$epidemiological_week)
cidade$epidemiological_week<-as.factor(cidade$epidemiological_week)

mediaestado<-with(estado, tapply(new_confirmed, epidemiological_week, mean))
mediacidade<-with(cidade, tapply(new_confirmed, epidemiological_week, mean))


taxa<-data.frame(semana=min(numeric(estado$epidemiological_week)):max(numeric(estado$epidemiological_week)),porcento=mediacidade/mediaestado)

taxa %>% 
  ggplot(aes(x=semana,y=porcento))+ 
  geom_point()+
  geom_line()+
  xlab("Semana Epidemiologica")+
  scale_y_continuous("Taxa da Cidade do Rio Grande do Sul",breaks = seq(0,1,0.1),limits = c(0,1))
#################################################################################################


estado.ibg<-unique(df$city_ibge_code[df$state=="RS"&df$place_type=="state"])

novodf<-df %>% mutate(cabrito = case_when(
  city %in% c("Porto Velho","Manaus","Rio Branco","	Campo Grande","Macapá","Brasília","Boa Vista",
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

taxatodos<-mediaspeidemi %>% 
  dplyr::filter(cabrito=="1",substr(city_ibge_code)==estado.ibg) %>% 
  mutate(media/)
