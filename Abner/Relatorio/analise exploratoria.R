library(gather.covid)
library(tidyverse)
library(udunits2)
library(units)
library(sf)
library(geobr)
library("scales")
library(zoo)
#############################################
#install.packages("broom", type="binary")   #
#install.packages("pillar", type="binary")  #
#install.packages("colorspace")             #
#install.packages("units")                  #
#install.packages("udunits2")               #
#############################################

df<-pegaCorona(tipo="caso_full",baixar = T)

####################################
analise_variaveis <- df %>% 
  select(city,date,estimated_population,estimated_population_2019,last_available_confirmed,last_available_confirmed_per_100k_inhabitants,last_available_death_rate,last_available_deaths,place_type,state,new_confirmed,new_deaths)
summary(df)



# analise_variaveis %>%
#   filter(new_confirmed>0) %>%
#   group_by(analise_variaveis$date) %>%
#  summarise(novo_soma = sum(analise_variaveis$new_confirmed)) %>%
#   select(novo_soma,date)
#   ggplot(aes(x = date , y = novo_soma))+
#   geom_line()


#DATA X NEW CONFIRMED####

# Estado de São Paulo
sp_data<- df %>% 
  filter(state=="SP" & place_type=="state")
  
sp_data %>%   
  ggplot(aes(x = date , y = new_confirmed))+
    geom_line(color="Blue")+
    ggtitle("Novos casos de covid do Estado de São Paulo desde o começo da Pandemia")+
    xlab("Data")+
    ylab("Novos Casos")+
    scale_x_date(limit=c(as.Date("2020-02-25"),as.Date("2021-05-24"))) +
    scale_y_continuous(breaks = seq(0,30000,by = 3000))

# Cidade de São Paulo

csp_data <- df %>% 
  filter(city=="São Paulo" & place_type=="city")

unique(analise_variaveis$city)

####15,6 PROPORÇÂO GRAFICOS###

csp_data %>% 
  ggplot(aes(x = date , y = new_confirmed))+
    geom_line(color="Blue")+
    ggtitle("Novos casos de Covid-19 da Cidade de São Paulo desde o começo da Pandemia")+
    xlab("Data")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
    ylab("Novos Casos")+
    scale_y_continuous(breaks = seq(0,30000,by = 3000))


csp_data %>% 
  ggplot(aes(x = date , y = new_deaths))+
  geom_line(color="Blue")+
  ggtitle("Novos obitos por Covid-19 na Cidade de São Paulo desde o começo da Pandemia")+
  xlab("Data")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  ylab("Quantidade De Mortos")+
  scale_y_continuous(breaks = seq(0,450,by = 25))

csp_data %>% 
  ggplot(aes(x = date , y = last_available_confirmed))+
  geom_line(color="Blue")+
  ggtitle("Casos de Covid-19 Acumulados na Cidade de São Paulo desde o começo da Pandemia")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  xlab("Data")+
  ylab("Casos Acumalados")+
  scale_y_continuous(breaks = seq(0,1000000,by = 100000),labels = comma_format(big.mark = ".",
                                                                              decimal.mark = ","))


csp_data %>% 
  ggplot(aes(x = date , y = last_available_deaths))+
  geom_line(color="Blue")+
  ggtitle("Obitos por Covid-19 Acumulados na Cidade de São Paulo desde o começo da Pandemia")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  xlab("Data")+
  ylab("Numero de Obitos")+
  scale_y_continuous(breaks = seq(0,35000,by = 2500),labels = comma_format(big.mark = ".",
                                                                               decimal.mark = ","))

csp_data["mm_novos_casos"]<-rollmean(ts(csp_data$new_confirmed),7,fill=NA,align="right")
csp_data["mm_novos_obitos"]<-rollmean(ts(csp_data$new_deaths),7,fill=NA,align="right")

csp_data %>% 
  drop_na() %>% 
  ggplot(aes(x = date , y = mm_novos_casos))+
  geom_line(color="Blue")+
  ggtitle("Média Movel de casos de Covid-19 da Cidade de São Paulo desde o começo da Pandemia")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  xlab("Data")+
  ylab("Numero de Casos")+
  scale_y_continuous(breaks = seq(0,6000,by = 400),labels = comma_format(big.mark = ".",
                                                                           decimal.mark = ","))
csp_data %>% 
  drop_na() %>% 
  ggplot(aes(x = date , y = mm_novos_obitos))+
  geom_line(color="Blue")+
  ggtitle("Média Movel de Obitos por Covid-19 da Cidade de São Paulo desde o começo da Pandemia")+
  theme(plot.title = element_text(size = 25, face = "bold"))+
  xlab("Data")+
  ylab("Numero de Obitos")+
  scale_y_continuous(breaks = seq(0,250,by = 25),labels = comma_format(big.mark = ".",
                                                                         decimal.mark = ","))

##### Summary##########333
t(summary(csp_data))


#############################################################
glimpse(analise_variaveis)
datasets <- list_geobr()       

grap_map_sp<- df %>% 
  filter(place_type=="state",date=="2021-06-04")



states <- read_state(code_state = "all" ,year= 2020)

sort(states$code_state)
sort(grap_map_sp$city_ibge_code)
states$abbrev_state <- tolower(states$abbrev_state)
grap_map_sp$state <- tolower(grap_map_sp$state)
states <- dplyr::left_join(states, grap_map_sp, by = c("code_state" = "city_ibge_code"))


ggplot() +
  geom_sf(data=states, aes(fill=log(last_available_confirmed)), size=.15) +
  labs(subtitle="States", size=8) +
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


ts.plot(ts(csp_data$new_confirmed))

plot(states$code_muni)

