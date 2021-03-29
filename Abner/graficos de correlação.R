library(corrplot)
library(tidyverse)
library(grDevices)

View(demografico)

correla<-demografico %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade) %>% 
  na.omit()


correla<-data.frame(lapply(correla,as.numeric))
correla<-data.frame(lapply(correla,as.double))


corrplot(cor(as.matrix(correla),method = "spearman"))

correla<-last.data %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla)[names(correla) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla
correla<-data.frame(lapply(correla,as.double))

corrplot.mixed(cor(as.matrix(correla),method = "spearman"), lower.col = "black", number.cex = .9, 
               bg = "gray")




corrRect(c(6,4))
############################################################
##Grafico correlação com cidades com mais de  200.000 habitantes 
correla2<-last.data %>% 
  filter(estimated_population_2019>=200000)

correla2<-correla2 %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla2)[names(correla2) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla2<-data.frame(lapply(correla2,as.double))

corrplot.mixed(cor(as.matrix(correla2),method = "spearman"), lower.col = "black", number.cex = .9, 
               bg = "gray")
corrRect(c(6,4))
##################################################################
######Grafico correlação cidade com menos de 200.000 habitantes
correla3<-last.data %>% 
  filter(estimated_population_2019<200000)

correla3<-correla3 %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla3)[names(correla3) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla3<-data.frame(lapply(correla3,as.double))


corrplot.mixed(cor(as.matrix(correla3),method = "spearman"))
cov(correla3)

corrplot.mixed(cor(as.matrix(correla3),method = "spearman"), lower.col = "black", number.cex = .9, 
               bg = "gray")
corrRect(c(6,4))

################################################################

#############GRAFICO DE CORRELAÇÂO 1 TODAS as cidades #########

names(correla)<- c("IDH","Esperança de Vida","Gini","Tratatamento de Agua","Renda per capita","Densidade","Ultimos Casos por 100k","Casos Confirmados","Taxa de mortes confirmadas","Mortes confirmadas")
cor1<-cor(as.matrix(correla),method = "spearman")
corrplot.mixed(cor1, lower.col = "black", number.cex = .7, tl.pos = "lt")

names(correla2)<- c("IDH","Esperança de Vida","Gini","Tratatamento de Agua","Renda per capita","Densidade","Ultimos Casos por 100k","Casos Confirmados","Taxa de mortes confirmadas","Mortes confirmadas")
cor2<-cor(as.matrix(correla2),method = "spearman")
corrplot.mixed(cor2, lower.col = "black", number.cex = .7, tl.pos = "lt")

names(correla3)<- c("IDH","Esperança de Vida","Gini","Tratatamento de Agua","Renda per capita","Densidade","Ultimos Casos por 100k","Casos Confirmados","Taxa de mortes confirmadas","Mortes confirmadas")
cor3<-cor(as.matrix(correla3),method = "spearman")
corrplot.mixed(cor3, lower.col = "black", number.cex = .7, tl.pos = "lt")



