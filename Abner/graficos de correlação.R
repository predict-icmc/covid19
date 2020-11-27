library(corrplot)

View(demografico)

correla<-demografico %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade) %>% 
  na.omit()

correla<-data.frame(lapply(correla,as.numeric))

corrplot(cor(as.matrix(correla),method = "spearman"))

correla<-bdcorrc %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla)[names(correla) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla
correla<-data.frame(lapply(correla,as.double))

corrplot.mixed(cor(as.matrix(correla),method = "spearman"))

############################################################
##Grafico correlação com cidades com mais de  200.000 habitantes 
correla2<-bdcorrc %>% 
  filter(estimated_population_2019>=200000)

correla2<-correla2 %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla2)[names(correla2) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla2<-data.frame(lapply(correla2,as.double))

corrplot.mixed(cor(as.matrix(correla2),method = "spearman"))
##################################################################
######Grafico correlação cidade com menos de 200.000 habitantes
correla3<-bdcorrc %>% 
  filter(estimated_population_2019<200000)

correla3<-correla3 %>% 
  select(IDH,espvida,gini,t_agua,rdpc,densidade,last_available_confirmed_per_100k_inhabitants,last_available_confirmed,last_available_death_rate,last_available_deaths) %>% 
  na.omit()

names(correla3)[names(correla3) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"
correla3<-data.frame(lapply(correla3,as.double))

corrplot.mixed(cor(as.matrix(correla3),method = "spearman"))
cov(correla3)
################################################################