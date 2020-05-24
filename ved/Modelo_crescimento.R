#install.packages("dplyr")
library(dplyr)
library(ggplot2)
#install.packages("spDataLarge", repos = "https://nowosad.github.io/drat/", type = "source")
library(tidyverse)
#install.packages('nls.multstart')
library(nls.multstart)
library(minpack.lm)
library(randomForest)


dados1<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)
dados1$tempo<- as.numeric(as.Date(dados1$date) - min(as.Date(dados1$date)))

latlong_cidade<-read.csv('latitude-longitude-cidades.csv', sep=';', header=TRUE)
latlong_cidade$city <-latlong_cidade$municipio
latlong_cidade$state <-latlong_cidade$uf

latlong_estado<-read.csv('latitude-longitude-estados.csv', sep=';', header=TRUE)
latlong_estado$state <-latlong_estado$uf
latlong_estado$place_type='state'

#Juntando as bases de dados
dados2 <- merge(dados1,latlong_cidade,by=c('state','city'), all.x=TRUE, all.y=FALSE)
dados <- merge(dados2,latlong_estado,by=c('state','place_type'), all.x=TRUE, all.y=FALSE)

dados <-dados %>%
  mutate(latitude = ifelse(place_type=='city', latitude.x, latitude.y),
         longitude = ifelse(place_type=='city',longitude.x, longitude.y)) 


dados <-select(dados, -c('uf.x','uf.y','latitude.x','longitude.x','latitude.y','longitude.y'))

# Criando variável semanas
dados$semanas<- floor(dados$tempo/7)
#View(dados)

# Variável que é igual a 0 no final de cada semana. 
dados$final_semana <- dados$tempo%%7

# Pegando somente valores no final de cada semana 
teste = subset(dados, dados$final_semana==0)

# filtrando os dados, selecionando apenas os estados

Estados <- filter(teste, teste$place_type == 'state')
View(Estados)

# adicionando a coluna de mortes diária, dado que as mortes estavam acumuladas

Estados <- Estados %>% 
  group_by(state) %>% 
  arrange(semanas)%>%
  mutate(obitos_dia = deaths - lag(deaths, defaut = first(deaths)))

# Modelo crescimento logístico estado de SP
SaoPaulo <- filter(Estados, state == 'SP')

fit <- nlsLM(deaths ~ SSlogis(semanas, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10),
             data = subset(Estados,state=='SP'))

ggplot(SaoPaulo, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=semanas,y=fitted(fit)))+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(SaoPaulo$semanas , max(SaoPaulo$semanas)+(1:4))

Asym<-coef(fit)[1]
xmid<-coef(fit)[2]
scal<-coef(fit)[3]

yp <-Asym/(1+exp((xmid-XX)/scal))

predict<-data.frame(XX,yp)

ggplot(SaoPaulo, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()


# Previsão para a próxima semana em SP
yp[max(SaoPaulo$semanas)+1]
View(SaoPaulo)
# Curva de novos casos

SaoPaulo$obitos_semana <- c(0,diff(SaoPaulo$deaths))

predict$obitos_semana_preditos <-c(0,diff(yp))

ggplot(SaoPaulo, aes(x = semanas, y = obitos_semana)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=XX, y = obitos_semana_preditos),
            data = predict,
            colour = "red")+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)







# Modelo crescimento logístico estado de PE
Pernambuco <- filter(Estados, state == 'PE')

fit <- nlsLM(deaths ~ SSlogis(semanas, Asym, xmid, scal),
             start = c(Asym=1500,xmid=50,scal=10),
             data = subset(Estados,state=='PE'))

ggplot(Pernambuco, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=semanas,y=fitted(fit)))+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(Pernambuco$semanas , max(Pernambuco$semanas)+(1:4))

Asym<-coef(fit)[1]
xmid<-coef(fit)[2]
scal<-coef(fit)[3]

yp <-Asym/(1+exp((xmid-XX)/scal))

predict<-data.frame(XX,yp)

ggplot(Pernambuco, aes(x = semanas, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()

# Previsão para a próxima semana em PE
yp[max(Pernambuco$semanas)+1]

# Curva de novos casos

Pernambuco$obitos_semana <- c(0,diff(Pernambuco$deaths))

predict$obitos_semana_preditos <-c(0,diff(yp))

ggplot(Pernambuco, aes(x = semanas, y = obitos_semana)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=XX, y = obitos_semana_preditos),
            data = predict,
            colour = "red")+
  labs(title = 'Mortes por COVID em semanas', subtitle='Estado de PE', x = 'Semanas',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)



####################### Random forest

sp <- filter(dados, (dados$place_type == 'state')&(dados$state=='SP'))
sp = sp[order(sp$tempo),]

isolamento = read.csv('isolamento.csv', header=TRUE)

isola_sp = subset(isolamento, isolamento$estado=='SP')
isola_sp$tempo<- as.numeric(as.Date(isola_sp$data, format='%d-%m-%y') - 
                              min(as.Date(sp$date)))+7
isola_sp =subset(isola_sp,isola_sp$tempo>=7, select = c('tempo','indice'))
View(df)

sp_2 = subset(sp, (sp$tempo<80)&(sp$tempo>=7))

#Juntando as bases de dados
df <- merge(sp_2,isola_sp,by=c('tempo'))

# Somente com os dados temporais
colnames(Estados)
X = select(sp, c('tempo'))
Y = select(sp, c('deaths'))

classifier = randomForest(X,Y$deaths,ntree = 100, random_state = 0)

ggplot(sp, aes(x = tempo, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=tempo,y=classifier$predicted))+
  labs(title = 'Mortes por COVID em Dias', subtitle='Estado de SP', x = 'Dias',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(sp$tempo , max(sp$tempo)+(1:4))

y_pred = predict(classifier, newdata = matrix(c(89:92), ncol=1))
y_pred
yp = c(classifier$predicted,y_pred)

predict<-data.frame(XX,yp)

ggplot(sp, aes(x = tempo, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()

# Com dados de isolamento
colnames(Estados)
X = select(df, c('tempo','indice'))
Y = select(df, c('deaths'))

classifier = randomForest(X,Y$deaths,ntree = 100, random_state = 0)

ggplot(df, aes(x = tempo, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  geom_line(aes(x=tempo,y=classifier$predicted))+
  labs(title = 'Mortes por COVID em Dias', subtitle='Estado de SP', x = 'Dias',
       y = 'Número de óbitos', fill = 'estados') +
  theme_bw()+
  facet_wrap(~state, ncol = 9)

# Predição próximas 4 semanas 
XX = c(df$tempo , max(df$tempo)+(1:4))

y_pred = predict(classifier, newdata = matrix(c(80,81,82,83,43,43,43,43), ncol=2))
y_pred
yp = c(classifier$predicted,y_pred)

predict<-data.frame(XX,yp)

ggplot(df, aes(x = tempo, y = deaths)) + 
  geom_point(size = 2, color = "blue") +
  labs(title = 'Previsão de mortes por COVID em semanas', subtitle='Estado de SP', x = 'Semanas', 
       y = 'Número de óbitos', fill = 'estados') +
  geom_line(aes(x=XX, y = yp),
            data = predict,
            colour = "red")+
  theme_bw()



##########################################################################################################
################################### MODELO SEIR #############################################
library (deSolve) 

seir_model = function (current_timepoint, state_values, parameters)
{
  # create state variables (local variables)
  S = state_values [1]        # susceptibles
  E = state_values [2]        # exposed
  I = state_values [3]        # infectious
  R = state_values [4]        # recovered
  
  with ( 
    as.list (parameters),     # variable names within parameters can be used 
    {
      # compute derivatives
      dS = (-beta * S * I)
      dE = (beta * S * I) - (delta * E)
      dI = (delta * E) - (gamma * I)
      dR = (gamma * I)
      
      # combine results
      results = c (dS, dE, dI, dR)
      list (results)
    }
  )
}

contact_rate = 3                     # number of contacts per day
transmission_probability = 0.07       # transmission probability
infectious_period = 10                # infectious period
latent_period = 5                     # latent period

beta_value = contact_rate * transmission_probability
gamma_value = 1 / infectious_period
delta_value = 1 / latent_period

Ro = beta_value / gamma_value

parameter_list = c (beta = beta_value, gamma = gamma_value, delta = delta_value)

W = 45919049 - 80558      # susceptible hosts
X = 29461          # infectious hosts
Y = 45950         # recovered hosts
Z = 50         # exposed hosts

N = W + X + Y + Z
W
initial_values = c (S = W/N, E = X/N, I = Y/N, R = Z/N)

timepoints = seq (80, 120, by=1)

output = lsoda (initial_values, timepoints, seir_model, parameter_list)

# susceptible hosts over time
plot (S ~ time, data = output, type='b',  col = 'blue', ylab = 'S, E, I, R', main = 'SEIR epidemic') 

# remain on same frame
par (new = TRUE)    

# exposed hosts over time
plot (E ~ time, data = output, type='b',  col = 'pink', ylab = '', axes = FALSE)

# remain on same frame
par (new = TRUE) 

# infectious hosts over time
plot (I ~ time, data = output, type='b',  col = 'red', ylab = '', axes = FALSE) 

# remain on same frame
par (new = TRUE)  

# recovered hosts over time
plot (R ~ time, data = output, type='b', col = 'green', ylab = '', axes = FALSE)















