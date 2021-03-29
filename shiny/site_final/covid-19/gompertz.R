# arquivo com plot do modelo de gompertz pro shiny.

# modelagem somente a partir dos ultimos 75 dias
# selectedCity <- selectedCity #%>% filter(tempo > max(selectedCity$tempo)-75)
# modelo de Gompertz com auto-inicialização
#fit.Gompertz.cases <- nlsLM(var ~ SSgompertz(tempo, Asym, b2, b3),
#   

# tempo de previsao: 10 dias
#XX = (0:(max(selectedCity$tempo)+10))
#Asym.G<-coef(fit.Gompertz.cases)[1]
#b2.G<-coef(fit.Gompertz.cases)[2]
#b3.G<-coef(fit.Gompertz.cases)[3]

#yp.G<-0
#yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)

#predict.G<-data.frame(x=XX,y=yp.G)

# exibe só os dados mais recentes
#predict.filtra <- predict.G %>% filter(x > max(selectedCity$tempo)-40)

# gera o grafico
p<-  ggplot(selectedCity.filtra) + 
  geom_line(aes(x = tempo, y = var), size = 1, color = "blue") +
  geom_point(aes(x=x, y = y, color = "red", alpha = .4),
             data = predict.filtra) +
  labs(x = 'Dias desde o primeiro caso', 
       y = 'Total acumulado', fill = '') +
  theme_bw()
ggplotly(p) %>% hide_legend() #%>% config(displayModeBar = F)
