library(shiny)
library(tidyverse)
library(nls.multstart)
library(minpack.lm)
library(plotly)

#dt <- get_corona_br()
dt<-read.csv(file = "2020-08-11_caso_full.csv",header=TRUE)
dt$tempo<- as.numeric(as.Date(dt$date) - min(as.Date(dt$date)))
dt$date <- as.Date(dt$date)

estados <- dt$state %>% unique

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # painel superior
  navbarPage(collapsible = F,
             tags$img(src = "logo-predict-small1.jpg"),
             tabPanel("Previsão",
                      titlePanel(h2("Casos e mortes por Covid-19 no Brasil por município")), 
                      
                      
                      # painel por cidades
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          radioButtons("radio", h3("Tipo de previsão"),
                                       choices = list("Municipal" = 1, "Estadual" = 0),
                                       selected = 1),
                          
                          # Input: estado e cidade ----
                          selectInput(inputId = "state",
                                      label = "Escolha um Estado:",
                                      choices = estados),
                          uiOutput(outputId = "choosecity"),
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          tabsetPanel(type = "tabs",
                                      tabPanel("Previsão de Casos", plotlyOutput(outputId = "distPlot")),
                                      tabPanel("Previsão de Mortes", plotOutput(outputId = "distPlot1")))
                          
                        )
                      )
             ),
             
             # painel sobre  
             tabPanel("Sobre",
                      titlePanel(h2("Sobre o Predict-Covid19")),
                      
                      paste("Este projeto visa a análise de dados de COVID-19 por meio de técnicas de visualização de dados e modelos preditivos, para o dimensionamento e prevenção dos impactos da epidemia de COVID-19 e outras síndromes respiratórias agudas graves, utilizando estatística e ciência de dados. Nossa proposta envolve a predição do número de casos e óbitos, a demanda de internações hospitalares de acordo com diferentes intervenções não farmacológicas, o que inclui medidas de distanciamento social, isolamento voluntário, isolamento de sintomáticos, uso de equipamentos de proteção individual (EPIs), monitoramento de contatos próximos ou domiciliares, triagem em serviços de saúde, entre outras.")
             )
             
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # escolhe a cidade a partir do input
  output$choosecity <- renderUI({
    if(input$radio == 1){
      cities <- dt %>% filter(state == input$state & place_type == "city") %>% 
        select(city) %>% unique
      selectInput("cities", "Escolha a Cidade", cities)}
    
  })
  #grafico de confirmados por municipio
  output$distPlot <- renderPlotly({
    
    #ajuste ao modelo
    SaoPaulo <- dt %>% filter(state == input$state &
                                city == input$cities &
                                place_type == "city")
    
    SaoPaulo <- SaoPaulo[order(SaoPaulo$tempo),]
    
    fit <- nlsLM(last_available_confirmed ~ SSlogis(tempo, Asym, xmid, scal),
                 start = c(Asym=150,xmid=1,scal=1),
                 data = subset(dt,state == input$state & city == input$cities))
    
    fit.Gompertz <- nlsLM(last_available_confirmed ~ SSgompertz(tempo, Asym, b2, b3),
                          start = c(Asym=1500,b2=0.9,b3=0.9),
                          data = subset(dt,state == input$state & city == input$cities))
    
    #summary(fit)
    #summary(fit.Gompertz)
    
    XX = (0:(max(SaoPaulo$tempo)+10))
    
    Asym<-coef(fit)[1]
    xmid<-coef(fit)[2]
    scal<-coef(fit)[3]
    
    Asym.G<-coef(fit.Gompertz)[1]
    b2.G<-coef(fit.Gompertz)[2]
    b3.G<-coef(fit.Gompertz)[3]
    
    yp<-0
    yp.G<-0
    
    yp <-Asym/(1+exp((xmid-XX)/scal))
    yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)
    
    predict<-data.frame(x=XX,y=yp,model='Logistic')
    predict.G<-data.frame(x=XX,y=yp.G,model='Gompertz')
    
    previsao<-rbind(predict,predict.G)
    
    # gera o grafico
  p <- SaoPaulo %>% plot_ly() %>% add_lines(x = ~date, y = ~last_available_confirmed)
     #add_lines(x = ~tempo, y = ~previsao)
        # ggplot(SaoPaulo, aes(x = tempo, y = last_available_confirmed)) + 
    #   geom_point(size = 1, color = "blue") +
    #   geom_line(aes(x=x, y = y,  color=model),
    #             data = previsao,
    #             show.legend = TRUE)+
    #   labs(title = 'Total de casos confirmados de COVID-19 em dias', subtitle=paste(input$cities,"-",input$state), x = 'Dias', 
    #        y = 'Número de casos confirmados', fill = '') +
    #   theme_bw()
  })
  
  # grafico de mortos por municipios
  output$distPlot1 <- renderPlot({
    
    #ajuste ao modelo
    SaoPaulo <- dt %>% filter(state == input$state &
                                city == input$cities &
                                place_type == "city")
    
    SaoPaulo <- SaoPaulo[order(SaoPaulo$tempo),]
    
    fit <- nlsLM(last_available_deaths ~ SSlogis(tempo, Asym, xmid, scal),
                 start = c(Asym=1500,xmid=50,scal=10),
                 data = subset(dt,state == input$state & city == input$cities))
    
    fit.Gompertz <- nlsLM(last_available_deaths ~ SSgompertz(tempo, Asym, b2, b3),
                          start = c(Asym=3,b2=0.9,b3=0.9),
                          data = subset(dt,state == input$state & city == input$cities))
    
    #summary(fit)
    #summary(fit.Gompertz)
    
    XX = (0:(max(SaoPaulo$tempo)+10))
    
    Asym<-coef(fit)[1]
    xmid<-coef(fit)[2]
    scal<-coef(fit)[3]
    
    Asym.G<-coef(fit.Gompertz)[1]
    b2.G<-coef(fit.Gompertz)[2]
    b3.G<-coef(fit.Gompertz)[3]
    
    yp<-0
    yp.G<-0
    
    yp <-Asym/(1+exp((xmid-XX)/scal))
    yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)
    
    predict<-data.frame(x=XX,y=yp,model='Logistic')
    predict.G<-data.frame(x=XX,y=yp.G,model='Gompertz')
    
    previsao<-rbind(predict,predict.G)
    
    
    ggplot(SaoPaulo, aes(x = tempo, y = last_available_deaths)) + 
      geom_point(size = 1, color = "blue") +
      geom_line(aes(x=x, y = y,  color=model),
                data = previsao,
                show.legend = TRUE)+
      labs(title = 'Mortes acumuladas por COVID-19 em dias', subtitle=paste(input$cities,"-",input$state), x = 'Dias', 
           y = 'Número de óbitos', fill = '') +
      theme_bw()
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
