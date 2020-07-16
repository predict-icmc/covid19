library(shiny)
library(tidyverse)
library(nls.multstart)
library(minpack.lm)

#dados <- get_corona_br()
dados<-read.csv(file = "caso_full.csv",header=TRUE)
dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))

estados <- dados$state %>% unique

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Total e previsão de mortes por Covid-19 no Brasil"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: estados ----
      selectInput(inputId = "state",
                  label = "Escolha um Estado:",
                  choices = estados),
      uiOutput(outputId = "choosecity"),
      ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      #plotOutput(outputId = "distPlot"),
      # Output: Graphs ----
      plotOutput(outputId = "distPlot1")
      
    )
  )
)

# Define server logic ----
server <- function(input, output) {

    # escolhe a cidade
  output$choosecity <- renderUI({
    cities <- dados %>% filter(state == input$state & place_type == "city") %>% 
      select(city) %>% unique
    selectInput("cities", "Escolha a Cidade", cities)
  })
    #grafico por estados
   output$distPlot <- renderPlot({
    
    x    <- dados %>% filter(state == input$state) 
    
    x %>% filter(place_type == "state") %>% 
      select(date, last_available_confirmed, last_available_deaths) %>% ggplot +
      geom_line(mapping = aes(as.numeric(date), last_available_confirmed, color = "blue")) +
      geom_line(mapping = aes(as.numeric(date), last_available_deaths, color = "red")) +
      ggtitle(paste("Total de casos e óbitos no estado de",input$state)) +
      xlab("Dias desde 2020-02-25")
    
  })

   # grafico por municipios
   output$distPlot1 <- renderPlot({
     
     #ajuste ao modelo
     SaoPaulo <- dados %>% filter(state == input$state &
                                    city == input$cities &
                                    place_type == "city")
     
     SaoPaulo <- SaoPaulo[order(SaoPaulo$tempo),]
     
     fit <- nlsLM(last_available_deaths ~ SSlogis(tempo, Asym, xmid, scal),
                  start = c(Asym=1500,xmid=50,scal=10),
                  data = subset(dados,state == input$state & city == input$cities))
     
     fit.Gompertz <- nlsLM(last_available_deaths ~ SSgompertz(tempo, Asym, b2, b3),
                           start = c(Asym=1500,b2=1,b3=1),
                           data = subset(dados,state == input$state & city == input$cities))
     
     summary(fit)
     summary(fit.Gompertz)
     
     XX = (0:(max(SaoPaulo$tempo)+120))
     
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
     
    
    # x %>% ggplot +
     #  geom_line(mapping = aes(as.numeric(date), last_available_confirmed, color = "blue")) +
      # geom_line(mapping = aes(as.numeric(date), last_available_deaths, color = "red")) +
      # ggtitle(paste("Total de casos e óbitos em",input$cities,"-",input$state)) +
      # xlab("Dias desde 2020-02-25") +
      # ylab("Total de Casos e Óbitos Confirmados")
      # theme(legend.title=element_blank())
      
     
   })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
