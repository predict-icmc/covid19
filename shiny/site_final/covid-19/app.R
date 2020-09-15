library(shiny)
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(feather)
library(plotly)
library(minpack.lm)

# pegando novos dados

# utilize a funcao pegaCorona() para baixar os dados atualizados 
# ATENÇÃO: necessário possuir os arquivos 'latitude-longitude-cidades.csv' e 'latitude-longitude-estados.csv' na working directory

# setwd("~/predict-covid19/shiny/site_final/covid-19")
#source("merge-data.R")

# essa função baixa os dados mais recentes do brasil.io e salva na pasta
#pegaCorona()

# arquivo feather a ser lido. Consulte merge-data.R para saber como gerar novos arquivos
latlong <- "latlong-covid.feather"
caso_full <- "full-covid.feather"

# Variaveis a serem exibidas
vars <- c(
    "Total de Casos Confirmados" = "last_available_confirmed",
    "Total de Óbitos" = "last_available_deaths",
    "Letalidade" = "last_available_death_rate",
    "Populacão Estimada 2019" = "estimated_population_2019",
    "Novos Casos Confirmados" = "new_confirmed",
    "Novos Óbitos" = "new_deaths"#,
    #"Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants"
)

vars_plot <- c(
    "Total de Casos Confirmados" = "last_available_confirmed",
    "Total de Óbitos" = "last_available_deaths",
    "Letalidade" = "last_available_death_rate",
    "Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants"
)


vars_plot_mm <- c(
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths"
)


# leitura dos dados.
dados<-read_feather(latlong)

estados <- dados$state %>% unique %>% as.character()
cleantable <- dados %>% select(state,city,estimated_population_2019,last_available_confirmed, last_available_deaths, last_available_death_rate,latitude,longitude,city_ibge_code)

dt<-read_feather(caso_full)



ui <- fluidPage(
    
    
    navbarPage("PREDICT - Covid-19", id="nav",
               tabPanel("Mapa Interativo",
                        div(class="outer",
                            tags$head(
                                #Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                            ),
                            
                            leafletOutput("map", width="100%", height="100%"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                          width = 330, height = "auto",
                                          
                                          h2("Mapa Interativo do Covid-19"),
                                          
                                          radioButtons("radio", h3("Listar por"),
                                                       choices = list("Cidades" = 1, "Estados" = 0),# "Regiões" = 2),
                                                       selected = 1),
                                          
                                          selectInput("color", "Variável:", vars, selected = "last_available_confirmed"),
                                          #selectInput("size", "Size", vars, selected = "last_available_death_rate")#,
                                          
                                          #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                          # Only prompt for threshold when coloring or sizing by superzip
                                          #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                            ),
                            #plotlyOutput(outputId = "predict_cases")
                            #plotOutput("graph", height = 200, click = "plot_click"),
                            #plotOutput("histCentile", height = 200),
                            #plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",tags$img(src = "logo-predict-small1.jpg"))
                        
                        #         'Desenvolvido pelo grupo', tags$em('PREDICT-Covid19'),
                        #)
                        
               ),
               tabPanel("Gráficos",
                    # painel por cidades
                        sidebarLayout(
                            
                            # Sidebar panel for inputs ----
                            sidebarPanel(position = "left",
                              
                                radioButtons("radio1", h3("Tipo de previsão"),
                                              choices = list("Municipal" = 1, "Estadual" = 0),# "Regional" = 2),
                                              selected = 1),
                                
                                #Tipo de plot
                                
                                # Input: estado e cidade ----
                              selectInput(inputId = "state",
                                        label = "Escolha um Estado:",
                                        choices = estados,  selected = "SP"),
                              uiOutput(outputId = "choosecity_map",  selected ="São Paulo"),
                            
                              span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style="color:#045a8d"),
                              h3(textOutput("case_count"), align = "right"),
                              h3(textOutput("deaths_count"), align = "right"),
                              h3(textOutput("letality_count"), align = "right"),
                              h3(textOutput("new_cases_count"), align = "right"),
                              h3(textOutput("new_deaths_count"), align = "right"),
                           
                              ),
                            
                            mainPanel(
                              
                              plotlyOutput(outputId = "predict_cases"),
                              
                              radioButtons("plotTypeMM", "", vars_plot_mm, selected = "new_confirmed"),
                              plotlyOutput(outputId = "mmPlot"),
                              
                                                            
                              radioButtons("plotType", " ", vars_plot, selected = "last_available_confirmed"),
                              plotlyOutput(outputId = "distPlot")
                              
                              
                              
                            #plotlyOutput(outputId = "predict_deaths")
                            )
                        )
                        #)
                        
               ),
               # Explorador
               tabPanel("Tabela",
                        fluidRow(
                            column(3,
                                   selectInput("states", "Estados", c("Todos os estados"="", estados), multiple=TRUE)
                            ),
                            column(3,
                                   conditionalPanel("input.states",
                                                    selectInput("cities", "Cidades", c("Todas as cidades"=""), multiple=TRUE)
                                   )
                            )#,
                            # column(3,
                            #        conditionalPanel("input.states", 
                            #                         selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                            #        )
                            #)
                            # ),
                            #  fluidRow(
                            # selectInput("color", "Variável:", vars, selected = "last_available_confirmed"),
                            #  column(1,
                            #           numericInput("minScore", "Mínimo", min=0, max=100, value=0)
                            #    ),
                            #    column(1,
                            #           numericInput("maxScore", "Máximo", min=0, max=100, value=100)
                            #    )
                        ),
                        hr(),
                        DT::dataTableOutput("ziptable")
               ),
               
               tabPanel("Sobre",
                        titlePanel(h2("Sobre o Predict-Covid19")),
                        mainPanel(
                        paste("Este projeto visa a análise de dados de COVID-19 por meio de técnicas de visualização de dados e modelos preditivos, para o dimensionamento e prevenção dos impactos da epidemia de COVID-19 e outras síndromes respiratórias agudas graves, utilizando estatística e ciência de dados. Nossa proposta envolve a predição do número de casos e óbitos, a demanda de internações hospitalares de acordo com diferentes intervenções não farmacológicas, o que inclui medidas de distanciamento social, isolamento voluntário, isolamento de sintomáticos, uso de equipamentos de proteção individual (EPIs), monitoramento de contatos próximos ou domiciliares, triagem em serviços de saúde, entre outras.")
                        )
                        
                        )
               
    ),
    conditionalPanel("false", icon("crosshair"))    
)


server <- function(input, output, session) {
    #observador que lansa as cidades a serem escolhidas
    output$choosecity <- renderUI({
        if(input$radio == 1){
            cities <- dt %>% filter(state == input$state & place_type == "city") %>% 
                select(city) %>% unique
            selectInput("chooseCity", "Escolha a Cidade", cities)}
    })
    # copia do observador que lansa as cidades a serem escolidas nos graficos
    output$choosecity_map <- renderUI({
      if(input$radio1 == 1){
        cities <- dt %>% filter(state == input$state & place_type == "city") %>% 
          select(city) %>% unique
        selectInput("chooseCity", "Escolha a Cidade", cities)}
    })
    
    
    
    ## Interactive Map ###########################################
    
    # Create the map
    output$map <- renderLeaflet({
        leaflet() %>%
            addTiles(attribution = 'Dados extraídos do site <a href="http://brasil.io/">brasil.io</a>') %>%  
            setView(lng =  -47.9292, lat = -15.7801, zoom = 4)
    })
    
    
    # observer que mantém os circulos e a legenda de acordo com as variaveis escolhidas pelo usr
    observe({
        colorBy <- input$color
        sizeBy <- input$color
        if(input$radio == 1)
            zipdata <- dados %>% filter(place_type == "city")
        else
            zipdata <- dados %>% filter(place_type == "state")
        
        colorData <- zipdata[[colorBy]]
        pal <- colorBin("viridis", colorData, 8, pretty = T)
        
        
        radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 100
        
        leafletProxy("map", data = zipdata) %>%
            clearMarkers() %>%
            addCircleMarkers(~longitude, ~latitude, radius=4+radius, layerId=~city_ibge_code,
                             stroke=FALSE, fillOpacity=0.3, fillColor=pal(colorData)) %>%
            addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
                      layerId="colorLegend")
    })
    
    # funcao que mostra a cidade clicada
    showCityPopup <- function(city_ibge_code, lat, lng) {
        selectedZip <- dados[dados$city_ibge_code == city_ibge_code,]
        content <- as.character(tagList(
            tags$h4(HTML(sprintf("%s %s",
                                 selectedZip$city, selectedZip$state
            ))),
            sprintf("Novos casos: %s", selectedZip$new_confirmed), tags$br(),
            sprintf("Novos óbitos: %s", selectedZip$new_deaths),tags$br(),
            sprintf("Populacão: %s", as.integer(selectedZip$estimated_population_2019)),tags$br(),
            sprintf("Total de Casos Confirmados: %s", as.integer(selectedZip$last_available_confirmed)), tags$br(),
            sprintf("Total de óbitos: %s", as.integer(selectedZip$last_available_deaths)), tags$br(),
            sprintf("Taxa de letalidade: %s%%", selectedZip$last_available_death_rate)
        ))
        leafletProxy("map") %>% addPopups(lng, lat, content, layerId = city_ibge_code)
        
    }
    
    
    
    #observador que mostra a cidade clicada
    observe({
        leafletProxy("map") %>% clearPopups()
        event <- input$map_marker_click
        if (is.null(event))
            return()
        
        isolate({
            showCityPopup(event$id, event$lat, event$lng)
            #aqui entraria o observador que controla a cidade a ser exibida na previsao
        })
    })
    
    # sessao de gráficos
    # label do numero de casos
    output$case_count <- renderText({
      req(input$chooseCity)
      
      if(input$radio1 == 1)
        total_casos <- dt %>% filter(state == input$state &
                                        city == input$chooseCity &
                                        place_type == "city" & is_last == "True") 
      else
        total_casos <- dt %>% filter(state == input$state &
                                        place_type == "state"& is_last == "True")
      paste0(prettyNum(total_casos$last_available_confirmed, big.mark=".", decimal.mark = ","), " casos\n")
    })
    
    # labels de texto dos gráficos
    # label do numero de mortes
    output$deaths_count <- renderText({
        req(input$chooseCity)
        
        if(input$radio1 == 1)
            total_casos <- dt %>% filter(state == input$state &
                                             city == input$chooseCity &
                                             place_type == "city" & is_last == "True") 
        else
            total_casos <- dt %>% filter(state == input$state &
                                             place_type == "state"& is_last == "True")
        paste0(prettyNum(total_casos$last_available_deaths, big.mark=".", decimal.mark = ","), " óbitos\n")
    })
    # label do numero de novos casos
    output$new_cases_count <- renderText({
        req(input$chooseCity)
        if(input$radio1 == 1)
            total_casos <- dt %>% filter(state == input$state &
                                             city == input$chooseCity &
                                             place_type == "city" & is_last == "True") 
        else
            total_casos <- dt %>% filter(state == input$state &
                                             place_type == "state"& is_last == "True")
        paste0(prettyNum(total_casos$new_confirmed, big.mark=".", decimal.mark = ","), " novos casos\n")
    })
    # label do numero de novas mortes
    output$new_deaths_count <- renderText({
        req(input$chooseCity)
        if(input$radio1 == 1)
            total_casos <- dt %>% filter(state == input$state &
                                             city == input$chooseCity &
                                             place_type == "city" & is_last == "True") 
        else
            total_casos <- dt %>% filter(state == input$state &
                                             place_type == "state"& is_last == "True")
        paste0(prettyNum(total_casos$new_deaths, big.mark=".", decimal.mark = ","), " novos óbitos\n")
    })
    # label da taxa de letalidade
    output$letality_count <- renderText({
        req(input$chooseCity)
        
        if(input$radio1 == 1)
            total_casos <- dt %>% filter(state == input$state &
                                             city == input$chooseCity &
                                             place_type == "city" & is_last == "True") 
        else
            total_casos <- dt %>% filter(state == input$state &
                                             place_type == "state"& is_last == "True")
        paste0(scales::percent(total_casos$last_available_death_rate), " letalidade\n")
    })
    
    # previsoes no plot_ly. Finalmente!
    
    output$predict_cases <- renderPlotly({
      req(input$chooseCity) #tratamento do reactive
      
    # cidade ou estado 
    if(input$radio1 == 1)
      selectedCity <- dt %>% filter(state == input$state &
                                      city == input$chooseCity &
                                      place_type == "city")
      else
        selectedCity <- dt %>% filter(state == input$state &
                                        place_type == "state")
      
      # ajuste do modelo
      
      # modelo de Gompertz com auto-inicialização
      fit.Gompertz.cases <- nlsLM(last_available_confirmed ~ SSgompertz(tempo, Asym, b2, b3),
                                  #start = c(Asym=1,b2=0,b3=0),
                                  data = selectedCity)
      XX = (0:(max(selectedCity$tempo)+10))
      Asym.G<-coef(fit.Gompertz.cases)[1]
      b2.G<-coef(fit.Gompertz.cases)[2]
      b3.G<-coef(fit.Gompertz.cases)[3]
      
      yp.G<-0
      yp.G<-Asym.G*exp((-b2.G)*(b3.G)^XX)
      
      predict.G<-data.frame(x=XX,y=yp.G)
      
      # exibe só os dados mais recentes
      predict.filtra <- predict.G %>% filter(x > max(selectedCity$tempo-1))
      selectedCity.filtra <- selectedCity %>% filter(tempo > max(selectedCity$tempo)-30)
      
      # gera o grafico
      p<-  ggplot(selectedCity.filtra) + 
        geom_line(aes(x = tempo, y = last_available_confirmed), size = 1, color = "blue") +
        geom_point(aes(x=x, y = y, color = "red", alpha = .4),
                   data = predict.filtra) +
        labs(title = 'Previsão de casos nos próximos 10 dias', subtitle=paste(input$cities,"-",input$state), x = 'Dias', 
             y = 'Total de casos confirmados', fill = '') +
        theme_bw()
      ggplotly(p) %>% config(displayModeBar = F) %>% hide_legend()
      
    })
    
    output$mmPlot <- renderPlotly({
      # expressão do reactive para tratamento do input    
      req(input$chooseCity)
      req(input$plotTypeMM)
      
      # seleciona cidade ou estado        
      if(input$radio1 == 1)
        selectedCity <- dt %>% filter(state == input$state &
                                        city == input$chooseCity &
                                        place_type == "city")
      else
        selectedCity <- dt %>% filter(state == input$state &
                                        place_type == "state")
      
      # input
      selectedvar <- selectedCity %>% select(input$plotTypeMM) %>% pull
      
      #     #calcula a media móvel
          rmean <- frollmean(selectedvar, 7)
          p <-  selectedCity %>% plot_ly() %>%
              add_bars(x = ~date, y = ~selectedvar) %>%
              add_lines(x = ~date, y = ~rmean) %>%
              config(displayModeBar = F) %>%
              hide_legend()
      
    })
    
    #grafico de baixo
    output$distPlot <- renderPlotly({
        # expressão do reactive para tratamento do input    
        req(input$chooseCity)
        req(input$plotType)
        
        # seleciona cidade ou estado        
        if(input$radio1 == 1)
            selectedCity <- dt %>% filter(state == input$state &
                                      city == input$chooseCity &
                                      place_type == "city")
      else
        selectedCity <- dt %>% filter(state == input$state &
                                        place_type == "state")
      
     # input
    selectedvar <- selectedCity %>% select(input$plotType) %>% pull
    
    # if(input$plotType == "new_cases" | input$plotType == "new_deaths"){
    #     #calcula a media móvel
    #     rmean <- frollmean(selectedvar, 7)
    #     p <-  selectedCity %>% plot_ly() %>%
    #         add_bars(x = ~date, y = ~selectedvar) %>%
    #         add_lines(x = ~date, y = ~rmean) %>%
    #         config(displayModeBar = F) %>%
    #         hide_legend()    
    # }
    # else
# plota o grafico    
    p <- selectedCity %>% plot_ly() %>% 
        add_lines(x = ~date, y = ~selectedvar) %>% 
        config(displayModeBar = F) %>% 
        hide_legend()
        
        })
        
    
    # data explorer (tabela)
    observe({
        cities <- if (is.null(input$states)) character(0) else {
            filter(cleantable, state %in% input$states) %>%
                `$`('city') %>%
                unique() %>%
                sort()
        }
        stillSelected <- isolate(input$cities[input$cities %in% cities])
        updateSelectizeInput(session, "cities", choices = cities,
                             selected = stillSelected, server = TRUE)
    })
    
    
    # vou deixar comentado pq acho q nao precisa   
    # observe({
    #   zipcodes <- if (is.null(input$states)) character(0) else {
    #     cleantable %>%
    #       filter(State %in% input$states,
    #              is.null(input$cities) | City %in% input$cities) %>%
    #       `$`('Zipcode') %>%
    #       unique() %>%
    #       sort()
    #   }
    #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
    #                        selected = stillSelected, server = TRUE)
    # })
    
    observe({
        if (is.null(input$goto))
            return()
        isolate({
            map <- leafletProxy("map")
            map %>% clearPopups()
            dist <- 0.5
            zip <- input$goto$city
            lat <- input$goto$lat
            lng <- input$goto$lng
            showCityPopup(zip, lat, lng)
            map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
        })
    })
    
    output$ziptable <- DT::renderDataTable({
        df <- cleantable %>%
            filter(
                #Score >= input$minScore,
                #Score <= input$maxScore,
                is.null(input$states) | state %in% input$states,
                is.null(input$cities) | city %in% input$cities#,
                #is.null(input$zipcodes) | Zipcode %in% input$zipcodes
            ) %>%
            mutate("Ir ao mapa" = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude,'" data-city="', city_ibge_code, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
        action <- DT::dataTableAjax(session, df, outputId = "ziptable")
        
        
        colnames(df) <- c("Estado","Cidade","Populacão Estimada 2019","Total de Casos","Total de Mortes","Taxa de Letalidade","Latitude","Longitude","Código IBGE", "Ir ao Mapa")
        DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })
    
}
#browser()
shinyApp(ui = ui, server = server)

# enviar site
#rsconnect::deployApp(account = "predict-covid")
