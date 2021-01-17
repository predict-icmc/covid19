
# pegando novos dados
# utilize a pegaCorona() para baixar os dados atualizados do brasil.io
# essa função baixa os dados mais recentes e salva na working dir
# ATENÇÃO: necessário possuir os arquivos 'latitude-longitude-cidades.csv'
# e 'latitude-longitude-estados.csv' na working directory

# descomente as tres linhas abaixo, execute-as no R, depois comente novamente
# antes de rodar o shiny. Em breve: cron jobs pra evitar essa gambiarra?

#setwd("~/covid19/shiny/site_final/covid-19")
#source("merge-data.R")
#pegaCorona()


# carrega as dependencias, lê as variáveis e carrega-as para o ambiente
source("load-data.R")

# interface do usuário - shiny
ui <- fluidPage(
    
  #add_busy_bar(color = "#045a8d", height = "6px"), #barra de loading superior
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
                                                       selected = 0)#,
                                          
                                          #selectInput("color", "Variável:", vars, selected = "last_available_confirmed"),
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
                                h4(textOutput("last_update"), align = "left"),
                                         
                                radioButtons("radio1", h3("Tipo de previsão"),
                                              choices = list("Estadual" = 0),#"Municipal" = 1, "Regional" = 2),
                                              selected = 0),
                                
                                #Tipo de plot
                                
                                # Input: estado e cidade ----
                              selectInput(inputId = "state",
                                        label = "Escolha um Estado:",
                                        choices = estados,  selected = "SP"),
                              #uiOutput(outputId = "choosecity_map",  selected ="São Paulo"),
                            
                              
                              span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style="color:#045a8d"),
                              h3(textOutput("case_count"), align = "right"),
                              h3(textOutput("deaths_count"), align = "right"),
                              h3(textOutput("letality_count"), align = "right"),
                              h3(textOutput("new_cases_count"), align = "right"),
                              h3(textOutput("new_deaths_count"), align = "right"),
                              
                           
                              ),
                            
                            mainPanel(
                              
                              
                              
                              #h2("Ajuste ao modelo de Gompertz"),
                              h3("Previsão da média móvel de casos para os próximos 14 dias"),
                              #radioButtons("predType", "", vars_plot_mm, selected = "last_available_confirmed"),
                              plotlyOutput(outputId = "predict_cases"),
                              
                              #h2("Variacão da média móvel"),
                              #radioButtons("plotTypeMM", "", vars_plot_mm, selected = "new_confirmed"),
                              #plotlyOutput(outputId = "mmPlot"),
                              
                              h2("Comparacão 2019-2020 de óbitos notificados em cartório"),
                              plotlyOutput(outputId = "cartMap"),
                              
                              
                              h2("Acumulado no período"),                              
                              radioButtons("plotType", " ", vars, selected = "last_available_confirmed"),
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
    
    
    # anáilise de mortes em cartório
    output$cartMap <- renderPlotly({
      
      covdeaths <- dt %>% filter(state == input$state & place_type == "state") %>% select(date,new_deaths)
      ex <- cart %>% filter(state == input$state) %>%  select(new_deaths_total_2020,new_deaths_total_2019,date)
      ex$date <- ex$date %>% as.Date()
      
      ex <- left_join(ex,covdeaths, by = c("date" = "date")) 
      # trocando os NA's por zero
      ex[is.na(ex)] = 0
      
      ex <- tibble("Variação 2019-2020" = ex$new_deaths_total_2020 - ex$new_deaths_total_2019, date = ex$date, "Óbitos de Covid-19" = ex$new_deaths)
      #ex <- ex %>% mutate(new_deaths = new_deaths_total_2019 + new_deaths)
      plt <- ex %>% reshape2::melt(id.vars = "date")
      p <- plt %>% 
        ggplot( aes(x=date, y=value, fill=variable, text=variable)) +
        geom_area( ) +
        scale_fill_viridis(discrete = TRUE) +
        ggtitle(paste0("Mortes em excesso no estado de ", input$state)) +
        geom_vline(aes(xintercept = as.Date("2020-03-13")))
      #theme(legend.position="none")
      
      ggplotly(p)
      
    })
    
    # observer que mantém os circulos e a legenda de acordo com as variaveis escolhidas pelo usr
    observe({
      
        colorBy <- input$color
        
        if(input$radio == 1){
            zipdata <- dados %>% filter(place_type == "city" )#&& estimated_population_2019 > 200000)
            # pegando as geometrias das cidades
            shp <- get_brmap("City")
            shp$City <- as.character(shp$City)
            # definindo que o dataframe contém dados geométricos
            shp_sf <- st_as_sf(shp)%>%
              st_transform(4326)
            #unindo os dados de COVID-19 com as geometrias das cidades.
            shp_sf <- shp_sf %>% filter(City %in% dados$city_ibge_code)
            shp_sf$City <- shp_sf$City %>% as.integer()
            shp_sf <- left_join(shp_sf,dados, by = c("City" = "city_ibge_code"))        
              }
        else{
            zipdata <- dados %>% filter(place_type == "state")
            # pegando as geometrias dos estados
            shp <- get_brmap("State")
            shp$City <- as.character(shp$State)
            # definindo que o dataframe contém dados geométricos
            shp_sf <- st_as_sf(shp)%>%
              st_transform(4326)
            #unindo os dados de COVID-19 com as geometrias dos estados
            shp_sf <- shp_sf %>% filter(State %in% dados$city_ibge_code)
            shp_sf$City <- shp_sf$City %>% as.integer()
            shp_sf <- left_join(shp_sf,dados, by = c("State" = "city_ibge_code"))
            }
        #browser()
        #colorData <- zipdata[[colorBy]]
        #pal <- colorBin("Reds", colorData, 8, pretty = T)
        #pal <- colorNumeric(palette = "Reds", domain = colorData)
        pal <- colorNumeric(palette = "Reds", domain = shp_sf$last_available_confirmed_per_100k_inhabitants)
        
        #radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 100
        
        leafletProxy("map", data = shp_sf) %>%
          clearGroup("Polygons") %>% 
          addPolygons(data = shp_sf,
                      smoothFactor = 0.5,
                      fillOpacity = 0.5,
                      weight = 0.5,
                      color = ~pal(last_available_confirmed_per_100k_inhabitants),
                      opacity = 0.8,
                      stroke = FALSE,
                      highlightOptions = highlightOptions(color = "black",
                                                          weight = 2,
                                                          bringToFront = TRUE),
                      popup = ~paste0(sep = " ",
                                      "<b>", city," - ", state,"<b><br>",
                                      "<b>Casos confirmados: </b>", last_available_confirmed, "<br>",
                                      "<b>Casos por 100k habitantes: </b>", last_available_confirmed_per_100k_inhabitants,tags$br(),
                                      "Novos casos: ", new_confirmed, tags$br(),
                                      "Novos óbitos: ", new_deaths,tags$br(),
                                      "Populacão: ", estimated_population_2019,tags$br(),
                                      "Total de Casos Confirmados: ", last_available_confirmed, tags$br(),
                                      "Total de óbitos: ", last_available_deaths, tags$br(),
                                      "Taxa de letalidade: ",last_available_death_rate),
                      label = ~city, layerId=~City) %>% 
          #addPolygons(~longitude, ~latitude, layerId=~city_ibge_code,
            #                 stroke=FALSE, fillOpacity=0.3, fillColor=pal(colorData)) %>%
            #addLegend("bottomright", pal=pal, values=colorData, title=colorBy,
            #          layerId="colorLegend")
          addLegend("bottomright",
                    title = "Casos confirmados por<br>100k habitantes", 
                    pal = pal, 
                    values = ~last_available_confirmed_per_100k_inhabitants, 
                    opacity = 0.8, layerId="colorLegend")
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
    # label da ultima att
    output$last_update <- renderText({
      
    
      data <- dt %>% filter(state == input$state &
                                       place_type == "state"& is_last == "True")
      paste0("Atualizado em ",as.Date(data$date))
    })
    
    
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
    # tratamento do reactive  
      #req(input$predType)
      #req(input$chooseCity)
      req(input$state)
      show_modal_spinner() # show the modal window
      
    # cidade ou estado 
    if(input$radio1 == 1)
      selectedCity <- dt %>% filter(state == input$state &
                                      city == input$chooseCity &
                                      place_type == "city")
      else
        selectedCity <- dt %>% filter(state == input$state &
                                        place_type == "state")
      
    # confirmados ou mortes
      #if(input$predType ==  "new_confirmed")
      #  oq = "Novos casos confirmados"
      #else
      #  oq = "Novos óbitos confirmados"
    
    #browser()
      #mm <- selectedCity %>% select(input$predType) %>% frollmean(7)
      mm <- frollmean(selectedCity$new_confirmed,7)
      
      fit <- nnetar(mm,lambda ="auto",p=7)
      
      # isso aqui demora um bocado.
      # intervalos de confiança para a predição
      pred <- predict(fit,14, PI = T, level = c(0.95, 0.80))
      remove_modal_spinner() # remove it when done
      #browser()
      
      trace1 <- list(
        line = list(
          color = "rgba(0,0,0,1)", 
          fillcolor = "rgba(0,0,0,1)"
        ), 
        mode = "lines", 
        name = "Média móvel", 
        type = "scatter", 
        x =  selectedCity$date,
        y =  round(mm), 
        xaxis = "x", 
        yaxis = "y"
      )
      trace2 <- list(
        fill = "toself", 
        line = list(
          color = "rgba(242,242,242,1)", 
          fillcolor = "rgba(242,242,242,1)"
        ), 
        mode = "lines", 
        name = "95% confiança", 
        type = "scatter", 
        x = pandate(c(time(pred$upper[,1]),rev(time(pred$lower[,1])))),
        y = round( c( pred$upper[,1], rev(pred$lower[,1]))),
        xaxis = "x", 
        yaxis = "y", 
        hoveron = "points"
      )
      trace3 <- list(
        fill = "toself", 
        line = list(
          color = "rgba(204,204,204,1)", 
          fillcolor = "rgba(204,204,204,1)"
        ), 
        mode = "lines", 
        name = "80% confiança", 
        type = "scatter", 
        x = pandate(c(time(pred$upper[,1]),rev(time(pred$lower[,1])))),
        y = round( c( pred$upper[,2], rev(pred$lower[,2]))),
        xaxis = "x", 
        yaxis = "y", 
        hoveron = "points"
      )
      trace4 <- list(
        line = list(
          color = "rgba(0,0,255,1)", 
          fillcolor = "rgba(0,0,255,1)"
        ), 
        mode = "lines", 
        name = "predição", 
        type = "scatter", 
        x = pandate( time( pred$mean)), 
        y = round( pred$mean), 
        xaxis = "x", 
        yaxis = "y"
      )
      data <- list(trace1, trace2, trace3, trace4)
      layout <- list(
        title = paste0("Estado de ",input$state), 
        xaxis = list(
          title = "Data", 
          domain = range(selectedCity$date)
        ), 
        yaxis = list(
          title = paste0("Novos casos"), 
          domain = c(0, 1)
        ), 
        margin = list(
          b = 40, 
          l = 60, 
          r = 10, 
          t = 25
        )
      )
      p <- plot_ly()
      p <- add_trace(p, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, xaxis=trace1$xaxis, yaxis=trace1$yaxis)
      p <- add_trace(p, fill=trace2$fill, line=trace2$line, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, xaxis=trace2$xaxis, yaxis=trace2$yaxis, hoveron=trace2$hoveron)
      p <- add_trace(p, fill=trace3$fill, line=trace3$line, mode=trace3$mode, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, xaxis=trace3$xaxis, yaxis=trace3$yaxis, hoveron=trace3$hoveron)
      p <- add_trace(p, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type, x=trace4$x, y=trace4$y, xaxis=trace4$xaxis, yaxis=trace4$yaxis)
      p %>% add_bars(x = ~date, y = ~selectedCity$new_confirmed)
      p <- layout(p, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin) 
      p %>%  add_bars(y = selectedCity$new_confirmed, x = selectedCity$date, name = "Novos casos diários")
    })
    
    output$mmPlot <- renderPlotly({
      # expressão do reactive para tratamento do input    
      #req(input$chooseCity)
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
        #req(input$chooseCity)
        req(input$state)
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
#rsconnect::deployApp(account = "predict-icmc")
