#' @PREDICT-ICMC app.R

#'* pegando novos dados*
#' utilize a função [pegaCorona()] para baixar os dados atualizados do brasil.io
# ela irá baixar os dados mais recentes e salvar na working dir

#' @ATENÇÃO: necessário possuir os arquivos 'latitude-longitude-cidades.csv'
# e 'latitude-longitude-estados.csv' na working directory

#' [ execute no console as tres linhas comentadas abaixo ]
# source("merge-data.R")
# pegaCorona()
# baixar_seade()

#'* enviar site (necessario ter token do shinyapps na maquina)*
# rsconnect::deployApp(account = "predict-icmc")

#'* carregando as dependencias no ambiente* 
#' para informacoes sobre as dependências necessárias, consulte o script abaixo
source("load-data.R")

# interface do usuário - shiny
ui <- fluidPage(

  # barra de carregamento superior
  add_busy_bar(color = "#045a8d", height = "6px"), # barra de loading superior
  # tab do mapa interativo
  navbarPage("PREDICT - Covid-19",
    id = "nav",
    tabPanel(
      "Mapa Interativo",
      div(
        class = "outer",
        tags$head(
          # Include our custom CSS
          includeCSS("styles.css"),
          includeScript("gomap.js")
        ),
        # plot do mapa
        leafletOutput("map", width = "100%", height = "100%"),
        # painel flutuante
        absolutePanel(
          id = "controls", class = "panel panel-default", fixed = TRUE,
          draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
          width = 330, height = "auto",
          h2("Mapa Interativo do Covid-19"),
          radioButtons("radio", h3("Listar por"),
            choices = list("Cidades" = 1, "Estados" = 0), # "Regiões" = 2),
            selected = 0
          ),
          selectInput("color", "Variável:", vars, selected = "var_mm_confirmed"),
          checkboxInput("log", "Escala log", value = FALSE),
          selectInput("palette", "Palheta de Cores:",
            list(
              "Sequencial:" = list("Reds", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
              "Divergente: " = list("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG"),
              "Colorblind: " = list("viridis", "magma", "plasma")
            ),
            selected = "Spectral"
          ),
          numericInput("nclass", "Número de classes (legenda):", min = 3, max = 9, value = 5)


          # conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),
        # plotlyOutput(outputId = "predict_cases")
        # plotOutput("graph", height = 200, click = "plot_click"),
        # plotOutput("histCentile", height = 200),
        # plotOutput("scatterCollegeIncome", height = 250)
      ),
      # logo inferior
      tags$div(id = "cite", tags$img(src = "logo-predict-small1.jpg"))

      #         'Desenvolvido pelo grupo', tags$em('PREDICT-Covid19'),
      # )
    ),
    # tab graficos por estado
    tabPanel(
      "Modelagem preditiva",
      # painel por cidades
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          position = "left",
          h4(textOutput("last_update"), align = "left"),
          radioButtons("radio1", h3("Tipo de previsão"),
            choices = list("Municipal" = 1, "Estadual" = 0, "Regional" = 2, "Federal" = 3),
            selected = 0
          ),
          # Input: estado e cidade ----
          uiOutput(outputId = "state"),
          uiOutput(outputId = "choosecity"), # ,  selected ="São Paulo"),
          radioButtons("radio2",h3("Variável") ,
                       choices = list("Novos Casos" = 1, "Novos Óbitos" = 0), 
                       selected = 1
          ),
          numericInput("pred_rng", "Intervalo de predição (semanas)", min=1, max=10, value=3),
          radioButtons("radio3",h3("Modelo") ,
                       choices = list("ARIMA" = 0, "NNAR" = 1), 
                       selected = 0
          ),
          numericInput("minScore", "I.C. Mínimo (%)", min=60, max=99, value=80),
          numericInput("maxScore", "I.C. Máximo (%)", min=60, max=99.9, value=95),
             

          # actionButton(
          #   inputId = "submit_state",
          #   label ="Gerar previsão"),
          # # labels com informacoes

          # h3(textOutput("case_count"), align = "right"),
          # h3(textOutput("deaths_count"), align = "right"),
          # h3(textOutput("letality_count"), align = "right"),
          # h3(textOutput("new_cases_count"), align = "right"),
          # h3(textOutput("new_deaths_count"), align = "right"),
          span(tags$i(h5("Dados retirados do portal brasil.io")), style = "color:#045a8d"),
          span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style = "color:#045a8d"),
        ),
        mainPanel(

          # graficos do plotly por estado
          h2(textOutput('pred_hdr')),
          h4(textOutput("title")),
          # radioButtons("predType", "" , vars_plot_mm, selected = "last_available_confirmed"),
          plotlyOutput(outputId = "predict_cases"),

          # h2("Variacão da média móvel"),
          # radioButtons("plotTypeMM", "", vars_plot_mm, selected = "new_confirmed"),
          # plotlyOutput(outputId = "mmPlot"),

          # h2("Comparacão 2019-2020 de óbitos notificados em cartório"),
          # h5("Pode haver atraso na consolidação dos dados"),
          # plotlyOutput(outputId = "cartMap"),
          # #h2("Acumulado no período"),
          #radioButtons("plotType", " ", vars_plot, selected = "last_available_confirmed"),
          #plotlyOutput(outputId = "distPlot")


          # plotlyOutput(outputId = "predict_deaths")
        )
      )
      # )
    ),
    tabPanel(
      "Gráficos por DRS (SP)",
      # painel por drs
      sidebarLayout(

        # Sidebar panel for inputs ----
        sidebarPanel(
          position = "left",
          h4(textOutput("last_update_drs"), align = "left"),
          
          # radioButtons("radio1", h3("Tipo de previsão"),
          #             choices = list("Estadual" = 0),#"Municipal" = 1, "Regional" = 2),
          #              selected = 0),

          # Tipo de plot

          # Input: estado e cidade ----
          selectInput(
            inputId = "drs",
            label = "Escolha uma DRS:",
            choices = drs_seade$nome_drs %>% levels()
          ),
          # uiOutput(outputId = "choosecity_map",  selected ="São Paulo"),
          # radioButtons("radio2",h3("Variável") ,
          #              choices = list("Novos Casos" = 1, "Novos Óbitos" = 0), 
          #              selected = 1
          # ),
          # numericInput("pred_rng", "Intervalo de predição (semanas)", min=1, max=10, value=3),
          # radioButtons("radio3",h3("Modelo") ,
          #              choices = list("ARIMA" = 0, "NNAR" = 1), 
          #              selected = 0
          # # ),
          # numericInput("minScore", "I.C. Mínimo", min=60, max=99, value=80),
          # numericInput("maxScore", "I.C. Máximo", min=60, max=99, value=95),
          
          span(tags$i(h5("Dados retirados do SEADE")), style = "color:#045a8d"),        
          span(tags$i(h6("A notificação dos casos está sujeita a uma variação significativa devido a política de testagem e capacidade das Secretarias Estaduais e Municipais de Saúde.")), style = "color:#045a8d") # ,
          # h3(textOutput("case_count"), align = "right"),
          # h3(textOutput("deaths_count"), align = "right"),
          # h3(textOutput("letality_count"), align = "right"),
          # h3(textOutput("new_cases_count"), align = "right"),
          # h3(textOutput("new_deaths_count"), align = "right"),
        ),
        mainPanel(
          h2("Previsão de casos para os próximos 21 dias"),
          h4("Ajuste ao modelo de Redes Dinâmicas"),
          # radioButtons("predType", "", vars_plot_mm, selected = "last_available_confirmed"),
          plotlyOutput(outputId = "predict_cases_drs"),

          # h2("Acumulado no período")
          # radioButtons("plotType", " ", names(drs_seade)),
          # plotlyOutput(outputId = "distPlot")
        )
      )
      # )
    ),
    # Explorador
    tabPanel(
      "Tabela",
      fluidRow(
        column(
          3,
          selectInput("states", "Estados", c("Todos os estados" = "", estados), multiple = TRUE)
        ),
        column(
          3,
          conditionalPanel(
            "input.states",
            selectInput("cities", "Cidades", c("Todas as cidades" = ""), multiple = TRUE)
          )
        ),
        # column(3,
        #        conditionalPanel("input.states",
        #                         selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        #        )
        # )
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
    tabPanel(
      "Sobre",
      titlePanel(h2("Sobre o Predict-Covid19")),
      mainPanel(
        column(
          2,
          tags$img(src = "logo-predict-1.jpg", width = "100%")
        ),
        column(
          10,
          paste("Este projeto visa a análise de dados de COVID-19 por meio de técnicas de visualização de dados e modelos preditivos, para o dimensionamento e prevenção dos impactos da epidemia de COVID-19 e outras síndromes respiratórias agudas graves, utilizando estatística e ciência de dados. Nossa proposta envolve a predição do número de casos e óbitos, a demanda de internações hospitalares de acordo com diferentes intervenções não farmacológicas, o que inclui medidas de distanciamento social, isolamento voluntário, isolamento de sintomáticos, uso de equipamentos de proteção individual (EPIs), monitoramento de contatos próximos ou domiciliares, triagem em serviços de saúde, entre outras."),
          h2("Equipe"),
          tags$ul(
            tags$li("Cibele Russo"),
            tags$li("Abner Leite"),
            tags$li("Flaviane Louzeiro"),
            tags$li("Francisco Rosa Dias de Miranda"),
            tags$li("Paulo Filho"),
            tags$li("Yuri Reis")
          )
        )
      )
    )
  ),
  conditionalPanel("false", icon("crosshair"))
)

# server side da aplicacao
server <- function(input, output, session) {
  # observador que lansa as cidades a serem escolhidas
  output$choosecity <- renderUI({
    if (input$radio1 == 1) {
      req(input$state)
      cities <- dt %>%
        filter(state == input$state & place_type == "city") %>%
        arrange(city) %>% 
        select(city) %>%
        unique()
      selectInput("chooseCity", "Escolha a Cidade", cities)
    }
  })
  output$state <- renderUI({
    if (input$radio1 %in% c(0,1)) {
      selectInput("state",label = "Escolha um Estado:",
                  choices = estados, selected = "SP")
    }
    else if (input$radio1 == 2) {
      selectInput("region",label = "Escolha uma Região:",
                  choices = regioes)
    }
    else if (input$radio1 == 3) return(NULL)
  })
  

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(attribution = 'Dados extraídos do site <a href="http://brasil.io/">brasil.io</a>') %>%
      setView(lng = -47.9292, lat = -15.7801, zoom = 4)
  })


  # observer que mantém o mapa e a legenda de acordo com as variaveis escolhidas pelo usr
  observe({
    req(input$color)

    colorBy <- input$color

    if (input$radio == 1) {
      zipdata <- dados %>% filter(place_type == "city" && estimated_population_2019 > 100000)
      # pegando as geometrias das cidades
      shp <- get_brmap("City")

      shp$City <- as.character(shp$City)
      # definindo que o dataframe contém dados geométricos
      shp_sf <- st_as_sf(shp) %>%
        st_transform(4326)
      # unindo os dados de COVID-19 com as geometrias das cidades.
      shp_sf <- shp_sf %>% filter(City %in% dados$city_ibge_code)
      shp_sf$City <- shp_sf$City %>% as.integer()
      shp_sf <- left_join(shp_sf, dados, by = c("City" = "city_ibge_code"))
    }
    else {
      zipdata <- dados %>% filter(place_type == "state")
      # pegando as geometrias dos estados
      shp <- get_brmap("State")

      shp$City <- as.character(shp$State)
      # definindo que o dataframe contém dados geométricos
      shp_sf <- st_as_sf(shp) %>%
        st_transform(4326)
      # unindo os dados de COVID-19 com as geometrias dos estados
      shp_sf <- shp_sf %>% filter(State %in% dados$city_ibge_code)
      shp_sf$City <- shp_sf$City %>% as.integer()
      shp_sf <- left_join(shp_sf, dados, by = c("State" = "city_ibge_code"))
    }
    # browser()
    colorData <- shp_sf[[colorBy]]

    # escala log se solicitado
    if (input$log) {
      colorData <- log10(colorData)
    }

    req(input$palette)
    req(input$nclass)

    if (input$palette %in% c("Spectral", "RdYlGn", "RdYlBu", "RdGy", "RdBu", "PuOr", "PRGn", "PiYG", "BrBG")) {
      pal <- colorBin(input$palette, colorData, req(input$nclass), pretty = T, reverse = T)
    } else {
      pal <- colorBin(input$palette, colorData, req(input$nclass), pretty = T, reverse = F)
    }

    leafletProxy("map", data = shp_sf) %>%
      clearShapes() %>%
      addPolygons(
        data = shp_sf,
        smoothFactor = 0.5,
        fillOpacity = 0.5,
        weight = 0.5,
        fillColor = ~ pal(colorData),
        opacity = 0.8,
        stroke = F,
        highlightOptions = highlightOptions(
          color = "black",
          weight = 2,
          bringToFront = TRUE
        ),
        popup = ~ paste0(
          sep = " ",
          "<b>", nome, "<b><br>",
          "Variacao da média móvel de casos: ", round(var_mm_confirmed, 2), "%<b><br>",
          "Variacao da média móvel de óbitos: ", round(var_mm_deaths, 2), "%<b><br>",
          "<b>Casos confirmados: </b>", last_available_confirmed, "<br>",
          "<b>Casos por 100k habitantes: </b>", last_available_confirmed_per_100k_inhabitants, tags$br(),
          "Novos casos: ", new_confirmed, tags$br(),
          "Novos óbitos: ", new_deaths, tags$br(),
          "Populacão: ", estimated_population_2019, tags$br(),
          "Total de Casos Confirmados: ", last_available_confirmed, tags$br(),
          "Total de óbitos: ", last_available_deaths, tags$br(),
          "Taxa de letalidade: ", last_available_death_rate
        ),
        label = ~nome, layerId = ~City
      ) %>%
      addLegend("bottomright",
        title = names(colorBy),
        pal = pal,
        values = colorBy,
        opacity = 0.8, layerId = "colorLegend"
      )
  })

  # # funcao que mostra a cidade clicada
  # showCityPopup <- function(city_ibge_code, lat, lng) {
  #     selectedZip <- dados[dados$city_ibge_code == city_ibge_code,]
  #     content <- as.character(tagList(
  #         tags$h4(HTML(sprintf("%s %s",
  #                              selectedZip$city, selectedZip$state
  #         ))),
  #         sprintf("Novos casos: %s", selectedZip$new_confirmed), tags$br(),
  #         sprintf("Novos óbitos: %s", selectedZip$new_deaths),tags$br(),
  #         sprintf("Populacão: %s", as.integer(selectedZip$estimated_population_2019)),tags$br(),
  #         sprintf("Total de Casos Confirmados: %s", as.integer(selectedZip$last_available_confirmed)), tags$br(),
  #         sprintf("Total de óbitos: %s", as.integer(selectedZip$last_available_deaths)), tags$br(),
  #         sprintf("Taxa de letalidade: %s%%", selectedZip$last_available_death_rate)
  #     ))
  #     leafletProxy("map") %>% addPopups(lng, lat, content, layerId = city_ibge_code)
  #
  # }

  # observador que mostra a cidade clicada a partir da tabela
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event)) {
      return()
    }

    isolate({
      showCityPopup(event$id, event$lat, event$lng)
      # aqui entraria o observador que controla a cidade a ser exibida na previsao
    })
  })

  #'@Gráficos
  # Mortes em cartório
  output$cartMap <- renderPlotly({
    req(input$state)

    covdeaths <- dt %>%
      filter(state == input$state & place_type == "state") %>%
      select(date, new_deaths)
    ex <- cart %>%
      filter(state == input$state) %>%
      select(new_deaths_total_2020, new_deaths_total_2019, date)
    ex$date <- ex$date %>% as.Date()

    ex <- left_join(ex, covdeaths, by = c("date" = "date"))
    # trocando os NA's por zero
    ex[is.na(ex)] <- 0

    ex <- tibble("Variação 2019-2020" = ex$new_deaths_total_2020 - ex$new_deaths_total_2019, date = ex$date, "Óbitos de Covid-19" = ex$new_deaths)
    # ex <- ex %>% mutate(new_deaths = new_deaths_total_2019 + new_deaths)
    plt <- ex %>% reshape2::melt(id.vars = "date")
    p <- plt %>%
      ggplot(aes(x = date, y = value, fill = variable, text = variable)) +
      geom_area() +
      scale_fill_viridis(discrete = TRUE) +
      ggtitle(paste0("Mortes em excesso no estado de ", input$state)) +
      geom_vline(aes(xintercept = as.Date("2020-03-13"))) +
      geom_vline(aes(xintercept = as.Date("2020-12-21")))
    # theme(legend.position="none")

    ggplotly(p)
  })
  
  #' @Previsoes no plot_ly
  
  # utilizando cashing para reaproveitar as contas no server
  forecast_c <- memoise(forecast)
  
  # objeto reativo que armazena o modelo utilizado
  dfit <- reactiveValues(data = NULL, xreg = NULL, title = NULL)
  
  # recebe um modelo e calcula a previsao com a confiança estipulada
  calcula_pred <- reactive({
    lwr <- input$maxScore
    upr <- input$minScore
    rng <- input$pred_rng
    fit <- dfit$data
    #xreg <- dfit$xreg
                    
    f <- forecast_c(fit, 7 * rng, PI = T, level = c(lwr/100, upr/100))#, xreg = xreg$mean)
    tmp <- autoplot(f)
    dfit$title <- tmp$labels$title
    f
  })
  
  output$title <- renderText({
    print(dfit$title)
  })
  
  output$pred_hdr <- renderText({
    if (input$radio2 == 1) lbl <- "casos"
    else lbl <- "óbitos"
    paste0("Previsão de novos ",lbl," para os próximos ",7*input$pred_rng, " dias")
  })
  
  

  # previsao por DRS
  output$predict_cases_drs <- renderPlotly({
    # tratamento do reactive
    req(input$drs)

    codi_drs <- drs_seade %>%
      filter(nome_drs == input$drs & datahora == "2021-02-14") %>%
      select(cod_drs) %>%
      pull()

    sp <- drs_seade %>% filter(cod_drs == codi_drs)

    # modelo de redes neurais
    dfit$data <- nnetar(sp$total_novos_casos, p = 7)
    # fit2 <- nnetar(x = sp$datahora, y = sp$ocupacao_leitos,p=7)

    show_modal_spinner(text = "Calculando previsão...")

    
    req(input$maxScore)
    req(input$minScore)
    req(input$pred_rng)
    
    pred <- calcula_pred()
    # constante de previsao = % ocupaçao * new_cases / pred(new_cases)

    const <- sp %>%
      last() %>%
      select(ocupacao_leitos, mm7d_casos)
    const$ocupacao_leitos <- as.numeric(const$ocupacao_leitos)
    const$total_novos_casos <- as.numeric(const$mm7d_casos)

    # constante de predicao de leitos de acordo com media movel de casos
    pred2 <- const$ocupacao_leitos * const$total_novos_casos
    # pred2 <- calcula_pred_c(fit2)

    remove_modal_spinner()

    trace1 <- list(
      line = list(
        color = "rgba(0,0,0,1)",
        fillcolor = "rgba(0,0,0,1)"
      ),
      mode = "lines",
      name = "Média móvel de casos confirmados (7 dias)",
      type = "scatter",
      x = sp$datahora,
      y = round(sp$mm7d_casos),
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
      name = "Previsão de novos casos diários (95% de confiança)",
      type = "scatter",
      x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)))),
      y = round(c(pred$upper[, 1], rev(pred$lower[, 1]))),
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
      name = "Previsão de novos casos diários (80% de confiança)",
      type = "scatter",
      x = c(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)), rev(min(sp$datahora) - ddays(1) + ddays(time(pred$mean)))),
      y = round(c(pred$upper[, 2], rev(pred$lower[, 2]))),
      xaxis = "x",
      yaxis = "y",
      hoveron = "points"
    )
    trace4 <- list(
      line = list(
        dash = 3,
        color = "rgba(0,0,255,1)",
        fillcolor = "rgba(0,0,255,1)"
      ),
      mode = "lines",
      name = "previsão de novos casos diários",
      type = "scatter",
      x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = round(pred$mean),
      xaxis = "x",
      yaxis = "y"
    )
    trace5 <- list(
      line = list(
        color = "rgba(0,155,155,1)",
        fillcolor = "rgba(0,155,155,1)"
      ),
      mode = "lines",
      name = "% leitos ocupados",
      type = "scatter",
      x = sp$datahora, # min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = sp$ocupacao_leitos %>% as.numeric(),
      xaxis = "x",
      yaxis = "y2"
    )
    trace7 <- list(
      line = list(
        dash = 3,
        color = "rgba(0,100,55,1)",
        fillcolor = "rgba(0,100,55,1)"
      ),
      mode = "lines",
      name = "previsão de internações",
      type = "scatter",
      x = min(sp$datahora) - ddays(1) + ddays(time(pred$mean)),
      y = pred2 / frollmean(pred$mean, 7, fill = const$mm7d_casos),
      xaxis = "x",
      yaxis = "y2"
    )
    data <- list(trace1, trace2, trace3, trace4, trace5, trace7)
    layout <- list(
      title = paste0(input$drs),
      xaxis = list(
        # title = "Data",
        domain = range(sp$datahora)
      ),
      yaxis = list(
        title = "Novos Casos Confirmados",
        domain = c(0, 1)
      ),
      yaxis2 = list(
        title = "% Leitos ocupados",
        domain = c(0, 1),
        overlaying = "y",
        automargin = TRUE,
        side = "right"
      ),
      margin = list(
        b = 40,
        l = 60,
        r = 10,
        t = 25
      )
    )
    p <- plot_ly()
    p <- add_trace(p, line = trace1$line, mode = trace1$mode, name = trace1$name, type = trace1$type, x = trace1$x, y = trace1$y, xaxis = trace1$xaxis, yaxis = trace1$yaxis)
    p <- add_trace(p, fill = trace2$fill, line = trace2$line, mode = trace2$mode, name = trace2$name, type = trace2$type, x = trace2$x, y = trace2$y, xaxis = trace2$xaxis, yaxis = trace2$yaxis, hoveron = trace2$hoveron)
    p <- add_trace(p, fill = trace3$fill, line = trace3$line, mode = trace3$mode, name = trace3$name, type = trace3$type, x = trace3$x, y = trace3$y, xaxis = trace3$xaxis, yaxis = trace3$yaxis, hoveron = trace3$hoveron)
    p <- add_trace(p, line = trace4$line, mode = trace4$mode, name = trace4$name, type = trace4$type, x = trace4$x, y = trace4$y, xaxis = trace4$xaxis, yaxis = trace4$yaxis)
    p <- add_bars(p, y = sp$total_novos_casos, x = sp$datahora, name = "Novos casos diários")
    p <- add_trace(p, line = trace5$line, mode = trace5$mode, name = trace5$name, type = trace5$type, x = trace5$x, y = trace5$y, xaxis = trace5$xaxis, yaxis = trace5$yaxis)
    p <- add_trace(p, line = trace7$line, mode = trace7$mode, name = trace7$name, type = trace7$type, x = trace7$x, y = trace7$y, xaxis = trace7$xaxis, yaxis = trace7$yaxis)
    p <- add_segments(p, yaxis = "y2", name = "100% de ocupação", x = as_date("2020-12-31"), xend = as_date("2021-04-15"), y = 100, yend = 100, line = list(dash = 6))
    p <- layout(p, title = layout$title, xaxis = layout$xaxis, yaxis = layout$yaxis, yaxis2 = layout$yaxis2, margin = layout$margin, legend = list(orientation = "h"))
  })

  # previsao por estado e municipio
  output$predict_cases <- renderPlotly({
    show_modal_spinner(text = "Calculando previsão...") # loading bar
    
    # cidade estado regiao ou br
    if (input$radio1 == 1) {
      req(input$chooseCity)
      selectedCity <- dt %>% filter(state == input$state &
        city == input$chooseCity &
        place_type == "city")
      title_g <- paste0(input$chooseCity, " - ", input$state)
    }
    else if (input$radio1 == 0) { #estado
      req(input$state)
      selectedCity <- dt %>% filter(state == input$state &
        place_type == "state")
      title_g <- paste0("Estado de ", input$state)
    }
    else if (input$radio1 == 2) { #regiao
      req(input$region)
      selectedCity <- dados_regioes %>% filter(regiao == input$region)
      title_g <- paste0("Região ",input$region)
    }
    else if (input$radio1 == 3) { #brasil todo
      selectedCity <- dados_regioes %>% filter(regiao == "BRASIL")
      title_g <- paste0("Brasil")
    }
    
      
      if (input$radio2 == 1){ #casos
      # media movel
        mm <- selectedCity$mm7d_confirmed
        mdlvar <- selectedCity$new_confirmed
        lbl <- "casos"
      }
      else{ # ou obitos
        mm <- selectedCity$mm7d_deaths
        mdlvar <- selectedCity$new_deaths
        lbl <- "óbitos"
      }
      # tipo de modelagem a ser adotado
      if(input$radio3 == 1){ # redes neurais
        rng <- input$pred_rng
        ts_mdl <- xts::xts(x = mdlvar, order.by = selectedCity$date, frequency = 7) 
        #xreg = auto.arima(ts_mdl, approximation=FALSE)
        dfit$data <- nnetar(ts_mdl, p = 7)#, xreg = xreg$fitted)
        #dfit$xreg <- forecast(xreg, 7 * rng)
      }
      else{ # arima
        rng <- input$pred_rng
        ts_mdl <- xts::xts(x = mdlvar, order.by = selectedCity$date, frequency = 7)
        #xreg <- nnetar(ts_mdl, p = 7) 
        dfit$data <- auto.arima(ts_mdl, approximation=FALSE)#, xreg = xreg$fitted)
        #dfit$xreg <- forecast(xreg, 7 * rng)
      }
      
      
      # intervalos de confiança para a predição
      pred <- calcula_pred()
      
      remove_modal_spinner() # remove a barra de carregamento
      # browser()
      
      trace1 <- list(
        line = list(
          color = "rgba(0,0,0,1)",
          fillcolor = "rgba(0,0,0,1)"
        ),
        mode = "lines",
        name = paste0("Média móvel de ",lbl," (7 dias)"),
        type = "scatter",
        x = selectedCity$date,
        y = round(mm),
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
        name = paste0("Previsão de novos ",lbl," diários (",input$maxScore,"% de confiança)"),
        type = "scatter",
        x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)))),
        y = round(c(pred$upper[, 1], rev(pred$lower[, 1]))),
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
        name = paste0("Previsão de novos ", lbl," diários (",input$minScore,"% de confiança)"),
        type = "scatter",
        x = c(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)), rev(min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)))),
        y = round(c(pred$upper[, 2], rev(pred$lower[, 2]))),
        xaxis = "x",
        yaxis = "y",
        hoveron = "points"
      )
      trace4 <- list(
        line = list(
          dash = 3,
          color = "rgba(0,0,255,1)",
          fillcolor = "rgba(0,0,255,1)"
        ),
        mode = "lines",
        name = paste0("previsão de novos ", lbl," diários"),
        type = "scatter",
        x = min(selectedCity$date) - ddays(1) + ddays(time(pred$mean)),
        y = round(pred$mean),
        xaxis = "x",
        yaxis = "y"
      )
      data <- list(trace1, trace2, trace3, trace4)
      layout <- list(
        title = title_g,
        xaxis = list(
          # title = "Data",
          domain = range(selectedCity$date)
        ),
        yaxis = list(
          title = paste0("Novos ", lbl," diários"),
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
      p <- add_trace(p, line = trace1$line, mode = trace1$mode, name = trace1$name, type = trace1$type, x = trace1$x, y = trace1$y, xaxis = trace1$xaxis, yaxis = trace1$yaxis)
      p <- add_trace(p, fill = trace2$fill, line = trace2$line, mode = trace2$mode, name = trace2$name, type = trace2$type, x = trace2$x, y = trace2$y, xaxis = trace2$xaxis, yaxis = trace2$yaxis, hoveron = trace2$hoveron)
      p <- add_trace(p, fill = trace3$fill, line = trace3$line, mode = trace3$mode, name = trace3$name, type = trace3$type, x = trace3$x, y = trace3$y, xaxis = trace3$xaxis, yaxis = trace3$yaxis, hoveron = trace3$hoveron)
      p <- add_trace(p, line = trace4$line, mode = trace4$mode, name = trace4$name, type = trace4$type, x = trace4$x, y = trace4$y, xaxis = trace4$xaxis, yaxis = trace4$yaxis)
      p %>% add_bars(x = ~date, y = ~ mdlvar)
      p <- layout(p, title = layout$title, xaxis = layout$xaxis, yaxis = layout$yaxis, margin = layout$margin, legend = list(orientation = "h"))
      p %>% add_bars(y = mdlvar, x = selectedCity$date, name = paste0("Novos ",lbl," diários"))    
        

  })

  # plot dinamico com a media movel.
  output$distPlot <- renderPlotly({
    # expressão do reactive para tratamento do input
    # req(input$chooseCity)
    req(input$state)
    req(input$plotType)

    # seleciona cidade ou estado
    if (input$radio1 == 1) {
      selectedCity <- dt %>% filter(state == input$state &
        city == input$chooseCity &
        place_type == "city")
    } else if (input$radio1 == 0) {
      selectedCity <- dt %>% filter(state == input$state &
        place_type == "state")
    }

    # input
    selectedvar <- selectedCity %>%
      select(input$plotType) %>%
      pull()

    if (input$plotType %in% c("new_confirmed", "new_deaths")) {
      p <- selectedCity %>%
        plot_ly() %>%
        add_lines(x = ~date, y = ~ frollmean(selectedvar, 7)) %>%
        add_bars(x = ~date, y = ~selectedvar) %>%
        config(displayModeBar = F) %>%
        hide_legend()
    }
    else {
      p <- selectedCity %>%
        plot_ly() %>%
        add_lines(x = ~date, y = ~selectedvar) %>%
        config(displayModeBar = F) %>%
        hide_legend()
    }
    
    # label da ultima att
    output$last_update <- renderText({
      req(input$state)
      
      data <- dt %>% filter(state == input$state &
                              place_type == "state" & is_last == "True")
      paste0("Atualizado em ", as.Date(data$date))
    })
    # label da ultima att DRS
    output$last_update_drs <- renderText({
      req(input$drs)
      
      data <- drs_seade %>%
        filter(nome_drs == input$drs) %>%
        arrange(datahora) %>%
        last()
      paste0("Atualizado em ", as.Date(data$datahora))
    })
    
    
    
    # label do numero de casos
    output$case_count <- renderText({
      if (input$radio1 == 1) {
        req(input$chooseCity)
        total_casos <- dt %>% filter(state == input$state &
                                       city == input$chooseCity &
                                       place_type == "city" & is_last == "True")
      }
      else if (input$radio1 == 0) {
        req(input$state)
        total_casos <- dt %>% filter(state == input$state &
                                       place_type == "state" & is_last == "True")
      }
      paste0(prettyNum(total_casos$last_available_confirmed, big.mark = ".", decimal.mark = ","), " casos\n")
    })
    
    # labels de texto dos gráficos
    # label do numero de mortes
    output$deaths_count <- renderText({
      if (input$radio1 == 1) {
        req(input$chooseCity)
        total_casos <- dt %>% filter(state == input$state &
                                       city == input$chooseCity &
                                       place_type == "city" & is_last == "True")
      }
      else if (input$radio1 == 0) {
        req(input$state)
        total_casos <- dt %>% filter(state == input$state &
                                       place_type == "state" & is_last == "True")
      }
      paste0(prettyNum(total_casos$last_available_deaths, big.mark = ".", decimal.mark = ","), " óbitos\n")
    })
    # label do numero de novos casos
    output$new_cases_count <- renderText({
      if (input$radio1 == 1) {
        req(input$chooseCity)
        total_casos <- dt %>% filter(state == input$state &
                                       city == input$chooseCity &
                                       place_type == "city" & is_last == "True")
      }
      else if (input$radio1 == 0) {
        req(input$state)
        total_casos <- dt %>% filter(state == input$state &
                                       place_type == "state" & is_last == "True")
      }
      paste0(prettyNum(total_casos$new_confirmed, big.mark = ".", decimal.mark = ","), " novos casos\n")
    })
    # label do numero de novas mortes
    output$new_deaths_count <- renderText({
      if (input$radio1 == 1) {
        req(input$chooseCity)
        total_casos <- dt %>% filter(state == input$state &
                                       city == input$chooseCity &
                                       place_type == "city" & is_last == "True")
      }
      else if (input$radio1 == 0) {
        req(input$state)
        total_casos <- dt %>% filter(state == input$state &
                                       place_type == "state" & is_last == "True")
      }
      paste0(prettyNum(total_casos$new_deaths, big.mark = ".", decimal.mark = ","), " novos óbitos\n")
    })
    # label da taxa de letalidade
    output$letality_count <- renderText({
      if (input$radio1 == 1) {
        req(input$chooseCity)
        total_casos <- dt %>% filter(state == input$state &
                                       city == input$chooseCity &
                                       place_type == "city" & is_last == "True")
      }
      else if (input$radio1 == 0) {
        req(input$state)
        total_casos <- dt %>% filter(state == input$state &
                                       place_type == "state" & is_last == "True")
      }
      paste0(scales::percent(total_casos$last_available_death_rate), " letalidade\n")
    })
    
  })
  
  #' @Tabela (data explorer)
  observe({
    cities <- if (is.null(input$states)) {
      character(0)
    } else {
      filter(cleantable, state %in% input$states) %>%
        `$`("city") %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities",
      choices = cities,
      selected = stillSelected, server = TRUE
    )
  })

    observe({
    if (is.null(input$goto)) {
      return()
    }
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
        # Score >= input$minScore,
        # Score <= input$maxScore,
        is.null(input$states) | state %in% input$states,
        is.null(input$cities) | city %in% input$cities # ,
        # is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate("Ir ao mapa" = paste('<a class="go-map" href="" data-lat="', latitude, '" data-long="', longitude, '" data-city="', city_ibge_code, '"><i class="fa fa-crosshairs"></i></a>', sep = ""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")


    colnames(df) <- c("Estado", "Cidade", "Populacão Estimada 2019", "Total de Casos", "Total de Mortes", "Taxa de Letalidade", "Latitude", "Longitude", "Código IBGE", "Ir ao Mapa")
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
# browser()
shinyApp(ui = ui, server = server)

# enviar site
# rsconnect::deployApp(account = "predict-icmc")
