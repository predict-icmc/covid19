library(shiny)
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

vars <- c(
  "Total de Casos Confirmados" = "last_available_confirmed",
  "Total de Óbitos" = "last_available_deaths",
  "Letalidade" = "last_available_death_rate",
  "Populacão Estimada 2019" = "estimated_population_2019",
  "Novos Casos Confirmados" = "new_confirmed",
  "Novos Óbitos" = "new_deaths",
  "Confirmados / 100 mil habitantes" = "last_available_confirmed_per_100k_inhabitants"
)
dados<-read.csv(file = "2020-07-22 _merge-covid.csv",header=TRUE)
dados <- dados %>% drop_na(latitude,longitude)

ui <- fluidPage(
  
  
  navbarPage("PREDICT - Covid-19", id="nav",
                    
             tabPanel("Mapa Interativo",
                      div(class="outer",
                          tags$head(
                             #Include our custom CSS
                            includeCSS("styles.css"),
                            #includeScript("gomap.js")
                          ),
                          
                          leafletOutput("map", width="100%", height="100%"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, height = "auto",
                                        
                                        h2("Mapa interativo do Covid-19"),
                                        
                                        radioButtons("radio", h3("Listar por"),
                                                     choices = list("Cidades" = 1, "Estados" = 0),
                                                     selected = 1),
                                        
                                        selectInput("color", "Variável:", vars, selected = "last_available_confirmed"),
                                        #selectInput("size", "Size", vars, selected = "last_available_death_rate")#,
                                        
                                        #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                        # Only prompt for threshold when coloring or sizing by superzip
                                        #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                          ),
                          
                          #plotOutput("histCentile", height = 200),
                          #plotOutput("scatterCollegeIncome", height = 250)
                      ),
                      
                      tags$div(id="cite",tags$img(src = "logo-predict-small1.jpg"))
                               
                      #         'Desenvolvido pelo grupo', tags$em('PREDICT-Covid19'),
                      #)
                          
             )
             )
  )


server <- function(input, output, session) {
  
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
      pal <- colorBin("viridis", colorData, 5, pretty = FALSE)
    
    
      radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 100
    
      leafletProxy("map", data = zipdata) %>%
      clearMarkers() %>%
      addCircleMarkers(~longitude, ~latitude, radius=radius, layerId=~city_ibge_code,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
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
      currcity <- event$id
      
    })
  })
  
}
shinyApp(ui = ui, server = server)
                      