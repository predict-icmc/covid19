library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

vars <- c(
  "Total Confirmados" = "last_available_confirmed",
  "Total Mortos" = "last_available_death",
  "Populacão" = "estimated_population_2019",
  "Novos Confirmados" = "new_confirmed",
  "Novos Mortos" = "new_death"
)


ui <- fluidPage(
  
# Choices for drop-downs


navbarPage("Superzip", id="nav",
           
           tabPanel("Interactive map",
                    div(class="outer",
                        
                        tags$head(
                          # Include our custom CSS
                          #includeCSS("styles.css"),
                          #includeScript("gomap.js")
                        ),
                        
                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      
                                      h2("ZIP explorer"),
                                      
                                      selectInput("color", "Color", vars),
                                      selectInput("size", "Size", vars, selected = "last_available_confirmed")#,
                                      #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                       # Only prompt for threshold when coloring or sizing by superzip
                                      #                 numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                      ),
                                      
                                      plotOutput("histCentile", height = 200),
                                      plotOutput("scatterCollegeIncome", height = 250)
                        ),
                        
                        tags$div(id="cite",
                                 'Data compiled for ', tags$em('Coming Apart: The State of White America, 1960–2010'), ' by Charles Murray (Crown Forum, 2012).'
                        )
                    )
           ),
           
           tabPanel("Data explorer",
                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           ),
#
          conditionalPanel("false", icon("crosshair"))
)



# PEGA OS DADOS DO COVID

dados1<-read.csv(file = "2020-07-31_caso_full.csv",header=TRUE)
dados1$tempo<- as.numeric(as.Date(dados1$date) - min(as.Date(dados1$date)))

latlong_cidade<-read.csv('latitude-longitude-cidades.csv', sep=';', header=TRUE)
latlong_cidade$city <-latlong_cidade$municipio
latlong_cidade$state <-latlong_cidade$uf
, "Washington, DC"="DC"
latlong_estado<-read.csv('latitude-longitude-estados.csv', sep=';', header=TRUE)
latlong_estado$state <-latlong_estado$uf
latlong_estado$place_type='state'

# Juntando as bases de dados
dados2 <- merge(dados1,latlong_cidade,by=c('state','city'), all.x=TRUE, all.y=FALSE)
dados <- merge(dados2,latlong_estado,by=c('state','place_type'), all.x=TRUE, all.y=FALSE)

dados <-dados %>%
  mutate(latitude = ifelse(place_type=='city', latitude.x, latitude.y),
         longitude = ifelse(place_type=='city',longitude.x, longitude.y)) 
dados <-select(dados, -c('uf.x','uf.y','latitude.x','longitude.x','latitude.y','longitude.y'))

zipdata <- dados


# zipcode,centile,superzip,rank,city,state,adultpop,households,college,income

server <- function(input, output, session) {

    ## Interactive Map ###########################################
    
    # Create the map
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles(
          urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
          attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
        ) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4) # colocar lat e long do br
    })
    
    # A reactive expression that returns the set of zips that are
    # in bounds right now
    zipsInBounds <- reactive({
      if (is.null(input$map_bounds))
        return(zipdata[FALSE,])
      bounds <- input$map_bounds
      latRng <- range(bounds$north, bounds$south)
      lngRng <- range(bounds$east, bounds$west)
      
      subset(zipdata,
             latitude >= latRng[1] & latitude <= latRng[2] &
               longitude >= lngRng[1] & longitude <= lngRng[2])
    })
    
    # Precalculate the breaks we'll need for the two histograms
    centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
    
    output$histCentile <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      hist(zipsInBounds()$centile,
           breaks = centileBreaks,
           main = "SuperZIP score (visible zips)",
           xlab = "Percentile",
           xlim = range(allzips$centile),
           col = '#00DD00',
           border = 'white')
    })
    
    output$scatterCollegeIncome <- renderPlot({
      # If no zipcodes are in view, don't plot
      if (nrow(zipsInBounds()) == 0)
        return(NULL)
      
      print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
    })
    
    # This observer is responsible for maintaining the circles and legend,
    # according to the variables the user has chosen to map to color and size.
    observe({
      colorBy <- input$color
      sizeBy <- input$size
      
      if (colorBy == "superzip") {
        # Color and palette are treated specially in the "superzip" case, because
        # the values are categorical instead of continuous.
        colorData <- ifelse(zipdata$centile >= (100 - input$threshold), "yes", "no")
        pal <- colorFactor("viridis", colorData)
      } else {
        colorData <- zipdata[[colorBy]]
        pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
      }
      
      if (sizeBy == "superzip") {
        # Radius is treated specially in the "superzip" case.
        radius <- ifelse(zipdata$centile >= (100 - input$threshold), 30000, 3000)
      } else {
        radius <- zipdata[[sizeBy]] / max(zipdata[[sizeBy]]) * 30000
      }
      
      leafletProxy("map", data = zipdata) %>%
        clearShapes() %>%
        addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                   stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                  layerId="colorLegend")
    })
    
    # Show a popup at the given location
    showZipcodePopup <- function(zipcode, lat, lng) {
      selectedZip <- allzips[allzips$zipcode == zipcode,]
      content <- as.character(tagList(
        tags$h4("Score:", as.integer(selectedZip$centile)),
        tags$strong(HTML(sprintf("%s, %s %s",
                                 selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
        ))), tags$br(),
        sprintf("Median household income: %s", dollar(selectedZip$income * 1000)), tags$br(),
        sprintf("Percent of adults with BA: %s%%", as.integer(selectedZip$college)), tags$br(),
        sprintf("Adult population: %s", selectedZip$adultpop)
      ))
      leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
    }
    
    # When map is clicked, show a popup with city info
    observe({
      leafletProxy("map") %>% clearPopups()
      event <- input$map_shape_click
      if (is.null(event))
        return()
      
      isolate({
        showZipcodePopup(event$id, event$lat, event$lng)
      })
    })
    
    
    ## Data Explorer ###########################################
    
    observe({
      cities <- if (is.null(input$states)) character(0) else {
        filter(cleantable, State %in% input$states) %>%
          `$`('City') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$cities[input$cities %in% cities])
      updateSelectizeInput(session, "cities", choices = cities,
                           selected = stillSelected, server = TRUE)
    })
    
    observe({
      zipcodes <- if (is.null(input$states)) character(0) else {
        cleantable %>%
          filter(State %in% input$states,
                 is.null(input$cities) | City %in% input$cities) %>%
          `$`('Zipcode') %>%
          unique() %>%
          sort()
      }
      stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
      updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                           selected = stillSelected, server = TRUE)
    })
    
    observe({
      if (is.null(input$goto))
        return()
      isolate({
        map <- leafletProxy("map")
        map %>% clearPopups()
        dist <- 0.5
        zip <- input$goto$zip
        lat <- input$goto$lat
        lng <- input$goto$lng
        showZipcodePopup(zip, lat, lng)
        map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
      })
    })
    
    output$ziptable <- DT::renderDataTable({
      df <- cleantable %>%
        filter(
          Score >= input$minScore,
          Score <= input$maxScore,
          is.null(input$states) | State %in% input$states,
          is.null(input$cities) | City %in% input$cities,
          is.null(input$zipcodes) | Zipcode %in% input$zipcodes
        ) %>%
        mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
      action <- DT::dataTableAjax(session, df, outputId = "ziptable")
      
      DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
    })
  }


shinyApp(ui = ui, server = server)