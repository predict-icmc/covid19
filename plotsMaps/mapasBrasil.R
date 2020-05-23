##### 
##### https://dataficacao.wordpress.com/2017/02/21/criando-mapa-brasil-r/
#####

library(rgdal)
library(RColorBrewer)
library(leaflet)
mapa <- readOGR("Mapas/estados2015/BRUFE250GC_SIR.shp", stringsAsFactors=FALSE, encoding="UTF-8")

pg <- read.csv("mortesCovid.csv", sep = ";")
pg <- as.data.frame(pg)

mapEstados <- merge(mapa,pg, by.x = "CD_GEOCUF", by.y = "cod_cid_ibge")

proj4string(mapEstados) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(mapEstados$NM_ESTADO) <- "UTF-8"
mapEstados$cod_cid_ibge[is.na(mapEstados$obitos)] <- 0

pal <- colorBin("Reds",domain = NULL,n=5) 

state_popup <- paste0("<strong>Estado: </strong>", 
                      mapEstados$NM_ESTADO, 
                      "<br><strong>Habitantes por Km Quadrado: </strong>", 
                      mapEstados$obitos)
leaflet(data = mapEstados) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(mapEstados$obitos), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~mapEstados$obitos,
            title = "Óbitos Covid-19",
            opacity = 1)

#################### segundo tutorial
## http://rstudio-pubs-static.s3.amazonaws.com/334254_db4a393ba750464fa36349f2a2991b6a.html
#####

library(sp)
n.cat = 8

#Cores utilizadas para cada categoria
cores = c("#03E06EFF", "#38FA2AFF", "#9FFF15FF", "#DEFE0BFF", "#F3FB06FF", "#FFCD00FF", "#FF8500FF", "#FF3300FF")
intervalos = quantile(mapEstados$cod_cid_ibge, probs = seq(0,1,0.125)) + c(-0.001,rep(0,7),0.001)
spplot(mapEstados,c("cod_cid_ibge"),
       at=intervalos,
       ylab = "Densidade demográfica",
       col.regions =brewer.pal(8, "Greens")) #Outras opções de cores: Greens, BrBG, Accent


