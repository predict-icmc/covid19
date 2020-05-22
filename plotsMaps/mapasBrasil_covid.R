##### 
##### https://dataficacao.wordpress.com/2017/02/21/criando-mapa-brasil-r/
#####

library(rgdal)
library(RColorBrewer)
library(leaflet)
setwd("/home/louzeiro/Documentos/USP/predict/covid19/plotsMaps/")
mapa <- readOGR("2015/br_unidades_da_federacao/BRUFE250GC_SIR.shp", stringsAsFactors=FALSE, encoding="UTF-8")
pg <- read.csv("mortesCovid.csv", sep = ";")
pg <- as.data.frame(pg)
head(pg)

brasileiroHab <- merge(mapa,pg, by.x = "CD_GEOCUF", by.y = "cod_cid_ibge")
proj4string(brasileiroHab) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(brasileiroHab$NM_ESTADO) <- "UTF-8"
brasileiroHab$obitos[is.na(brasileiroHab$obitos)] <- 0

pal <- colorBin("Green",domain = NULL,n=5) 

state_popup <- paste0("<strong>Estado: </strong>", 
                      brasileiroHab$NM_ESTADO, 
                      "<br><strong>Total Óbitos: </strong>", 
                      brasileiroHab$obitos)
leaflet(data = brasileiroHab) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(brasileiroHab$obitos), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~brasileiroHab$hab_km_quad,
            title = "Total Obitos",
            opacity = 1)

#################### segundo tutorial
## http://rstudio-pubs-static.s3.amazonaws.com/334254_db4a393ba750464fa36349f2a2991b6a.html
#####

library(sp)
n.cat = 8

#Cores utilizadas para cada categoria
#cores = c("#03E06EFF", "#38FA2AFF", "#9FFF15FF", "#DEFE0BFF", "#F3FB06FF", "#FFCD00FF", "#FF8500FF", "#FF3300FF")
cores = c("#8B0000","#B22222","#A52A2A","#FA8072","#E9967A","#FFA07A","#FF7F50","#FF6347")
intervalos = quantile(brasileiroHab$obitos, probs = seq(0,1,0.125)) + c(-0.001,rep(0,7),0.001)
spplot(brasileiroHab,c("obitos"),
       at=intervalos,
       ylab = "Total Acumulado Óbitos",
       col.regions =brewer.pal(8, "Reds")) #Outras opções de cores: Greens, BrBG, Accent


