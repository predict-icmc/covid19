##install.packages('leaflet', dependencies = TRUE)
##install.packages('rgdal', dependencies = TRUE)

library(rgdal) 
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(tibble)

setwd("/home/louzeiro/Documentos/USP/predict/covid19/plotsMaps/")
mapa <- readOGR("Mapas/municipios2016/BRMUE250GC_SIR.shp", stringsAsFactors=FALSE, encoding="UTF-8")
#View(mapa@data$CD_GEOCMU)

###### base dados PNUD
#devtools::install_github("abjur/abjData")  # importando a base PNUD
library(abjData)                           # carregando a base PNUD

### criando o DF com os IDHs de 2010
### idhm_e - IDH municipal - educação.
### idhm_l - IDH municipal - longevidade.
### idhm_r - IDH municipal - renda.

tabMun = pnud_muni %>%
  select(ano,municipio, uf, codmun7, starts_with('idhm')) %>%
  filter(ano=="2010")
pg = as.data.frame(tabMun)

### codmun7 é o cod cidades IBGE
mapaBR <- merge(mapa,pg, by.x = "CD_GEOCMU", by.y = "codmun7")
proj4string(mapaBR) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(mapaBR$NM_MUNICIP) <- "UTF-8"
mapaBR$idhm_r[is.na(mapaBR$idhm_r)] <- 0


### Mapa Iterativo ####
pal <- colorBin("Greens",domain = NULL,n=5) 
state_popup <- paste0("<strong>Cidade: </strong>", 
                      mapaBR$NM_MUNICIP, 
                      "<br><strong>IDH Renda: </strong>", 
                      mapaBR$idhm_r)
mapaLeaflet = leaflet(data = mapaBR) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(mapaBR$idhm_r), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~mapaBR$idhm_r,
            title = "IDH Renda",
            opacity = 1)

mapaLeaflet

#################### segundo tutorial
## http://rstudio-pubs-static.s3.amazonaws.com/334254_db4a393ba750464fa36349f2a2991b6a.html
#####

####### Mapa Estático
library(sp)
n.cat = 8 # num categorias
#Cores utilizadas para cada categoria
cores = c("#03E06EFF", "#38FA2AFF", "#9FFF15FF", "#DEFE0BFF", "#F3FB06FF", "#FFCD00FF", "#FF8500FF", "#FF3300FF")
intervalos = quantile(mapaBR$idhm_r, probs = seq(0,1,0.125)) + c(-0.001,rep(0,7),0.001)
mapaEstatico = spplot(mapaBR,c("idhm_r"),
       at=intervalos,
       ylab = "IDH Renda",
       col.regions =brewer.pal(8, "Greens")) #Outras opções de cores: Greens, BrBG, Accent

mapaEstatico
