library(tidyverse)
#setwd("~/Documentos/USP/predict/covid19")
#------- Leitura dos dados
dados<-read.csv("https://brasil.io/dataset/covid19/caso?format=csv",header=TRUE)
dados$tempo<- as.numeric(as.Date(dados$date) - min(as.Date(dados$date)))
dados = as.data.frame(dados)

#------- Tramento necessario pois a base de dados (dados), contem muitas linhas sem o nome da cidade
baseCodCidadesIBGE <- read.csv("plotsMaps/cod_ibge_cidades.csv")
codCidades = baseCodCidadesIBGE$Código.Município.Completo ## cod cidades IBGE
munCovid = as.data.frame(matrix(0,nrow = nrow(baseCodCidadesIBGE),ncol = ncol(dados))) ## Ultima atualização de cada municipio
names(munCovid)<-c(names(dados)) ## usando os mesmos nomes dos dados
for (i in 1:length(codCidades)){
  aux <- dados[which(dados$city_ibge_code==codCidades[i]),] # verificando cada cidade
  munCovid[i,]=aux[1,] # salvando a ultima atualização da cidade
}
munCovid = munCovid %>% drop_na() # removendo as linha com NA 
# Separando apenas as cidades que possuem casos de óbitos
naoNulos = which(munCovid$deaths != 0)
munCovid = munCovid[naoNulos,]

#--------- Plot Balloon
#--------- separando as cidades com mais de 300 óbitos
maisDe = 300 # escolha o numero de óbitos
amostra10 = subset(munCovid, munCovid$deaths > maisDe) 
amostra10 %>% ggplot(aes(x = confirmed, 
                         y = estimated_population_2019, 
                         size = deaths)) +
  geom_text(aes(label=city, vjust = -1))+
  geom_point(fill = "cornsilk", shape = 21)+
  scale_size(range = c(.1, 6))+
  expand_limits(xend=max(amostra10$confirmed)+1000)


library(rgdal) 
#---- Observação: acessar o drive e baixar a pasta Mapas, o arquivo BRMUE250GC_SIR.shp 
#----             passava de 100MB e o github não permitiu upá-lo
mapa <- readOGR("Mapas/municipios2016/BRMUE250GC_SIR.shp", stringsAsFactors=FALSE, encoding="UTF-8")
municipiosBR <- merge(mapa,munCovid, by.x = "CD_GEOCMU", by.y = "city_ibge_code")
proj4string(municipiosBR) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
Encoding(municipiosBR$NM_MUNICIP) <- "UTF-8"
municipiosBR$deaths[is.na(municipiosBR$deaths)] <- 0 ## inserindo 0 nas posiçoes NA

############### Grafico Estático
library(sp)
library(RColorBrewer)
## o vetor intervalo ajuda na escala de cores
#intervalos = c(0,10,20,30,40,100,1000,3500)
intervalos = c(0,10,100,200,500,1000,3000)
mapaEstatico = spplot(municipiosBR,c("deaths"),
                      at=intervalos,
                      ylab = "Total Acumulado Óbitos",
                      col.regions = brewer.pal(7, "Reds")) #Outras opções de cores: Greens, BrBG, Accent

mapaEstatico

#----------- Grafico iterativo
library(leaflet)
pal <- colorBin("Reds",domain = NULL,n=5) 

state_popup <- paste0("<strong>Estado: </strong>", 
                      municipiosBR$NM_MUNICIP, 
                      "<br><strong>Habitantes por Km Quadrado: </strong>", 
                      municipiosBR$deaths)

leaflet(data = municipiosBR) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(fillColor = ~pal(municipiosBR$deaths), 
              fillOpacity = 0.8, 
              color = "#BDBDC3", 
              weight = 1, 
              popup = state_popup) %>%
  addLegend("bottomright", pal = pal, values = ~municipiosBR$deaths,
            title = "Óbitos Covid-19",
            opacity = 1)
