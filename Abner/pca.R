library(scales)
library(ElemStatLearn)
library(kableExtra)
library(nnet)
library(devtools)
library(ggbiplot)

## Função nova #################################################################
library(tidyverse)
require(psych)
#selecionando variaveis
df.pca<-last.data %>% 
  select(IDH,gini,t_agua,rdpc,densidade) %>% 
  na.omit()
#Transformando para numerico
df.pca<-apply(df.pca,2,as.numeric)
#aplicando o metodo de componentes principais
KMO(df.pca)
fit<-princomp(df.pca,cor=TRUE)
summary(fit)
fit$loadings

#########################################################################################
#Função antiga
summary(bdcorr)

#Selecionando variaveis
df.pca<-bdcorrc %>% 
  select(IDH,gini,t_agua,rdpc,densidade,last_available_confirmed) %>% 
  na.omit()

#names(df.pca)[names(df.pca) == "last_available_confirmed_per_100k_inhabitants"] <- "ultimo_caso_100k"

df.pca<-data.frame(lapply(df.pca,as.double))

#df.pca<-data.frame(apply(df.pca ,MARGIN = 2,FUN = rescale))
#Aplicando componentes princiapais
pca.result<-prcomp(df.pca[,1:5],scale. = T)

#Analisando os desvioes padroes
R<-fit$sdev
R
v1<- round(R^2 / sum(R^2), 3) #proporção da variância explicada
v2<- cumsum((R^2 / sum(R^2)))
kable(data.frame(paste("CP", 1: 5), v1, v2),
      col.names = c("CP","Proporção Variância Explicada", "Acumulada"),
      align = "c") %>% kable_styling(position = "center", font_size = 12)


par(mfrow=c(1,2))
aux<- R^2 / sum(R^2)
plot(aux, type="b", pch=16, main="Scree plot", ylab="Proporção da Variância Explicada",
     xlab="ordem da componente")
axis(1, at= 11, labels=11, col.axis="blue")
plot(cumsum( R^2 / sum( (R^2))), type="b", pch=16,
     main="Scree plot", ylab="Acumulada", xlab="ordem da componente")
abline(h = 0.9, v = 11, col=c("red","blue"), lwd=2, lty= 3)
axis(1, at= 6, labels=6, col.axis="blue")

#pegando os dados transformados
pca.dados<-data.frame(pca.result$x)
pca.result$x

ggbiplot(pca.result)

ggbiplot(pca.result,ellipse=TRUE)

#################################################################################

