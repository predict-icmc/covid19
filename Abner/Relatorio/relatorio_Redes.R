library(devtools)
unload(tidyverse)
detach(package:tidyverse)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')

model <- nnetar(redes$new_confirmed,p=7)
plot.nnet(model)

grnn_weights(model)
grnn_weights(model)
model<-grnn_forecasting(redes$new_confirmed, h = 4)
model<-grnn_forecasting(redes$new_confirmed, h = 2, lags =7)
plot(model)

plot.nnet()

plot(model)

plot.nnet(model)

library(nnet)

#import the function from Github
library(devtools)
source_url('https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r')

#plot each model
plot.nnet(model)
plot.nnet(mod2)
plot.nnet(mod3)


library(NeuralNetTools)
plotnet(rep(0,46),struct=c(7,5,1))
