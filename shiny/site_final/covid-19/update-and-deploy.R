#source("merge-data.R")
#pegaCorona()
#baixar_seade()
rsconnect::deployApp(account = "predict-icmc", forceUpdate = T)

