# Covid-19 excess deaths
# obitos em comparação com 2019
# doc do dataset: https://github.com/turicas/covid19-br/blob/master/api.md#%C3%B3bitos-registrados-em-cart%C3%B3rio
source("load-data.R")
#input$state
aq <- cart %>% filter(state == "SP") %>%
  mutate(
    delta_respiratory_failure = new_deaths_respiratory_failure_2020 - new_deaths_respiratory_failure_2019,
    delta_indeterminate = deaths_indeterminate_2020 - deaths_indeterminate_2019,
    delta_others = deaths_others_2020 - deaths_others_2019,
    delta_pneumonia = deaths_pneumonia_2020 - deaths_pneumonia_2019,
    delta_septicemia = deaths_septicemia_2020 - deaths_septicemia_2019,
    delta_sars = deaths_sars_2020 - deaths_sars_2019) %>%
  select(date,delta_respiratory_failure,delta_indeterminate,delta_others,delta_pneumonia,delta_pneumonia,delta_septicemia,delta_sars)

inputstate <- "SP"

aq$date <- as.Date(aq$date)
aq <- aq %>% filter(date< as.POSIXct("2020-09-30"))
aw <- reshape2::melt(aq,id.vars = "date") 

covdeaths <- dt %>% filter(city_ibge_code == 35) %>% select(date,new_deaths)
ex <- cart %>% filter(state == inputstate) %>%  select(new_deaths_total_2020,new_deaths_total_2019,date)
ex$date <- ex$date %>% as.Date()

ex <- left_join(ex,covdeaths, by = c("date" = "date")) 
dt <- tibble("Variação 2019-2020" = ex$new_deaths_total_2020 - ex$new_deaths_total_2019, date = ex$date, "Mortes em decorrência do Covid-19" = ex$new_deaths)
# trocando os NA's por zero
ex[is.na(ex)] = 0
#ex <- ex %>% mutate(new_deaths = new_deaths_total_2019 + new_deaths)
plt <- dt %>% reshape2::melt(id.vars = "date")
p <- plt %>% 
  ggplot( aes(x=date, y=value, fill=variable, text=variable)) +
  geom_area( ) +
  scale_fill_viridis(discrete = TRUE) +
  ggtitle(paste0("Mortes em excesso no estado de ", inputstate)) +
  theme_ipsum() + geom_vline(aes(xintercept = as.Date("2020-03-13")))
  #theme(legend.position="none")

p <- ggplotly(p, tooltip="text")
p

  ex %>%  ggplot(aes(x = date)) + geom_point(aes(y = new_deaths_total_2019 %>% frollmean(7), col = "red")) +
  geom_point(aes(y = new_deaths_total_2020 %>% frollmean(7), col = "blue")) + geom_vline(aes(xintercept = as.Date("2020-03-13"))) +
    geom_point(aes(y = new_deaths %>% frollmean(7), col = "black"))

p<- aw %>% ggplot(aes(x = date, y = value)) + 
  facet_wrap(~variable) +
  geom_line()
p
