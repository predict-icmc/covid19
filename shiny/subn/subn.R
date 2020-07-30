library(tidyverse)

dados<-read.csv(file = "obito_cartorio.csv",header=TRUE)

dados %>% ggplot() +
          geom_line(aes(x = epidemiological_week_2019, y =  new_deaths_total_2020 -new_deaths_total_2019,color = "blue"))
          

dados %>% ggplot() +
  geom_line(aes(x = epidemiological_week_2019, y =  new_deaths_respiratory_failure_2020 -new_deaths_respiratory_failure_2019,color = "blue"))

aq <- dados %>% filter(state == "SP" & epidemiological_week_2019 < 25) %>% mutate(delta_respiratory_failure = new_deaths_respiratory_failure_2020 - new_deaths_respiratory_failure_2019,
                 delta_indeterminate = deaths_indeterminate_2020 - deaths_indeterminate_2019,
                 delta_others = deaths_others_2020 - deaths_others_2019,
                 delta_pneumonia = deaths_pneumonia_2020 - deaths_pneumonia_2019,
                 delta_septicemia = deaths_septicemia_2020 - deaths_septicemia_2019,
                 delta_sars = deaths_sars_2020 - deaths_sars_2019) %>%
          select(date,delta_respiratory_failure,delta_indeterminate,delta_others,delta_pneumonia,delta_pneumonia,delta_septicemia,delta_sars)

aq$date <- as.Date(aq$date)
aq <- aq %>% filter(date< as.POSIXct("2020-06-30"))
aw <- melt(aq,id.vars = "date") 

aw %>% ggplot(aes(x = date, y = value)) + 
  facet_wrap(~variable) +
  geom_line()
