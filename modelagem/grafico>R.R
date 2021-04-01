library(gather.covid)
library(plotly)

a <- pegaCorona("caso_full")

a <- filter(a, city == "Araraquara") %>% mutate(mm7d_casos = frollmean(new_confirmed,7), mm7d_mortes = frollmean(new_deaths,7))

a %>% plot_ly(x = ~date) %>% add_bars(y = ~new_deaths, name = "Novos Obitos") %>% add_lines(y = ~mm7d_mortes, name = "Mm7d Obitos") %>% layout(title = "Novos óbitos em Araraquara-SP")

a %>% plot_ly(x = ~date) %>% add_bars(y = ~new_confirmed, name = "Novos Casos") %>% add_lines(y = ~mm7d_casos, name = "Mm7d Casos") %>% layout(title = "Novos casos em Araraquara-SP")

b <- feather::read_feather("seade-covid.feather") %>%  filter(nome_drs == "DRS 03 Araraquara")


b %>% plot_ly(x = ~datahora, y = ~ocupacao_leitos) %>% add_lines() %>% layout(title = "Ocupação de leitos de UTI em Araraqua")
