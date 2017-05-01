dado <- read.csv('FAOSTAT_data_4-25-2017.csv',
                 stringsAsFactors = FALSE, encoding = 'win-1252')
dado <- dado[, -c(1, 2, 13, 14)]

names(dado) <- c("ISO3", "País", "Cod_Elemento", "Elemento",
                 "Cod_Produto", "Produto", "Cod_Ano", "Ano",
                 "Unidade", "Valor")
library(dplyr)
library(tidyr)
wide <- dado %>% select(ISO3, Ano, Produto,Valor) %>%
  group_by(Produto, Ano, ISO3) %>% 
  mutate(id = seq_len(n())) %>% 
  ungroup() %>% 
  spread(key = Produto, value = Valor)

library(ggplot2)
names(wide)[9] <- "Agricola"
ggplot(wide[wide$ISO3 %in% c('BRA', 'ARG'), ], aes(Ano, Agricola, col = ISO3)) + 
  geom_path()

dado %>% filter(ISO3 %in% c('BRA', 'ARG'),
                Cod_Produto %in% c(6610, 6655)) %>% 
  ggplot(aes(Ano, Valor)) + 
  geom_path(aes(linetype = ISO3, col = Produto))


dado2 <- dado %>% filter(ISO3 %in% c('BRA', 'ARG'), Cod_Produto %in% c(6610, 6655))

h1 <- Highcharts$new()
h1$chart("spline")
for (pais in unique(dado2$País)) {
  for (prod in unique(dado2$Produto))
  h1$series(data = toJSONArray2(dado2[dado2$País == pais &
                                        dado2$Produto == prod, c("Ano", "Valor")],
                                json = FALSE, names = FALSE), 
            name = paste(prod, '-', pais))
}
h1

h2 <- hPlot(x = "Ano", y = "Valor", data = dado2,
            type = "line", group = "País")

