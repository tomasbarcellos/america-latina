# iniciar
library(tidyr)
library(dplyr)

# baixar tabela
download.file(url = 'https://sidra.ibge.gov.br/geratabela?format=br.csv&name=fronteira-agricola.csv&labels=false&query=t/1612/n1/all/v/109,214,215,216/p/all/c81/2692,2702,2708,2711,2713/l/t,v%2Bc81,p',
              destfile = 'fontes/fronteira-agricola.csv')

# alterar tabela
fronteira <- read.csv2('fontes/fronteira-agricola.csv')
names(fronteira) <- c('arroz - plantada', 'feijao - plantada',
                      'mandioca - plantada', 'milho - plantada',
                      'soja - plantada',
                      'arroz - colhida', 'feijao - colhida',
                      'mandioca - colhida', 'milho - colhida',
                      'soja - colhida',
                      'arroz - quantidade', 'feijao - quantidade',
                      'mandioca - quantidade', 'milho - quantidade',
                      'soja - quantidade',
                      'arroz - valor', 'feijao - valor',
                      'mandioca - valor', 'milho - valor',
                      'soja - valor')

fronteira_junta <- fronteira %>%
  mutate(ano = 1991:2015) %>%
  gather(key = indicador, value = valor, -ano) %>%
  separate(indicador, c("cultura", "variavel"),
           sep = " - ") %>%
  mutate(id = paste(ano, cultura)) %>% 
  spread(key = variavel, value = valor)

saveRDS(fronteira_junta, 'dados/fronteira-agricola.RDS')
