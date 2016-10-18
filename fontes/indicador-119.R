library(xml2)
library(feather)

serie <- "http://interwp.cepal.org/sisgen/ws/cepalstat/getDataWithoutMeta.asp?IdIndicator=119"
pagina <-  read_xml(serie)

id <- pagina %>% xml_find_all("//datos") %>% xml_attr('idIndicator')

nome <- pagina %>% xml_find_all("//datos") %>% xml_attr('indicador')

unidade <- pagina %>% xml_find_all("//datos") %>% xml_attr('unidad')

# Dimensoes

genero <- pagina %>% xml_find_all("//dato") %>% xml_attr('dim_144')

pais <- pagina %>% xml_find_all("//dato") %>% xml_attr('dim_208')

escolaridade <- pagina %>% xml_find_all("//dato") %>% xml_attr('dim_1426')

ano <- pagina %>% xml_find_all("//dato") %>% xml_attr('dim_29117')

# Metadados
fonte <- pagina %>% xml_find_all("//dato") %>% xml_attr('id_fuente')

notas <- pagina %>% xml_find_all("//dato") %>% xml_attr('ids_notas')

sigla_pais <- pagina %>% xml_find_all("//dato") %>% xml_attr('iso3')

# Dado
valor <- pagina %>% xml_find_all("//dato") %>% xml_attr('valor') %>% as.numeric()


# Info da serie
n_dim <- pagina %>% xml_find_all("//info") %>% xml_attr('numeroDimensiones') %>% as.numeric()

n_dados <- pagina %>% xml_find_all("//info") %>% xml_attr('numeroDatos') %>% as.numeric()

tempo <- pagina %>% xml_find_all("//info") %>% xml_attr('tiempoProceso') %>% as.numeric()

save(list = ls(), file = paste0("historico/CEPAL-indicador-119-extracao-", Sys.Date(), ".RDA"))

tabela <- data.frame(genero, pais, escolaridade, ano, valor)

if(nrow(tabela) != n_dados) {
  stop('numero de linha não é o mesmo que o volume de dados informado pela CEPAL')
}

write_feather(tabela, "shiny/dados/desemprego.feather")

### Dicionário ###

# Download das dimensoes do inddicador 119

url <- "http://interwp.cepal.org/sisgen/ws/cepalstat/getDimensions.asp?idIndicator=119?&language=spanish"
pagina <-  read_xml(url)

# Dimensoes
nomes_dimensoes <- pagina %>% xml_find_all("//dim") %>% xml_attr('name')

id_dimensoes <- pagina %>% xml_find_all("//dim") %>% xml_attr('id')

# numero de filhos
filhos <- pagina %>% xml_length()

netos <- sapply(seq_len(filhos), function(x) pagina %>% xml_child(x) %>% xml_length())

# Desagregadores
nome_desagregacao <- pagina  %>% xml_find_all("//des") %>% xml_attr('name')
id_desagregacao <- pagina  %>% xml_find_all("//des") %>% xml_attr('id')
ordem_desagregacao <- pagina  %>% xml_find_all("//des") %>% xml_attr('order')
in_desagregacao <- pagina  %>% xml_find_all("//des") %>% xml_attr('in')

nomes_dimensoes <- c(rep(x = c(nomes_dimensoes[1]), times = netos[1]),
                     rep(x = c(nomes_dimensoes[2]), times = netos[2]),
                     rep(x = c(nomes_dimensoes[3]), times = netos[3]),
                     rep(x = c(nomes_dimensoes[4]), times = netos[4]))

id_dimensoes <- c(rep(x = c(id_dimensoes[1]), times = netos[1]),
                  rep(x = c(id_dimensoes[2]), times = netos[2]),
                  rep(x = c(id_dimensoes[3]), times = netos[3]),
                  rep(x = c(id_dimensoes[4]), times = netos[4]))

if (!(length(nomes_dimensoes) == length(id_dimensoes) | length(nomes_dimensoes) == sum(netos))) {
  stop("Há algum erro no tamanho dos vetores. Reveja o código por favor.")
}

dicionario_119 <- data.frame(nomes_dimensoes, id_dimensoes, nome_desagregacao,
                             id_desagregacao, ordem_desagregacao, in_desagregacao)

saveRDS(dicionario_119, "dados/dicionario_119.RDS")

### Colar dadaos ao dicionário ###

