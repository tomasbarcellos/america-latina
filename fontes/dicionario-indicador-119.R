# Download das dimensoes do inddicador 119
library(rvest)

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