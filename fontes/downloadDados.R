# Retire o jogo da velha caso n?o possua os pacoques listados abaixo:
# install.packages("dplyr")
# install.packages("readxl")
# install.packages("rjson")

library(dplyr)
library(tidyr)
library(readxl)
library(rjson)
library(httr)
library(rvest)

###########################
#### Download de dados ####
#### do Banco Mundial: ####
####    precos  das    ####
####    mercadorias    ####
###########################

# Download de dados do Banco Mundial
if (dir.exists("dados") == FALSE) dir.create("dados")
download.file("http://siteresources.worldbank.org/INTPROSPECTS/Resources/GemDataEXTR.zip", 
              destfile = "dados/GEMData.zip")
arquivosGEM <- unzip(zipfile = "dados/GEMData.zip", list = T)
commodities <- grep(pattern = "[C|c]ommodity", x = arquivosGEM)
unzip(zipfile = "dados/GEMData.zip", files = arquivosGEM[commodities,1], exdir = "Dados")
arquivo <- list.files("dados", pattern = ".xls")

# Abre no R arquivo com dados dos precos mundais das princiais mercadorias
precos <- read_excel(path = paste0("dados/",arquivo), sheet = 1)

# Faz alteracoes necessarias nos dados
precos <- precos[-1, ]
names(precos)[1] <- "Ano"
nomes_precos <- names(precos)
nomes_precos <- gsub(pattern = "WLD", replacement = "", x = nomes_precos)
names(precos) <- nomes_precos
precos[, -1] <- sapply(precos[, -1], as.numeric)
precos <- gather(data = precos, Mercadoria, Preço, -Ano)

# Salva dados como objeto do R
if (dir.exists("dados") == FALSE) dir.create("dados")
saveRDS(object = precos, file = "dados/precos_commodities.rds")

################################
####   Download de dados    ####
####    do UN ComTrade:     ####
#### exporta??es dos paises ####
####   da America Latina    ####
################################

# Download de dados UNComTrade com API

# Define fun??o para download de dados baseado no API da UnComTrade
get.Comtrade <- function(r, # Area do relatorio. Um numero por pais
                         url = "http://comtrade.un.org/api/get?",
                         maxrec = 250000, # Maximo de observacoes
                         type = "C", # Comercio
                         freq = "A", # Anual
                         px = "HS", # Sistema Harmonizado, como reportado
                         ps = "recent", # Periodo da serie
                         p = "all", # Todos parceiros comerciais
                         rg = "all", # Regime de comercio (import, export)
                         cc = "AG2", # Nivel de detalhamento
                         fmt = "json" # Formato JSON
)
{
  string<- paste0(url
                  ,"max=",maxrec,"&" #maximum no. of records returned
                  ,"type=",type,"&" #type of trade (c=commodities)
                  ,"freq=",freq,"&" #frequency
                  ,"px=",px,"&" #classification
                  ,"ps=",ps,"&" #time period
                  ,"r=",r,"&" #reporting area
                  ,"p=",p,"&" #partner country
                  ,"rg=",rg,"&" #trade flow
                  ,"cc=",cc,"&" #classification code
                  ,"fmt=",fmt        #Format
  )
  
  cat("Conectando com UNComTrade...\n")
  
  resposta <- RETRY("GET", string)
  
  cat("Resposta recebida: ", http_status(resposta)[[1]],"\n")
  
  if(fmt == "csv") {
    raw.data<- read.csv(httr::content(resposta),header=TRUE, stringsAsFactors = TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data <- httr::content(resposta)
      data <- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata <- NULL
      if(length(data) > 0) {
        var.names <- names(data[[1]])
        data <- as.data.frame(t( sapply(data, rbind)))
        ndata <- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i] <- NA
          ndata <- cbind(ndata, unlist(data[,i]))
        }
        ndata <- as.data.frame(ndata)
        colnames(ndata) <- var.names
      }
      return(list(validation = validation, data = ndata))
    }
  }
} # Fim da fun??o

# Define data frame com codigo e nome dos pa?ses da Am?rica Latina
am_lat <- data.frame(codigo_UNComTrade = c(32, 68, 76, 152, 170, 192, 214, 218,
                                           222, 254, 320, 324, 332, 340, 388, 484,
                                           558, 591, 600, 604, 780, 858, 862),
                     pais = c("Argentina", "Bolivia", "Brasil", "Chile",
                              "Colombia", "Cuba", "Rep. Dominacana", "Ecuador",
                              "El Salvador", "Guiana Francesa", "Guatemala",
                              "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico",
                              "Nicaragua", "Panama", "Paraguay", "Peru",
                              "Trinidad y Tobago", "Uruguay", "Venezuela"),
                     stringsAsFactors = FALSE)

# Cria lista para armazenar tabelas
comercioAL <- vector("list", nrow(am_lat))

# Nomeia cada item da lista com o nome do pa?s para os quais armazenar? os dados
names(comercioAL) <- am_lat$pais

# Loop que tenta fazer o download dos dados de exporta??o de cada pa?s
for (pais in seq_along(am_lat$pais)) {
  comercioAL[[pais]] <- try(get.Comtrade(am_lat[pais,1],
                                         ps = "2011,2010,2009,2008,2007"))
} # Primeira rodada, erros de conexao sao comuns

# Cria vetor que armazenar? os erros da ?ltima opera??o
erros <- which(sapply(comercioAL, function (x) class(x) == "try-error") |
  sapply(comercioAL, function (x) any(sapply(x, is.null))))

# Imprime a quatidade de erros occoridos, caso hajam
warning(length(erros), if (length(erros) == 1) {" erro encontrado!"} else {" erros encontrados!"},
    if(length(erros) >0 ) {
      " Rode o c?digo abaixo para realizar nova tentativa de download para aqueles pa?ses em que ouve falha"})

# Loop que dura enquanto persistirem erros na tentativa de download
while (length(erros) > 0) {
  for (i in erros) {
    comercioAL[[i]] <- try(get.Comtrade(am_lat[i,1])) 
  }
  
  erros <- which(sapply(comercioAL, function (x) class(x) == "try-error") |
                   sapply(comercioAL, function (x) any(sapply(x, is.null))))
  
  warning(length(erros), if (length(erros) == 1) {" erro encontrado!"} else {" erros encontrados!"},
          if(length(erros) >0 ) {
            " Realizarei uma nova tentativa"})
}

# O c?digo abaixo deve ser rodado caso ainda tenham persistido erros (elimine '#' da linha abaixo)
# comercioAL[[which(erros == TRUE)]] <- (get.Comtrade(am_lat[which(erros == TRUE),1]))

# Salva os dados como objeto(lista) do R
# saveRDS(comercioAL, file = "comercio_listaJSON.rds")

# Elimina lista dos paises que tenha falhado em fazer download (elimine '#' da linha abaixo)
# comercioAL[[which(erros == TRUE)]] <- NULL

AL_df <- vector('list', length(comercioAL))
names(AL_df) <- names(comercioAL)

for (pais in seq_along(AL_df)) {
  if (length(comercioAL[[i]]) == 2) {
    AL_df[[pais]] <- comercioAL[[pais]]$data
  } else {
    AL_df[[pais]] <- comercioAL[[pais]]
  }
  
}

# Transforma todas as listas num unico data-frame
AL_df <- as.data.frame(do.call(rbind, AL_df))

# Verifica quais colunas s?o inuteis (apenas NAs)
elim <- as.vector(which(sapply(AL_df, function (x) sum(is.na(x)) == length(x)) | 
        sapply(AL_df, function (x) length(levels(x)) <= 1) == T))

# Elimina colunas inuteis
AL_df <- AL_df[ , -elim]

AL_df$cmdCode <- as.integer(as.character(AL_df$cmdCode))

AL_df$TradeValue <- as.numeric(as.character(AL_df$TradeValue))

# traducao <- read.csv2(file = "dados/traducao.csv",
#                       stringsAsFactors = F)
# 
# AL_df <- left_join(x = AL_df, y = traducao, by = "cmdCode")

AL_df <- unique(AL_df)

antigo <- readRDS('dados/comercioAL.RDS')

novo <- rbind(AL_df, antigo)

saveRDS(novo, file = "dados/comercioAL.RDS")

###########################
#### Download de dados ####
####     da CEPAL:     ####
####    balança de     ####
####     capitais      ####
###########################

# # Define função para download de dados baseado no API da UnComTrade
# get.CEPAL <- function(IdIndicador, dimensao1, desagregador1,
#                       dimensao2 = "", desagregador2 = "",
#                       dimensao3 = "", desagregador3 = "", lingua = "spanish") {
#   string<- paste0("http://interwp.cepal.org/sisgen/ws/cepalstat/getDataWithoutMeta.asp?",
#                   "IdIndicator=", IdIndicador)
#   dados <- read.table(string, stringsAsFactors = FALSE, sep = "`")
#   valores <- str_extract_all(string = dados, pattern = "=[-]?[0-9]*[.]?[0-9]*")[[1]]
#   #  valores <- valores[-c(1,2,3, length(valores)-2, length(valores)-1, length(valores))]
#   #  indicador <- valores[1]
#   #  valores <- valores[-1]
#   #  valores <- as.data.frame(matrix(valores, ncol = 9, byrow = TRUE))
#   #  names(valores) <- c("indicador1", "desagregador1", "indicador2", "desagregador2", "indicador3", "desagregador3", "IdFonte", "lixo", "valor")
#   return(valores)
# } # Fim da função (incompleto)
# 
# teste <- get.CEPAL(IdIndicador = 1629)

capitais <- read.table("cepal.csv", stringsAsFactors = FALSE)
capitais <- capitais[, c(2,4,6,8,10,12,14)]
names(capitais) <- c("pais_CEPAL", "variavel_CEPAL", "ano_CEPAL", "fonte", "nota", "iso3", "valor")
saveRDS(capitais, "capitais_todos.rds")

am_lat2 <- data.frame(pais_CEPAL = c(216, 221, 222, 224, 225, 249, 228, 229,
                                           230, 43448, 235, 237, 238, 239, 249, 233,
                                           240, 241, 242, 244, 256, 258, 259),
                     pais = c("Argentina", "Bolivia", "Brasil", "Chile",
                              "Colombia", "Cuba", "Rep. Dominacana", "Ecuador",
                              "El Salvador", "Guiana Francesa", "Guatemala",
                              "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico",
                              "Nicaragua", "Panama", "Paraguay", "Peru",
                              "Trinidad y Tobago", "Uruguay", "Venezuela"),
                     stringsAsFactors = FALSE)

capitais.AL <-  capitais %>% filter(capitais$pais_CEPAL %in% am_lat2$pais_CEPAL)
rm(capitais)
capitais.AL <- left_join(x = capitais.AL, y = am_lat2, by = "pais_CEPAL")
rm(am_lat2)

anos <- read.table('anos_CEPAL.csv', stringsAsFactors = FALSE)
anos <- anos[, c(2,4)]
names(anos) <- c("ano", "ano_CEPAL")
capitais.AL <- left_join(x = capitais.AL, y = anos, by = "ano_CEPAL")
rm(anos)

variavel <- read.table('variavel_CEPAL.csv', stringsAsFactors = FALSE)
variavel <- variavel[, c(2,4)]
names(variavel) <- c("variavel", "variavel_CEPAL")
capitais.AL <- left_join(x = capitais.AL, y = variavel, by = "variavel_CEPAL")
rm(variavel)

saveRDS(capitais.AL, "capitais_AL.rds")

# Reservas internacionais
library(ecoseries)
reservas <- series_bacen(3546)[[1]] # milhões de US$ # mensal
                          
juros_FED <- series_bacen(18152)[[1]] # % a.a. # mensal

div_interna <- series_bacen(4154)[[1]] # milhões de u.m.c. # mensal

PIB <- series_bacen(4382)[[1]] # milhõs R$ # mensal

selic <- series_bacen(4189)[[1]] # % a.a. # mensal

cambio <- series_bacen(3697)[[1]]

custo_reservas <- left_join(juros_FED, reservas, by = 'data') %>% 
  left_join(PIB, by = 'data') %>% left_join(selic, by = 'data') %>% 
  left_join(cambio)

names(custo_reservas)[2:6] <- c('juros_FED', 'reservas', 'PIB', 'selic', 'cambio')

custo_reservas <- custo_reservas %>% 
  mutate(data = as.Date(data , format = '%d/%m/%Y'),
         custo = (reservas * cambio) * (selic - juros_FED), # em milhões de R$
         custo_PIB = custo / PIB)

saveRDS(custo_reservas, 'dados/reservas.RDS')


# library(GetTDData)
# download.TD.data(NULL)
# TD <- read.TD.files()
# head(TD)

###################
## SM NECESSARIO ##
###################


resp <- GET('http://www.dieese.org.br/analisecestabasica/salarioMinimo.html')
SMN <- content(resp) %>% html_table() %>% `[[`(1)
SMN[, 2:3] <- sapply(SMN[, 2:3], gsub, pattern = "\\.", replacement = "")
SMN[, 2:3] <- sapply(SMN[, 2:3], gsub, pattern = "\\n", replacement = " ")
SMN[, 2:3] <- sapply(SMN[, 2:3], gsub, pattern = ",", replacement = ".")
SMN[, 2:3] <- sapply(SMN[, 2:3], gsub, pattern = "R\\$ ", replacement = "")
SMN[, 2:3] <- sapply(SMN[, 2:3], as.numeric)
SMN <- SMN %>% filter(!grepl(x = SMN$Período, pattern = "[0-9]{4}")) %>% 
  mutate(Data = sort(seq.Date(as.Date('1994-07-01'), by = "1 month",
                              length.out = nrow(.)),
                     decreasing = TRUE),
         Taxa = round(`Salário mínimo nominal` / `Salário mínimo necessário`, 3))
saveRDS(SMN, 'dados/sal_min_nec.RDS')

###########################
## Concentração bancária ##
###########################

arquivo <- 'fontes/concentracao.xlsx'
resp <- GET('http://www.bcb.gov.br/htms/estabilidade/2017_04/refAnexoEstatistico.xlsx', # falta automatizar
            write_disk(arquivo))
IHH <- read_excel(arquivo, "IHH", skip = 4) %>% 
  `[`(3:6) %>% mutate(indice = "IHH")
RC4 <- read_excel(arquivo, "RC4", skip = 38) %>%
  `[`(3:6) %>% mutate(indice = "RC4")
concentracao <- bind_rows(IHH, RC4) %>% 
  gather(key = metrica, value = valor, -indice, -Trim.)

saveRDS(concentracao, 'dados/concentracao.RDS')

#########################
## Fronteira agrpicola ##
#########################

dado <- read.csv('FAOSTAT_data_4-25-2017.csv',
                 stringsAsFactors = FALSE, encoding = 'win-1252')
dado <- dado[, -c(1, 2, 13, 14)]

names(dado) <- c("ISO3", "País", "Cod_Elemento", "Elemento",
                 "Cod_Produto", "Produto", "Cod_Ano", "Ano",
                 "Unidade", "Valor")
dado <- filter(dado, Produto %in% c("Superficie agrícola", "Praderas y pastos permanentes"))
saveRDS(dado, 'dados/fronteira_agri_AL.RDS')
