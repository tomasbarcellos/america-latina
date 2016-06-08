###########################
#### Download de dados ####
#### do Banco Mundial: ####
####    precos  das    ####
####    mercadorias    ####
###########################

install.packages("dplyr")
install.packages("readxl")
library(dplyr)
library(readxl)

# Download de dados do Banco Mundial
download.file("http://siteresources.worldbank.org/INTPROSPECTS/Resources/GemDataEXTR.zip", 
              destfile = "GEMData.zip")
if (dir.exists("Dados") == FALSE) dir.create("Dados")
arquivosGEM <- unzip(zipfile = "GEMData.zip", list = T)
commodities <- grep(pattern = "[C|c]ommodity", x = arquivosGEM)
unzip(zipfile = "GEMData.zip", files = arquivosGEM[commodities,1], exdir = "Dados")
arquivo <- list.files("Dados")

# Abre no R arquivo com dados dos precos mundais das princiais mercadorias
precos <- read_excel(path = paste0("Dados/",arquivo), sheet = 1)

# Faz alteracoes necessarias nos dados
precos <- precos[-1, ]
names(precos)[1] <- "Ano"

# Salva dados como objeto do R
saveRDS(object = precos, file = "precos_commodities.rds")

################################
####   Download de dados    ####
####    do UN ComTrade:     ####
#### exportações dos paises ####
####   da America Latina    ####
################################

# Download de dados UNComTrade com API
install.packages("rjson")
library(rjson)

# Define função para download de dados baseado no API da UnComTrade
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
                         fmt = "csv" # Formato CSV
)
{
  string<- paste(url
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
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE, stringsAsFactors = FALSE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
} # Fim da função

# Define data frame com codigo e nome dos países da América Latina
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

# Nomeia cada item da lista com o nome do país para os quais armazenará os dados
names(comercioAL) <- am_lat$pais

# Loop que tenta fazer o download dos dados de exportação de cada país
for (pais in 1:nrow(am_lat)) {
  comercioAL[[pais]] <- try(get.Comtrade(am_lat[pais,1]))
} # Primeira rodada, erros de conexao sao comuns

# Cria vetor que armazenará os erros da última operação
erros <- vector(length = nrow(am_lat))

# Loop que atribui a cada elemento do vetor a ocorrencia de erro no download
for (i in seq_along(erros)) {
  erros[i] <- class(comercioAL[[i]]) == "try-error"
}

# Imprime a quatidade de erros occoridos, caso hajam
warning(sum(erros), if (sum(erros) == 1) {" erro encontrado!"} else {" erros encontrados!"},
    if(sum(erros) >0 ) {
      " Rode o código abaixo para realizar nova tentativa de download para aqueles países em que ouve falha"})

# Loop que dura enquanto persistirem erros na tentativa de download
while (sum(erros) > 0){
  for (pais in 1:nrow(am_lat)) {
    if (class(comercioAL[[pais]]) == "try-error") {
      comercioAL[[pais]] <- try(get.Comtrade(am_lat[pais,1])) 
    }
  }
  for (i in seq_along(erros)) {
    erros[i] <- class(comercioAL[[i]]) == "try-error"
  }
  warning(sum(erros), if (sum(erros) == 1) {" erro encontrado!"} else {" erros encontrados!"},
          if(sum(erros) >0 ) {
            " Realizarei uma nova tentativa"})
}

# O código abaixo deve ser rodado caso ainda tenham persistido erros (elimine '#' da linha abaixo)
# comercioAL[[which(erros == TRUE)]] <- (get.Comtrade(am_lat[which(erros == TRUE),1]))

# Salva os dados como objeto(lista) do R
saveRDS(comercioAL,file = "dados_lista.rds")

# Elimina lista dos paises que tenha falhado em fazer download
comercioAL[[which(erros == TRUE)]] <- NULL

# Elimina lista "validation"(vazia) das listas e transforma a lita "data" na lista principal
for (pais in seq_along(comercioAL)) {
  comercioAL[[pais]]$validation <- NULL
  comercioAL[[pais]] <- comercioAL[[pais]]$data
}

# Transforma todas as listas num unico data-frame
comercioAL <- as.data.frame(do.call(rbind, comercioAL))

saveRDS(comercioAL,file = "dados_df.rds")
