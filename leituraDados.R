install.packages("dplyr")
library(dplyr)

# Leitura de dados baixados
list.files("Dados")
teste <- read.csv("Dados/Precos_anuais_mercadorias.csv" ,
                  stringsAsFactors = FALSE)
names(teste)
str(teste)

# Download de dados do Banco Mundial
download.file("http://siteresources.worldbank.org/INTPROSPECTS/Resources/GemDataEXTR.zip", 
              destfile = "GEMData.zip")
unzip(zipfile = "GEMData.zip", "Dados")
list.files("Dados")
# Abre arquivo com dados dos precos mundais das princiais mercadorias


# Abre dados baiaxados manualmente do UnComTrade
comercio <- read.csv("Dados/comtrade.csv",
                     stringsAsFactors = FALSE)
str(comercio)
names(comercio)
comercio[comercio$Period == 2015, 2]

nova <- comercio %>% filter(Period == 2015) group_by(Reporter) %>%
  arrange(desc(Commodity))

nova2 <- data.frame(Ano = nova$Period,
                    Pais = nova$Reporter,
                    Mercadoria = nova$Commodity,
                    Codigo = nova$Commodity.Code,
                    Valor = nova$Trade.Value..US..,
                    ValorFOB = nova$FOB.Trade.Value..US..)
head(nova2)

# Download de dados UNComTrade com API
install.packages("rjson")
library(rjson)

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
    raw.data<- read.csv(string,header=TRUE)
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
}

BOL <- get.Comtrade(68)
str(BOL)
am_lat <- data.frame(codigo_UNComTrade = c(32, 68, 76, 152, 170, 192, 214, 218,
                                222, 254, 320, 324, 332, 340, 388, 484,
                                558, 591, 600, 604, 780, 858, 862),
                     pais = c("Argentina", "Bolivia", "Brasil", "Chile",
                     "Colombia", "Cuba", "Rep. Dominacana", "Ecuador",
                     "El Salvador", "Guiana Francesa", "Guatemala",
                     "Guyana", "Haiti", "Honduras", "Jamaica", "Mexico",
                     "Nicaragua", "Panama", "Paraguay", "Peru",
                     "Trinidad y Tobago", "Uruguay", "Venezuela"))
am_lat
