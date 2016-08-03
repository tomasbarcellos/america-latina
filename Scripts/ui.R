# ui.R
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plotly)

shinyUI(fluidPage(
  titlePanel("América Latina"),
  fluidRow(
    column(12,
           h4("Você está participando da criação de um painel com as informações
              básicas sobre a inserção das sociedades latino-americanas no mercado mundal, 
              visando tornar seu acompanhamento mais acessível ao público brasileiro."),
           h4("Aqui será possível acompanhar a evolução das estatísticas sobre comércio exterior, 
              preços das principais merdorias e fluxo de capitais.", align = "justify"))
           ),
  # Linha para gráfico 1
  fluidRow(
    
    column(3,
           br(),
           br(),
           br(),
           br(),
           selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                       choices = list("Exportacao", "Importacao", "Exportacao e Importacao"), selected = "Exportacao"),
           
           sliderInput("ano", label = "Escolha o periodo",
                       min = 2011, max = 2015, value = 2013),
           
           sliderInput("quant", label = "Quantos paises?",
                       min = 3, max = 18, value = 5)),
    
    column(9,
           downloadLink('download.graf1', "Clique aqui para baixar os dados deste gráfico!"),
           h3(textOutput("titulo1")),
           plotlyOutput("graf1"),
           br(),
           br())
  ),
  # Linha para gráfico 2
  fluidRow(
    
    column(3,
           br(),
           br(),
           br(),
           br(),
           selectInput("pais", label = "Deseja obter informações sobre qual pais?", 
                       choices = list("Argentina", "Bolivia (Plurinational State of)", "Brazil",
                                      "Chile", "Colombia", "Dominican Rep.", "Ecuador", "El Salvador", "Guatemala",
                                      "Guinea", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", 
                                      "Peru", "Uruguay", "Venezuela"), selected = "Brazil"),
           
           sliderInput("qt_merc", label = "Mostrar mercadorias que acumulem quantos por cento?",
                       min = 30, max = 100, value = 50),
           
           selectInput("parceiro", label = list(p("Escolha o parceiro comercial:"),helpText("Não está funcionando")),
                       choices = list("Mundo", "América Latina", "Estados Unidos"), selected = "Mundo")),
    
    column(9,
           downloadLink('download.graf2', "Clique aqui para baixar os dados deste gráfico!"),
           h3(textOutput("titulo2")),
           plotlyOutput("graf2"),
           br())
  ),
  # Linha para gráfico 3
  fluidRow(
    
    column(3,
           selectInput("grupo.mercadoria", label = list("Escolha um grupo de mercadorias",helpText("Não está funcionando")), 
                       choices = list("Minerais", "Agropecuárias","Industriais"), selected = "Agropecuárias"),
           checkboxGroupInput("mercadoria", label = "Escolha as mercadorias", 
                              choices = list('CRUDE_WTI', 'GOLD','GRNUT_OIL','IAGRICULTURE','IBEVERAGES',
                                             'IENERGY','IFATS_OILS','IFERTILIZERS','IFOOD','IGRAINS','IMETMIN','INONFUEL',
                                             'IRON_ORE','IRON_ORE_SPOT','ITIMBER','IOTHERFOOD','IOTHERRAWMAT','IRAW_MATERIAL'),
                              selected = c('IAGRICULTURE','IBEVERAGES','IENERGY', 'IGRAINS'))),
    
    column(9,
           downloadLink('download.graf3', "Clique aqui para baixar os dados deste gráfico!"),
           h3(textOutput("titulo3")),
           sliderInput("periodo", label = "Visualizar os preços entre:",
                       min = 1960, max = 2015, value = c(1995,2015)),
           plotlyOutput("graf3"),
           br())
  ),
  # Linha para gráfico 4
  fluidRow(
    
    column(3,
           br(),
           br(),
           br(),
           br(),
           br(),
           selectInput("capitais", label = "Escolha  a variável sobre balança de capitais que deseja conhecer", 
                       choices = list("Entrada líquida de capitais autonomos", "Entrada líquida de capitais não-autonomos",
                                      "Total da entrada liquida de capital", "Balança de rendas", 
                                      "Transferências líquidas"), selected = "Total da entrada liquida de capital")),
    
    column(9,
           h3("Série histórica das balanças de capitais"),
           plotlyOutput("graf4"))
  )
    ))