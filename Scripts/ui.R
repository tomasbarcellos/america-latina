# ui.R
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plotly)

shinyUI(fluidPage(
  titlePanel("America Latina"),
  fluidRow(
    column(12,
           h4("Voce esta participando da criacao de um painel com as informacoes
              basicas sobre a insercao das sociedades latino-americanas no mercado mundal, 
              visando tornar seu acompanhamento mais acessivel ao publico brasileiro."),
           h4("Aqui sera possivel acompanhar a evolucao das estatisticas sobre comercio exterior, 
              precos das principais merdorias e fluxo de capitais.", align = "justify"))
           ),
  # Linha para grafico 1
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
           downloadLink('download.graf1', "Clique aqui para baixar os dados deste grafico!"),
           h3(textOutput("titulo1")),
           plotlyOutput("graf1"),
           br(),
           br())
  ),
  # Linha para grafico 2
  fluidRow(
    
    column(3,
           br(),
           br(),
           br(),
           br(),
           selectInput("pais", label = "Deseja obter informacoes sobre qual pais?", 
                       choices = list("Argentina", "Bolivia (Plurinational State of)", "Brazil",
                                      "Chile", "Colombia", "Dominican Rep.", "Ecuador", "El Salvador", "Guatemala",
                                      "Guinea", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", 
                                      "Peru", "Uruguay", "Venezuela"), selected = "Brazil"),
           
           sliderInput("qt_merc", label = "Mostrar mercadorias que acumulem quantos por cento?",
                       min = 40, max = 85, value = 50),
           
           selectInput("parceiro", label = list(p("Escolha o parceiro comercial:"),helpText("Nao esta funcionando")),
                       choices = list("Mundo", "America Latina", "Estados Unidos"), selected = "Mundo")),
    
    column(9,
           downloadLink('download.graf2', "Clique aqui para baixar os dados deste grafico!"),
           h3(textOutput("titulo2")),
           plotlyOutput("graf2"),
           br())
  ),
  # Linha para grafico 3
  fluidRow(
    
    column(3,
           selectInput("grupo.mercadoria", label = list("Escolha um grupo de mercadorias",helpText("Nao esta funcionando")), 
                       choices = list("Minerais", "Agropecuarias","Industriais"), selected = "Agropecuarias"),
           checkboxGroupInput("mercadoria", label = "Escolha as mercadorias", 
                              choices = list('CRUDE_WTI', 'GOLD','GRNUT_OIL','IAGRICULTURE','IBEVERAGES',
                                             'IENERGY','IFATS_OILS','IFERTILIZERS','IFOOD','IGRAINS','IMETMIN','INONFUEL',
                                             'IRON_ORE','IRON_ORE_SPOT','ITIMBER','IOTHERFOOD','IOTHERRAWMAT','IRAW_MATERIAL'),
                              selected = c('IAGRICULTURE','IBEVERAGES','IENERGY', 'IGRAINS'))),
    
    column(9,
           downloadLink('download.graf3', "Clique aqui para baixar os dados deste grafico!"),
           h3(textOutput("titulo3")),
           sliderInput("periodo", label = "Visualizar os precos entre:",
                       min = 1960, max = 2015, value = c(1995,2015)),
           plotlyOutput("graf3"),
           br())
  ),
  # Linha para grafico 4
  fluidRow(
    
    column(3,
           selectInput("capitais", label = "Escolha  a variavel sobre balanca de capitais que deseja conhecer", 
                       choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                                      "Total da entrada liquida de capital", "Balanca de rendas", 
                                      "Transferencias liquidas"), selected = "Total da entrada liquida de capital"),
           br(),
           br(),
           sliderInput("paises.capitais", label = "Numero de paises",
                       min = 1, max = 18, value = c(1,4))),
    
    column(9,
           h3("Serie historica das balancas de capitais"),
           plotlyOutput("graf4"))
  )
    ))