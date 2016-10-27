# ui.R
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plotly)
library(feather)

shinyUI(fluidPage(
  tags$div(align = "center",
    titlePanel("America Latina"),
    
    fluidRow(
      column(12,
             img(src = "iela_portal_2015_logos_ola.png"),
             h4("Voce esta participando da criacao de um painel com as informacoes
              basicas sobre a insercao das sociedades latino-americanas no mercado mundal, 
              visando tornar seu acompanhamento mais acessivel ao publico brasileiro."),
             h4("Aqui sera possivel acompanhar a evolucao das estatisticas sobre comercio exterior, 
              precos das principais merdorias e fluxo de capitais.", align = "justify"))
    )
  ),
  
  tabsetPanel(
    tabPanel("Mercado Mundial", value = 1),
    tabPanel("Mundo do trabalho", value = 2),
    id = "grupo", selected = 1),
  
  conditionalPanel(condition = "input.grupo==1",
                   
                   tabsetPanel(
                     tabPanel("Comercio Exterior por Pais", value = 1,
                              helpText("Esta mostra Exportacoes ou importacoes por pais")),
                     tabPanel("Balanca Comercial", value = 2,
                              helpText("Esta mostra dados da balanca comercial")),
                     tabPanel("Precos", value = 3,
                              helpText("Esta mostra dados dos precos internacionais 
                      das princiapais mercadorias vendidas por paises latino-americanos")),
                     tabPanel("Balanca de Capitais", value = 4,
                              helpText("Esta mostra dados da balanca de capitais")),
                     id = "tab1selected", selected = 4),
                   
                   # Linha para grafico 1
                   conditionalPanel(condition = "input.tab1selected==1",
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
                                    )
                   ),
                   
                   # Linha para grafico 2
                   conditionalPanel(condition = "input.tab1selected==2",
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
                                    )
                   ),
                   # Linha para grafico 3
                   conditionalPanel(condition = "input.tab1selected==3",
                                    fluidRow(
                                      
                                      column(3,
                                             selectInput("grupo.mercadoria", label = list("Escolha um grupo de mercadorias",helpText("Nao esta funcionando")), 
                                                         choices = list("Minerais", "Agropecuarias","Industriais"), selected = "Agropecuarias"),
                                             checkboxGroupInput("mercadoria", label = "Escolha as mercadorias", 
                                                                choices = list('CRUDE_WTI','IAGRICULTURE','IBEVERAGES', 'IENERGY','IFATS_OILS',
                                                                               'IFERTILIZERS','IFOOD','IGRAINS','IMETMIN','INONFUEL',
                                                                               'IRON_ORE','ITIMBER','IOTHERFOOD'),
                                                                selected = c('IAGRICULTURE', 'IGRAINS'))),
                                      
                                      column(9,
                                             downloadLink('download.graf3', "Clique aqui para baixar os dados deste grafico!"),
                                             h3(textOutput("titulo3")),
                                             sliderInput("periodo", label = "Visualizar os precos entre:",
                                                         min = 1960, max = 2015, value = c(1995,2015)),
                                             plotlyOutput("graf3"),
                                             br())
                                    )
                   ),
                   # Linha para grafico 4
                   conditionalPanel(condition = "input.tab1selected==4",
                                    
                                    fluidRow(
                                      
                                      column(3,
                                             br(),
                                             br(),
                                             br(),
                                             selectInput("capitais", label = "Escolha  a variavel sobre balanca de capitais que deseja conhecer", 
                                                         choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                                                                        "Total da entrada liquida de capital", "Balanca de rendas", 
                                                                        "Transferencias liquidas"), selected = "Total da entrada liquida de capital"),
                                             br(),
                                             br(),
                                             sliderInput("paises.capitais", label = "Numero de paises",
                                                         min = 1, max = 18, value = c(3,5))),
                                      
                                      column(9,
                                             h3("Serie historica das balancas de capitais"),
                                             plotlyOutput("graf4"))
                                    )
                   )
  ),
  
  conditionalPanel(condition = "input.grupo==2",
                   
                   tabsetPanel(
                     tabPanel("Desemprego", value = 1),
                     tabPanel("Movimento sindical", value = 2),
                     tabPanel("Valor da Força de Trabalho", value = 3),
                     id = "tab2selected", selected = 1),
                   
                   conditionalPanel(condition = "input.tab2selected==1",
                                    
                                    fluidRow(
                                      br(),
                                      br(),
                                      tags$div(
                                        align = "center",
                                        h3("Evolução do desemprego"),
                                        sliderInput("periodo.desemprego", label = "Período",
                                                    min = 2005, max = 2015, value = c(2010,2015)),
                                        selectInput("genero.desemprego", "Gênero",
                                                    choices = list("Ambos" = 146,
                                                                   "Masculino" = 265,
                                                                   "Feminino" = 266)),
                                        plotlyOutput("graf.desemprego")
                                      )
                                      
                                    )
                   )
  ),
  
  fluidRow(
    tags$footer(align = "center", 
                p(
                  "Este painel foi criado pelo",
                  a(href = "http://iela.ufsc.br", "Instituto de Estudos Latino-Americanos"),
                  img(src = "logo-mini.png")
                ),
                p(
                  "Acesse o link deste projeto no", a(href = "https://github.com/tomasbarcellos/Painel-AL/", "GitHub"), "." 
                ))
  )
))