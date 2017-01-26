# ui.R

shinyUI(fluidPage(
  tags$div(align = "center",
    titlePanel("America Latina"),
    
    fluidRow(
      column(12,
             img(src = "iela_portal_2015_logos_ola.png"),
             h4("Voce está participando da criação de um painel com as informações
              básicas sobre a inserção das sociedades latino-americanas no mercado mundial, 
              visando tornar seu acompanhamento mais acessível ao público brasileiro."),
             h4("Aqui será possível acompanhar a evolução das estatísticas sobre comércio exterior, 
              preços das principais mercadorias e fluxos de capitais.", align = "justify"))
    )
  ),
  
  tabsetPanel(
    tabPanel("Mercado Mundial", value = 1),
    tabPanel("Mundo do trabalho", value = 2),
    tabPanel("Rentismo", value = 3),
    id = "grupo", selected = 1),
  
  # Início da aba 1 - Mercado Mundial
  conditionalPanel(condition = "input.grupo==1",
                   
                   tabsetPanel(
                     tabPanel("Comercio Exterior por Pais", value = 1),
                     tabPanel("Balanca Comercial", value = 2),
                     tabPanel("Precos", value = 3),
                     tabPanel("Balanca de Capitais", value = 4),
                     tabPanel("Termos de troca", value = 5),
                     id = "tab1selected", selected = 5),
                   
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
                                      
                                      column(width = 9,
                                             h3("Serie historica das balancas de capitais"),
                                             plotlyOutput("graf4"))
                                    )
                   ),
                   
                   # Aba de termos de troca
                   conditionalPanel(condition = "input.tab1selected==5",
                                    
                                    fluidRow(
                                      
                                      column(3,
                                             br(),
                                             br(),
                                             selectInput("termos_var", label = "Escolha  a variável sobre termos de troca que deseja conhecer", 
                                                         choices = list("Poder de compra das exportações de bens" = 4361,
                                                                        "Termos de trocas de bens, FOB" = 4357,
                                                                        "Porder de compra das exportações de bens e serviços" = 4359,
                                                                        "Termos de troca de bens e serviços"= 4360,
                                                                        "Termos de troca de serviços" = 4358),
                                                         selected = "Termos de troca de bens e serviços"),
                                             br(),
                                             br(),
                                             checkboxGroupInput("termos_pais", label = "Escolha os paises",
                                                                choices = structure(unique(termos_troca$País),
                                                                                    names = unique(termos_troca$País_desc)),
                                                                selected = c("Brasil", "México", "Argentina"), inline = TRUE)),
                                      
                                      column(width = 9,
                                             h3("Série histórica dos termos de troca"),
                                             plotlyOutput("graf_termos"))
                                    )
                   )
  ),
  
  # Início da aba 2 - Trabalho
  conditionalPanel(condition = "input.grupo==2",
                   
                   tabsetPanel(
                     tabPanel("Desemprego", value = 1),
                     tabPanel("Movimento sindical", value = 2),
                     tabPanel("Valor da Força de Trabalho", value = 3),
                     id = "tab2selected", selected = 1),
                   
                   # Aba desemprego
                   conditionalPanel(condition = "input.tab2selected==1",
                                    
                                    fluidRow(
                                      br(),
                                      tags$div(
                                        align = "center",
                                        h3("Evolução do desemprego"),
                                        column(3,
                                               checkboxGroupInput(
                                                 "paises.desemprego", "Países",
                                                 choices = unique(desemprego$País_desc),
                                                 selected = c(Argentina = "Argentina", 
                                                              Brasil = "Brasil",
                                                              Venezuela = "Venezuela (República Bolivariana de)"),
                                                 inline = TRUE), 
                                               sliderInput("periodo.desemprego", label = "Período",
                                                           min = 2005, max = 2015, value = c(2010,2015)),
                                               selectInput("genero.desemprego", "Gênero",
                                                           choices = list("Ambos" = 146,
                                                                          "Masculino" = 265,
                                                                          "Feminino" = 266))),
                                        column(9, 
                                               plotlyOutput("graf_desemprego"))
                                      )
                                    )
                   ),
                   
                   # Aba greves
                   conditionalPanel(condition = "input.tab2selected==2",
                                    
                                    fluidRow(
                                      br(),
                                      tags$div(
                                        align = "center",
                                        h3("Nº de greves por pais"),
                                        column(3,
                                               selectInput("greve.indicador", "Indicador de greves",
                                                           choices = list(`Greves` = "Number of strikes and lockouts by economic activity null",
                                                                          `Dias nao trabalhados (em razao de greves)` = "Days not worked due to strikes and lockouts by economic activity null",
                                                                          `Trabalhadores envolvidos em greves` = "Workers involved in strikes and lockouts by economic activity (thousands)",
                                                                          `Dias nao trabalhados (em razao de greves) por 1000 trabalhadores` = "Days not worked per 1000 workers due to strikes and lockouts by economic activity null"),
                                                           selected = "Number of strikes and lockouts by economic activity null"),
                                               checkboxGroupInput("greve.paises", "Países",
                                                                  choices = unique(greves$ref_area.label),
                                                                  selected = c(Argentina = "Argentina",
                                                                               Brasil = "Brazil",
                                                                               Mexico = "Mexico"), inline = TRUE)),
                                        column(9, plotlyOutput("graf_greves"))
                                      )
                                    )
                   )
  ),
  
  # Início da aba 3 - Rentismo
  conditionalPanel(condition = "input.grupo==3",
                   
                   tabsetPanel(
                     tabPanel("Fronteira agrícola", value = 1),
                     tabPanel("Setor bancário", value = 2),
                     id = "tab3selected", selected = 1),
                   
                   conditionalPanel(condition = "input.tab3selected==1",
                                    
                                    fluidRow(
                                      br(),
                                      tags$div(
                                        align = "center",
                                        h3("Indicadores de uso da terra"),
                                        column(3, 
                                               sliderInput("periodo.fronteira", label = "Período",
                                                           min = 1991, max = 2015, value = c(1991,2015)),
                                               selectInput("var.fronteira", "Escolha uma variável",
                                                           choices = list("Área plantada (ha)" = "plantada",
                                                                          "Área colhida (ha)" = "colhida",
                                                                          "Quantidade produzida (t)" = "quantidade",
                                                                          "Valor da produção (R$)" = "valor"))),
                                        column(9, plotlyOutput("graf_fronteira"))
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