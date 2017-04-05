# ui.R

shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "América Latina"#, .list = list(shiny::img(src = 'www/logo-mini.png'))
                  ),
  
  #####
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mercado Mundial", 
               icon = icon("dashboard"),
               menuSubItem("Comercio Exterior", "comex", icon = icon("bar-chart"), selected = TRUE),
               menuSubItem("Preços", "precos", icon = icon("line-chart")),
               menuSubItem("Balanca de Capitais", "capitais", icon = icon("money")),
               menuSubItem("Termos de troca", "termos",
                           icon = icon("line-chart"))),
      
      menuItem("Trabalhadores",
               menuSubItem("Desemprego", "desemprego", icon = icon("glyphicon-cutlery", lib = "glyphicon")),
               menuSubItem("Greves", "greves", icon = icon("glyphicon-equalizer", lib = "glyphicon"))),
      
      menuItem("Foco no Brasil", 
               menuSubItem("Fronteira Agrícola", "front_agri", 
                           icon = icon("glyphicon-stats", lib = "glyphicon")),
               menuSubItem("Custo da reservas", "reservas", 
                           icon = icon("glyphicon-stats", lib = "glyphicon")))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      #####
    tabItem("comex",
            fluidRow(
              column(6,
                     fluidRow(
                       box("Países", width = 12,
                           checkboxGroupInput("quant", label = "Escolha os países",
                                              choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                             "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                             "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                             "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                             "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                              selected = c("Argentina", "Bolivia (Plurinational State of)",
                                                           "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                           "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                           "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                           "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                              inline = TRUE))
                     ),
                     fluidRow(box("", width = 12,
                                  selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                                              choices = list("Exportação" = '[2|3]',
                                                             "Importação" = '[1|4]',
                                                             "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                                              selected = "Exportação")
                     )),
                     fluidRow(box("", width = 12,
                                  sliderInput("ano", label = "Escolha o ano",
                                              min = 2011, max = 2015, value = 2013)
                     ))
              ),
              column(6,
                     fluidRow(
                       box(title = "Comércio exterior por país", width = 12,
                           # p(actionLink("comex_filtros", "Filtros"), align = 'right'),
                           # downloadLink('download.graf1', "Clique aqui para baixar os dados deste grafico!"),
                           leafletOutput("mapa"),
                           p("Fonte:", a("Estatísticas de comércio da ONU", href = "https://comtrade.un.org/data/"))
                           # shinyBS::bsModal("comex_modal", "Filtros", "comex_filtros", size = 'large',
                           #                  fluidRow(
                           #                    box(width = 6#,
                           #                    ),
                           #                    box(width = 6#,
                           #                        
                           #                    )
                           #                  )
                           # )
                       )
                     ))
            ),
            fluidRow(
              box(title = "Balança comercial", width = 12,
                  p(actionLink("comex_filtros2", "Filtros"), align = 'right'),
                  # downloadLink('download.graf2', "Clique aqui para baixar os dados deste grafico!"),
                  plotlyOutput("graf2"),
                  p("Fonte:", a("Estatísticas de comércio da ONU", href = "https://comtrade.un.org/data/")),
                  shinyBS::bsModal("comex_modal2", "Filtros", "comex_filtros2", size = 'large',
                                   fluidRow(
                                     box("Detalhamento da balança comercial", width = 12,
                                         column(6, 
                                                selectInput("pais",
                                                            label = "Deseja obter informacoes sobre qual pais?", 
                                                            choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                                           "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                                           "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                                           "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                                           "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                                            selected = "Brazil")),
                                         column(6,
                                                sliderInput("qt_merc", label = "Mostrar mercadorias que acumulem quantos por cento?",
                                                            min = 40, max = 85, value = 50)))))
              )
            )
    ),
    
    #####
    tabItem("precos",
              fluidRow(
                box(title = "Preços", width = 12,
                    p(actionLink("precos_filtros", "Filtros"), align = 'right'),
                    # downloadLink('download.graf3', "Clique aqui para baixar os dados deste grafico!"),
                    plotlyOutput("graf3", height = '100%'),
                    p("Fonte:", a("Banco Mundial", href = "http://siteresources.worldbank.org/INTPROSPECTS/Resources/GemDataEXTR.zip")),
                    shinyBS::bsModal("precos_modal", "Filtros", "precos_filtros", size = 'large',
                                     fluidRow(
                                       box(width = 6,
                                           sliderInput("periodo", label = "Visualizar os precos entre:",
                                                       min = 1960, max = 2015, value = c(1995,2015))
                                       ),
                                       box(width = 6,
                                           checkboxGroupInput("mercadoria", label = "Escolha o(s) índice(s):", 
                                                              choices = list(`Petróleo WTI` = 'CRUDE_WTI',
                                                                             `Agricultura` = 'IAGRICULTURE',
                                                                             `Bebidas` = 'IBEVERAGES', 
                                                                             `Energia` = 'IENERGY',
                                                                             `Óleos e gordutas` = 'IFATS_OILS',
                                                                             `Fertilizantes` = 'IFERTILIZERS',
                                                                             `Comida` = 'IFOOD',
                                                                             `Grãos` = 'IGRAINS',
                                                                             `Metais e minerais` = 'IMETMIN',
                                                                             `Não combustíveis` = 'INONFUEL',
                                                                             `Minério de ferro` = 'IRON_ORE',
                                                                             `Madeira` = 'ITIMBER',
                                                                             `Outros alimentos` = 'IOTHERFOOD'),
                                                              selected = c('IAGRICULTURE', 'IGRAINS'),
                                                              inline = TRUE)
                                       )
                                     ))
                    )
              )

      ),
      
      #####
      tabItem("capitais",
        fluidRow(
          box(title = "Balança de capitais", width = 12,
              p(actionLink("capitais_filtros", "Filtros"), align = 'right'),
              # downloadLink('download.graf2', "Clique aqui para baixar os dados deste grafico!"),
              plotlyOutput("graf4", height = '100%'),
              p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")),
              shinyBS::bsModal("capitais_modal", "Filtros", "capitais_filtros", size = 'large',
                               fluidRow(
                                 box(width = 4,
                                     selectInput("capitais", label = "Variável: ", 
                                                 choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                                                                "Total da entrada liquida de capital", "Balanca de rendas", 
                                                                "Transferencias liquidas"), selected = "Total da entrada liquida de capital")
                                 ),
                                 box(width = 8,
                                     checkboxGroupInput("paises.capitais", "Países: ", inline = TRUE,
                                                        choices = unique(capitais$pais), 
                                                        selected = c("Argentina", "Chile", "Colombia"))
                                 )
                               ))
              )
        )
      ),
      
      #####
      tabItem("termos",
              fluidRow(
                box(title = "Termos de trocas", width = 12,
                    p(actionLink("termos_filtros", "Filtros"), align = 'right'),
                    plotlyOutput("graf_termos", height = '100%'),
                    p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")),
                    shinyBS::bsModal("termos_modal", "Filtros", "termos_filtros", size = 'large', 
                                     fluidRow(
                                       box(width = 8,
                                           checkboxGroupInput("termos_pais", label = "Países: ",
                                                              choices = structure(unique(termos_troca$País),
                                                                                  names = unique(termos_troca$País_desc)),
                                                              selected = c("Brasil", "México", "Argentina"), inline = TRUE)
                                       ),
                                       box(width = 4,
                                           selectInput("termos_var", label = "Variável: ", 
                                                       choices = list("Poder de compra das exportações de bens" = 4361,
                                                                      "Termos de trocas de bens, FOB" = 4357,
                                                                      "Porder de compra das exportações de bens e serviços" = 4359,
                                                                      "Termos de troca de bens e serviços"= 4360,
                                                                      "Termos de troca de serviços" = 4358),
                                                       selected = 4360)
                                       )
                                     ))
                )
              )
      ),
    #####
      tabItem("desemprego",
              fluidRow(
                box(title = "Desemprego", width = 12,
                    p(actionLink("desemprego_filtros", "Filtros"), align = 'right'),
                    plotlyOutput("graf_desemprego", height = '100%'),
                    p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")),
                    shinyBS::bsModal("desemprego_modal", "Filtros", "desemprego_filtros", size = 'large',
                                     fluidRow(
                                       box(width = 6,
                                           selectInput("genero.desemprego", "Gênero: ",
                                                       choices = list("Ambos" = 146,
                                                                      "Masculino" = 265,
                                                                      "Feminino" = 266))
                                           ),
                                       box(width = 6,
                                         sliderInput("periodo.desemprego", label = "Período",
                                                     min = 2005, max = 2015, value = c(2008,2015)
                                         )
                                       )
                                     ), 
                                     fluidRow(
                                       box(width = 12,
                                           checkboxGroupInput(
                                             "paises.desemprego", "Países: ",
                                             choices = unique(desemprego$País_desc),
                                             selected = c(Argentina = "Argentina", 
                                                          Brasil = "Brasil",
                                                          Venezuela = "Venezuela (República Bolivariana de)"),
                                             inline = TRUE))
                                     )
                    )
                )
              )
              
      ),
    #####
    tabItem("greves",
            fluidRow(
              box(title = "Greves", width = 12,
                  p(actionLink("greves_filtros", "Filtros"), align = 'right'),
                  plotlyOutput("graf_greves", height = '100%'),
                  p("Fonte:", a("Organização Internacional do Trabalho - OIT", href = "http://www.ilo.org/ilostat/faces/wcnav_defaultSelection?")),
                  shinyBS::bsModal("greves_modal", "Filtros", "greves_filtros", size = 'large',
                                   fluidRow(
                                     box(width = 4,
                                         selectInput("greve.indicador", "Indicador: ",
                                                     choices = list('Número de greves' = "Number of strikes and lockouts by economic activity null",
                                                                    'Dias não trabalhados (em razão de greves)' = "Days not worked due to strikes and lockouts by economic activity null",
                                                                    'Trabalhadores envolvidos em greves' = "Workers involved in strikes and lockouts by economic activity (thousands)",
                                                                    'Dias não trabalhados (em razão de greves) por 1000 trabalhadores' = "Days not worked per 1000 workers due to strikes and lockouts by economic activity null"),
                                                     selected = "Number of strikes and lockouts by economic activity null")
                                     ),
                                     box(width = 6,
                                         sliderInput("greve.anos", "Período: ",
                                                     min = 1970, max = 2015, value = c(1980, 2015))
                                     )
                                   ), fluidRow(
                                     box(width = 12,
                                         checkboxGroupInput("greve.paises", "Países: ",
                                                            choices = unique(greves$ref_area.label),
                                                            selected = c(Argentina = "Argentina",
                                                                         Brasil = "Brazil",
                                                                         Mexico = "Mexico"), inline = TRUE)
                                     )
                                   ))
                  )
            )
    ),
    #####
      # Início da aba 3 - Rentismo
    tabItem("front_agri",
            fluidRow(
              box(title = "Indicadores de uso da terra", width = 12, 
                  p(actionLink("fronteira_filtros", "Filtros"), align = 'right'),
                  plotlyOutput("graf_fronteira", height = '100%'),
                  p("Fonte:", a("Instituto Brasileiro de Geografia e Estatística - IBGE", href = "http://sidra.ibge.gov.br/")),
                  shinyBS::bsModal("fronteira_modal", "Filtros", "fronteira_filtros", size = 'large',
                                   fluidRow(
                                     box(width = 6,
                                         sliderInput("periodo.fronteira", label = "Período",
                                                     min = 1994, max = 2015, value = c(1994,2015))
                                     ),
                                     box(width = 6,
                                         selectInput("var.fronteira", "Escolha uma variável",
                                                     choices = list("Área plantada (ha)" = "plantada",
                                                                    "Área colhida (ha)" = "colhida",
                                                                    "Quantidade produzida (t)" = "quantidade",
                                                                    "Valor da produção (R$)" = "valor"))
                                     ))
                                   )
                    )
              )
            
    ),
    tabItem("reservas",
            fluidRow(
              box(title = "Custo das reservas", width = 12, 
                  p(actionLink("reservas_filtros", "Filtros"), align = 'right'),
                  plotlyOutput("graf_reservas", height = '100%'),
                  p("Fonte:", a("Banco Central do Brasil - BCB", href = "http://www.bcb.gov.br/")),
                  shinyBS::bsModal("reservas_modal", "Filtros", "reservas_filtros", size = 'large',
                                   fluidRow(
                                     box(width = 6,
                                         sliderInput("periodo.reservas", label = "Período",
                                                     min = 1994, max = 2015, value = c(1994,2015))
                                     ),
                                     box(width = 6,
                                         selectInput("var.reservas", "Escolha uma variável",
                                                     choices = list("Custo (milhões de R$ correntes)" = "custo",
                                                                    "Custo (% do PIB)" = "custo_PIB"))
                                     ))
                  )
              )
            )
            
    )
    )
  )
))