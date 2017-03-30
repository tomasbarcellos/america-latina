# ui.R

shinyUI(dashboardPage(skin = "green",
  dashboardHeader(title = "América Latina"),
  
  #####
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mercado Mundial", 
               # tabName = "mercado",
               icon = icon("dashboard"),
               menuSubItem("Comercio Exterior", "comex", icon = icon("bar-chart")),
               menuSubItem("Preços", "precos", icon = icon("line-chart")),
               menuSubItem("Balanca de Capitais", "capitais", icon = icon("money")),
               menuSubItem("Termos de troca", "termos", selected = TRUE,
                           icon = icon("line-chart"))),
      
      menuItem("Trabalhadores",
               menuSubItem("Desemprego", "desemprego", icon = icon("glyphicon-cutlery", lib = "glyphicon")),
               menuSubItem("Greves", "greves", icon = icon("glyphicon-equalizer", lib = "glyphicon"))),
      
      menuItem("Rentismo", 
               menuSubItem("Fronteira Agrícola", "front_agri", 
                           icon = icon("glyphicon-stats", lib = "glyphicon")))
    )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
    #####
    
      #####
      tabItem("comex",
              fluidRow(
                box(width = 6,
                    selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                                choices = list("Exportação" = '[2|3]',
                                               "Importação" = '[1|4]',
                                               "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                                selected = "Exportação")),
                box(width = 6,
                    sliderInput("ano", label = "Escolha o ano",
                                min = 2011, max = 2015, value = 2013)
                )
              ),
              fluidRow(
                box("Países", width = 2,
                    checkboxGroupInput("quant", label = "Escolha os países",
                                              choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                             "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                             "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                             "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                             "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                              selected = c("Brazil", "Argentina", "Venezuela"), inline = TRUE)),
                
                box(title = "Comercio Exterior por Pais", width = 10,
                  # downloadLink('download.graf1', "Clique aqui para baixar os dados deste grafico!"),
                  plotlyOutput("graf1")
                )
              ),
              fluidRow(
                box("Detalhamento da balança comercial", width = 2,
                           selectInput("pais",
                                       label = "Deseja obter informacoes sobre qual pais?", 
                                       choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                      "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                      "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                      "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                      "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                       selected = "Brazil"),
                           
                           sliderInput("qt_merc", label = "Mostrar mercadorias que acumulem quantos por cento?",
                                       min = 40, max = 85, value = 50)),
                
                box(title = "Balanca Comercial", width = 10,
                  # downloadLink('download.graf2', "Clique aqui para baixar os dados deste grafico!"),
                  plotlyOutput("graf2")
                )
              )
      ),
      
      #####
      tabItem("precos",
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
              ),
              fluidRow(
                box(title = "Preços", width = 12,
                    # downloadLink('download.graf3', "Clique aqui para baixar os dados deste grafico!"),
                    plotlyOutput("graf3")
                    )
              )

      ),
      
      #####
      tabItem("capitais",
        fluidRow(
          box(width = 3,
              selectInput("capitais", label = "Variável: ", 
                          choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                                         "Total da entrada liquida de capital", "Balanca de rendas", 
                                         "Transferencias liquidas"), selected = "Total da entrada liquida de capital")
              ),
          box(width = 9,
              checkboxGroupInput("paises.capitais", "Países: ", inline = TRUE,
                                 choices = unique(capitais$pais), 
                                 selected = c("Argentina", "Chile", "Colombia"))
          )
        ),
        fluidRow(
          box(title = "Balança de capitais", width = 12,
              plotlyOutput("graf4")
              )
        )
      ),
      
      #####
      tabItem("termos",
              fluidRow(
                box(title = "Termos de trocas", width = 12,
                    shiny::actionLink("termos_filtros", "Filtros"),
                    plotlyOutput("graf_termos"),
                    shinyBS::bsModal("termos_modal", "Filtros", "termos_filtros", size = 'large', 
                                     fluidRow(
                                       box(width = 9,
                                           checkboxGroupInput("termos_pais", label = "Países: ",
                                                              choices = structure(unique(termos_troca$País),
                                                                                  names = unique(termos_troca$País_desc)),
                                                              selected = c("Brasil", "México", "Argentina"), inline = TRUE)
                                       ),
                                       box(width = 3,
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
                box(width = 8,
                    checkboxGroupInput(
                      "paises.desemprego", "Países: ",
                      choices = unique(desemprego$País_desc),
                      selected = c(Argentina = "Argentina", 
                                   Brasil = "Brasil",
                                   Venezuela = "Venezuela (República Bolivariana de)"),
                      inline = TRUE)),
                box(width = 4,
                    selectInput("genero.desemprego", "Gênero: ",
                                choices = list("Ambos" = 146,
                                               "Masculino" = 265,
                                               "Feminino" = 266)),
                    sliderInput("periodo.desemprego", label = "Período",
                                min = 2005, max = 2015, value = c(2008,2015)
                    )
                )
              ),
              fluidRow(
                box(title = "Desemprego", width = 12,
                    plotlyOutput("graf_desemprego")
                )
              )
              
      ),
    #####
    tabItem("greves",
            fluidRow(
              box(width = 3,
                  selectInput("greve.indicador", "Indicador: ",
                              choices = list('Número de greves' = "Number of strikes and lockouts by economic activity null",
                                             'Dias não trabalhados (em razão de greves)' = "Days not worked due to strikes and lockouts by economic activity null",
                                             'Trabalhadores envolvidos em greves' = "Workers involved in strikes and lockouts by economic activity (thousands)",
                                             'Dias não trabalhados (em razão de greves) por 1000 trabalhadores' = "Days not worked per 1000 workers due to strikes and lockouts by economic activity null"),
                              selected = "Number of strikes and lockouts by economic activity null")
              ),
              box(width = 9,
                  checkboxGroupInput("greve.paises", "Países: ",
                                     choices = unique(greves$ref_area.label),
                                     selected = c(Argentina = "Argentina",
                                                  Brasil = "Brazil",
                                                  Mexico = "Mexico"), inline = TRUE)
              )
            ),
            fluidRow(
              box(title = "Greves", width = 12,
                  plotlyOutput("graf_greves"))
            )
    ),
    #####
      # Início da aba 3 - Rentismo
    tabItem("front_agri",
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
              )),
            fluidRow(
              box(title = "Indicadores de uso da terra", width = 12,
                    plotlyOutput("graf_fronteira"))
              )
            
    )
    )
  )
))