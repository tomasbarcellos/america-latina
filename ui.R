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
               menuSubItem("Termos de troca", "termos", icon = icon("line-chart"))),
      
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
                box(title = "Filtros", width = 12,
                    column(6,
                           selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                                       choices = list("Exportação" = '[2|3]',
                                                      "Importação" = '[1|4]',
                                                      "Exportação e Importação" = '[1-4]'),
                                       selected = "Exportação"),
                           
                           sliderInput("ano", label = "Escolha o período",
                                       min = 2011, max = 2015, value = 2013),
                           
                           sliderInput("quant", label = "Quantos paises?",
                                       min = 3, max = 18, value = 5)
                ),
                column(6,
                       selectInput("pais",
                                   label = "Deseja obter informacoes sobre qual pais?", 
                                   choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                  "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                  "Ecuador", "El Salvador", "Guatemala", "Guinea",
                                                  "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                  "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                   selected = "Brazil"),
                       
                       sliderInput("qt_merc", label = "Mostrar mercadorias que acumulem quantos por cento?",
                                   min = 40, max = 85, value = 50))
                )
              ),
              fluidRow(
                box(title = "Comercio Exterior por Pais", width = 12,
                  # downloadLink('download.graf1', "Clique aqui para baixar os dados deste grafico!"),
                  plotlyOutput("graf1")
                )
              ),
              fluidRow(
                box(title = "Balanca Comercial", width = 12,
                  # downloadLink('download.graf2', "Clique aqui para baixar os dados deste grafico!"),
                  plotlyOutput("graf2")
                )
              )
      ),
      
      #####
      tabItem("precos",
              fluidRow(
                box(title = "Filtro - Período", width = 6,
                    sliderInput("periodo", label = "Visualizar os precos entre:",
                                min = 1960, max = 2015, value = c(1995,2015))
                ),
                box(title = "Filtros - Índices", width = 6,
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
                box("Preços", width = 12,
                    # downloadLink('download.graf3', "Clique aqui para baixar os dados deste grafico!"),
                    plotlyOutput("graf3")
                    )
              )

      ),
      
      #####
      tabItem("capitais",
        fluidRow(
          box("Filtro - Variável", width = 6,
              selectInput("capitais", label = "Escolha  a variavel sobre balanca de capitais que deseja conhecer", 
                          choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                                         "Total da entrada liquida de capital", "Balanca de rendas", 
                                         "Transferencias liquidas"), selected = "Total da entrada liquida de capital")
              ),
          box("Filtro - Países", width = 6,
              sliderInput("paises.capitais", label = "Numero de paises",
                          min = 1, max = 18, value = c(3,5)))
              ),
        fluidRow(
          box("Balança de capitais", width = 12,
              h3("Serie historica das balancas de capitais"),
              plotlyOutput("graf4")
              )
        )
      ),
      
      #####
      tabItem("termos",
              fluidRow(
                box("Filtro - País", width = 6,
                  checkboxGroupInput("termos_pais", label = "Escolha os paises",
                                     choices = structure(unique(termos_troca$País),
                                                         names = unique(termos_troca$País_desc)),
                                     selected = c("Brasil", "México", "Argentina"), inline = TRUE)
                ),
                box("Filtro - Variável", width = 6,
                    selectInput("termos_var", label = "Escolha  a variável sobre termos de troca que deseja conhecer", 
                                choices = list("Poder de compra das exportações de bens" = 4361,
                                               "Termos de trocas de bens, FOB" = 4357,
                                               "Porder de compra das exportações de bens e serviços" = 4359,
                                               "Termos de troca de bens e serviços"= 4360,
                                               "Termos de troca de serviços" = 4358),
                                selected = 4360)
                )
              ),
              fluidRow(
                box("Gráfico", width = 12,
                    h3("Série histórica dos termos de troca"),
                    plotlyOutput("graf_termos")
                )
              )
      ),
    #####
      tabItem("desemprego",
              fluidRow(
                box("Filtros - Paises", width = 4,
                    checkboxGroupInput(
                      "paises.desemprego", "Países",
                      choices = unique(desemprego$País_desc),
                      selected = c(Argentina = "Argentina", 
                                   Brasil = "Brasil",
                                   Venezuela = "Venezuela (República Bolivariana de)"),
                      inline = TRUE)),
                box("Filtros - Paises", width = 4,
                    sliderInput("periodo.desemprego", label = "Período",
                                min = 2005, max = 2015, value = c(2010,2015))
                ),
                box("Filtros - Paises", width = 4,
                    selectInput("genero.desemprego", "Gênero",
                                choices = list("Ambos" = 146,
                                               "Masculino" = 265,
                                               "Feminino" = 266))
                    )
              ),
              fluidRow(
                box("Gráfico - Desemprego", width = 12,
                    plotlyOutput("graf_desemprego")
                )
              )
              
      ),
    #####
    tabItem("greves",
            fluidRow(
              box("Filtros - Número de greves", width = 6,
                         selectInput("greve.indicador", "Indicador de greves",
                                     choices = list(`Greves` = "Number of strikes and lockouts by economic activity null",
                                                    `Dias nao trabalhados (em razao de greves)` = "Days not worked due to strikes and lockouts by economic activity null",
                                                    `Trabalhadores envolvidos em greves` = "Workers involved in strikes and lockouts by economic activity (thousands)",
                                                    `Dias nao trabalhados (em razao de greves) por 1000 trabalhadores` = "Days not worked per 1000 workers due to strikes and lockouts by economic activity null"),
                                     selected = "Number of strikes and lockouts by economic activity null")
                  ),
              box("Filtros - Países", width = 6,
                checkboxGroupInput("greve.paises", "Países",
                                   choices = unique(greves$ref_area.label),
                                   selected = c(Argentina = "Argentina",
                                                Brasil = "Brazil",
                                                Mexico = "Mexico"), inline = TRUE)
              )
            ),
            fluidRow(
              box("Gráfico - greves", width = 12,
                  plotlyOutput("graf_greves"))
            )
    ),
    #####
      # Início da aba 3 - Rentismo
    tabItem("front_agri",
            fluidRow(
              box("Filtros - Períoddo", width = 6,
                  sliderInput("periodo.fronteira", label = "Período",
                              min = 1994, max = 2015, value = c(1994,2015))
              ),
              box("Filtros - Variável", width = 6,
                  selectInput("var.fronteira", "Escolha uma variável",
                              choices = list("Área plantada (ha)" = "plantada",
                                             "Área colhida (ha)" = "colhida",
                                             "Quantidade produzida (t)" = "quantidade",
                                             "Valor da produção (R$)" = "valor"))
              )),
            fluidRow(
              box("Indicadores de uso da terra", width = 12,
                    plotlyOutput("graf_fronteira"))
              )
            
    )
    )
  )
))