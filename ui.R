# ui.R

shinyUI(dashboardPage(title = "OLA - Observatório Latino-Americano", skin = "green",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$link(href = 'estilo.css', type = "text/css", rel = 'stylesheet', media = 'all'),
    navbarPage('', collapsible = TRUE, id = "barra", position = 'static-top',
      tabPanel("Mercado Mundial", tabBox(width = "100%",
      tabPanel("Toda região", fluidRow(
        column(5,
               box("Países", width = "100%",
                   checkboxGroupInput("quant", label = "Escolha os países",
                                      choices = list("Argentina", "Bolivia (Plurinational State of)",
                                                     "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                     "Ecuador", "El Salvador", "Guatemala",
                                                     "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                     "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                      selected = c("Argentina", "Bolivia (Plurinational State of)",
                                                   "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                   "Ecuador", "El Salvador", "Guatemala",
                                                   "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                   "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                      inline = TRUE)
               ),
               box("", width = "100%",
                   selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                               choices = list("Exportação" = '[2|3]',
                                              "Importação" = '[1|4]',
                                              "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                               selected = "[1-4]"),
                   selectInput("mapa_merc", "Mercadoria: ", 
                               choices = c("Total", as.character(unique(base$pai_desc))),
                               selected = 2),
                   sliderInput("ano", label = "Escolha o ano", sep = "", 
                               dragRange = FALSE, min = 2007, max = 2016, value = 2016)
               )
        ),
        column(7, box(title = NULL, width = "100%",
                      leafletOutput("mapa", height = "450px"),
                      p("Fonte:", a("Estatísticas de comércio da ONU", target = "_blank",
                                    href = "https://comtrade.un.org/data/"))
        ))
      )),
      tabPanel("Detalhamento", fluidRow(
        column(4, 
               box("Detalhamento da balança comercial", width = "100%",
                   selectInput("pais",
                               label = "Deseja obter informacoes sobre qual pais?", 
                               choices = list("Argentina", "Bolivia (Plurinational State of)",
                                              "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                              "Ecuador", "El Salvador", "Guatemala",
                                              "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                              "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                               selected = "Brazil"),
                   selectInput("tipo2", label = "Escolha  a categoria do comercio exterior", 
                               choices = list("Exportação" = '[2|3]',
                                              "Importação" = '[1|4]',
                                              "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                               selected = "[1-4]"),
                   sliderInput("ano2", label = "Escolha o ano", sep = "", 
                               dragRange = FALSE, min = 2007, max = 2016, value = c(2012, 2016)))), 
        column(8,
               box(title = "Balança comercial", width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf2", "highcharts"),
                   p("Fonte:", a("Estatísticas de comércio da ONU", target = "_blank",
                                 href = "https://comtrade.un.org/data/"))
               ))
      )),

      #####
      # tabPanel("Preços das mercadorias", fluidRow(
      #   column(4, 
      #          box(width = "100%",
      #              sliderInput("periodo", label = "Visualizar os precos entre:", sep = "",
      #                          min = 1960, max = 2015, value = c(1995,2015))
      #          ),
      #          box(width = "100%",
      #              checkboxGroupInput("mercadoria", label = "Escolha o(s) índice(s):", 
      #                                 choices = list(`Petróleo WTI` = 'CRUDE_WTI',
      #                                                `Agricultura` = 'IAGRICULTURE',
      #                                                `Bebidas` = 'IBEVERAGES', 
      #                                                `Energia` = 'IENERGY',
      #                                                `Óleos e gordutas` = 'IFATS_OILS',
      #                                                `Fertilizantes` = 'IFERTILIZERS',
      #                                                `Comida` = 'IFOOD',
      #                                                `Grãos` = 'IGRAINS',
      #                                                `Metais e minerais` = 'IMETMIN',
      #                                                `Não combustíveis` = 'INONFUEL',
      #                                                `Minério de ferro` = 'IRON_ORE',
      #                                                `Madeira` = 'ITIMBER',
      #                                                `Outros alimentos` = 'IOTHERFOOD'),
      #                                 selected = c('IAGRICULTURE', 'IGRAINS'),
      #                                 inline = TRUE)
      #          )),
      #   column(8, box(title = "Preços", width = "100%",
      #                 plotlyOutput("graf3", height = '100%'), 
      #                 p("Fonte:", a("Banco Mundial", target = "_blank",
      #                               href = "http://siteresources.worldbank.org/INTPROSPECTS/Resources/GemDataEXTR.zip")))
      #   )
      # )),
      #####
      tabPanel("Termos de troca", fluidRow(
        column(4,
               box(width = "100%",
                   checkboxGroupInput("termos_pais", label = "Países: ",
                                      choices = structure(unique(termos_troca$País),
                                                          names = unique(termos_troca$País_desc)),
                                      selected = c("222", "233", "216"), inline = TRUE)
               ),
               box(width = "100%",
                   selectInput("termos_var", label = "Variável: ", 
                               choices = list("Termos de trocas de bens" = 4357,
                                              "Termos de troca de serviços" = 4358,
                                              "Termos de troca de bens e serviços"= 4360,
                                              "Poder de compra das exportações de bens e serviços" = 4359,
                                              "Poder de compra das exportações de bens" = 4361),
                               selected = 4357),
                   sliderInput("termos_periodo", "", sep = "",
                               min = range(termos_troca$Años_desc)[1], max = range(termos_troca$Años_desc)[2],
                               value = range(termos_troca$Años_desc))
               )
        ), 
        column(8, box(title = "Termos de trocas", width = "100%",
                      HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                      chartOutput("graf_termos", "highcharts"),
                      p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                    href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
      ))
      )),
      
      #####
      tabPanel("Fluxo de capitais", fluidRow(
        column(4, 
               box(width = "100%",
                   selectInput("capitais", label = "Variável: ",
                               choices = structure(
                                 unique(bal_pag$Rubrica),
                                 names = c("Investimento direto", 'Balança de rendas',
                                           'Balança de transferências correntes',
                                           'Exportações de bens e serviços',
                                           'Importações de bens e serviços', 
                                           "Conta corrente", 'Conta de capital',
                                           'Conta financeira', 'Reservas internacionais')),
                               selected = "Balance de renta"),
                   sliderInput("capitais_periodo", "", sep = "",
                               min = range(bal_pag$Ano)[1], max = range(bal_pag$Ano)[2],
                               value = range(bal_pag$Ano))
                   # selectInput("capitais", label = "Variável: ", 
                   #             choices = list("Entrada liquida de capitais autonomos", "Entrada liquida de capitais nao-autonomos",
                   #                            "Total da entrada liquida de capital", "Balanca de rendas", 
                   #                            "Transferencias liquidas"), selected = "Total da entrada liquida de capital")
               ),
               box(width = "100%",
                   checkboxGroupInput("paises.capitais", "Países: ", inline = TRUE,
                                      choices = unique(bal_pag$Pais), 
                                      selected = c("Argentina", "Chile", "Colombia"))
               )
        ),
        column(8, 
               box(title = "Balança de capitais", width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf4", 'highcharts'),
                   p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                 href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
      )),
      tabPanel("Classe trabalhadora", tabBox(width = "100%", #side = "right",
      tabPanel("Desemprego", fluidRow(
        column(4,
               box(width = "100%",
                   selectInput("genero.desemprego", "Gênero: ",
                               choices = list("Ambos" = 146,
                                              "Masculino" = 265,
                                              "Feminino" = 266))
               ),
               box(width = "100%",
                   sliderInput("periodo.desemprego", label = "Período", sep = "",
                               min = range(desemprego$Años_desc)[1],
                               max = range(desemprego$Años_desc)[2],
                               value = range(desemprego$Años_desc))
               ),
               box(width = "100%",
                   checkboxGroupInput(
                     "paises.desemprego", "Países: ",
                     choices = unique(desemprego$País_desc),
                     selected = c(Argentina = "Argentina", 
                                  Brasil = "Brasil",
                                  Venezuela = "Venezuela (República Bolivariana de)"),
                     inline = TRUE))
        ),
        column(8,
               box(title = "Desemprego", width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf_desemprego", 'highcharts'),
                   p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                 href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
        
      )),
    #####
    tabPanel("Mobilização", fluidRow(
      column(4, 
             box(width = "100%",
                 selectInput("greve.indicador", "Indicador: ",
                             choices = list('Número de greves' = "Number of strikes and lockouts by economic activity null",
                                            'Dias não trabalhados (em razão de greves)' = "Days not worked due to strikes and lockouts by economic activity null",
                                            'Trabalhadores envolvidos em greves' = "Workers involved in strikes and lockouts by economic activity (thousands)",
                                            'Dias não trabalhados (em razão de greves) por 1000 trabalhadores' = "Days not worked per 1000 workers due to strikes and lockouts by economic activity null"),
                             selected = "Number of strikes and lockouts by economic activity null"),
                 sliderInput("greve.anos", "Período: ", sep = "", 
                             min = 1970, max = 2015, value = c(1980, 2015))
             ),
             box(width = "100%",
                 checkboxGroupInput("greve.paises", "Países: ",
                                    choices = unique(greves$ref_area.label),
                                    selected = c(Argentina = "Argentina",
                                                 Brasil = "Brazil",
                                                 Mexico = "Mexico"), inline = TRUE)
             )
      ),
      column(8,
             box(title = NULL, width = "100%",
                 HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                 showOutput("graf_greves", 'highcharts'),
                 p("Fonte:", a("Organização Internacional do Trabalho - OIT", target = "_blank",
                               href = "http://www.ilo.org/ilostat/faces/wcnav_defaultSelection?")))
      )
    ))
    )),
    
    #####
    # Início da aba 3 - Rentismo
    tabPanel("Renda da terra", fluidRow(
      column(4,
             box(width = "100%",
                 checkboxGroupInput("paises.fronteira", "Escolha os países",
                                    choices = list("Argentina", "Bolivia (Estado Plurinacional de)",
                                                   "Brasil", "Chile", "Colombia", "Costa Rica",
                                                   "Ecuador", "El Salvador", "Guatemala",
                                                   "Guayana francesa", "Guyana", "Honduras",                       
                                                    "México", "Nicaragua", "Panamá", "Paraguay",
                                                   "Perú", "Suriname", "Uruguay", "Venezuela (República Bolivariana de)"),
                                    selected = c("Colombia", "Perú", "Urugay"), inline = TRUE)
             ),
             box(width = "100%",
                 selectInput("var.fronteira", "Escolha uma variável: ",
                             choices = list("Área plantada" =  "Superficie agrícola",
                                            "Área para pasto" = "Praderas y pastos permanentes"))
             ),
             box(width = "100%",
                 sliderInput("periodo.fronteira", label = "Período", sep = "", 
                             min = 1961, max = 2015, value = c(1990,2015))
             )),
      column(8,
             box(title = "Indicadores de uso da terra", width = "100%", 
                 HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                 showOutput("graf_fronteira", 'highcharts'),
                 p("Fonte:", a("Organização das Nações Unidade para a Alimentação e Agricultura - FAO", target = "_blank",
                               href = "http://www.fao.org/statistics/en/")))
      )
    ))
    ))
))