# ui.R

shinyUI(dashboardPage(title = "Painel estatístico da América Latina", skin = "green",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    tags$link(href = 'estilo.css', type = "text/css", rel = 'stylesheet', media = 'all'),
    navbarPage('', collapsible = TRUE, id = "barra", position = 'static-top',
      tabPanel("Mercado Mundial", tabBox(width = "100%",
      tabPanel("Mapa", fluidRow(
        column(5,
               box("", width = "100%",
                   checkboxGroupInput("quant", label = "Países: ",
                                      choices = structure(
                                        c("Argentina", "Bolivia (Plurinational State of)",
                                          "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                          "Ecuador", "El Salvador", "Guatemala",
                                          "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                          "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                        names = c("Argentina", "Bolívia", "Brasil", "Chile",
                                                  "Colômbia", "República Dominicana", "Equador",
                                                  "El Salvador", "Guatemala", "Honduras",
                                                  "Jamaica", "México", "Nicaragua", "Panamá",
                                                  "Paraguai", "Peru", "Uruguay", "Venezuela")
                                      ),
                                      selected = c("Argentina", "Bolivia (Plurinational State of)",
                                                   "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                                   "Ecuador", "El Salvador", "Guatemala",
                                                   "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                                   "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                      inline = TRUE)
               ),
               box("", width = "100%",
                   selectInput("tipo", label = "Tipo de comércio: ", 
                               choices = list("Exportação" = '[2|3]',
                                              "Importação" = '[1|4]',
                                              "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                               selected = "[1-4]"),
                   selectInput("mapa_merc", "Grupo de mercadorias: ", 
                               choices = c("Total", as.character(unique(base$pai_desc))),
                               selected = "Total"),
                   sliderInput("ano", label = "Ano: ", sep = "", 
                               dragRange = FALSE, min = 2007, max = 2018, value = 2018)
               )
        ),
        column(7, box(title = NULL, width = "100%",
                      leafletOutput("mapa", height = "450px"),
                      p("Fonte:", a("Estatísticas de comércio da ONU", target = "_blank",
                                    href = "https://comtrade.un.org/data/"))
        ))
      )),
      tabPanel("Detalhe", fluidRow(
        column(4, 
               box(title = NULL, width = "100%",
                   selectInput("pais",
                               label = "País: ", 
                               choices = structure(
                                 c("Argentina", "Bolivia (Plurinational State of)",
                                      "Brazil", "Chile", "Colombia", "Dominican Rep.",
                                      "Ecuador", "El Salvador", "Guatemala",
                                      "Honduras", "Jamaica", "Mexico", "Nicaragua",
                                      "Panama", "Paraguay", "Peru", "Uruguay", "Venezuela"),
                                 names = c("Argentina", "Bolívia", "Brasil", "Chile",
                                           "Colômbia", "República Dominicana", "Equador",
                                           "El Salvador", "Guatemala", "Honduras",
                                           "Jamaica", "México", "Nicaragua", "Panamá",
                                           "Paraguai", "Peru", "Uruguay", "Venezuela")
                                 ),
                               selected = "Brazil"),
                   selectInput("tipo2", label = "Tipo de comércio: ", 
                               choices = list("Exportação" = '[2|3]',
                                              "Importação" = '[1|4]',
                                              "Exportação e Importação (corrente de comércio)" = '[1-4]'),
                               selected = "[2|3]"),
                   sliderInput("ano2", label = "Período: ", sep = "", 
                               dragRange = FALSE, min = 2007, max = 2018, value = c(2012, 2018)))), 
        column(8,
               box(title = NULL, width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf2", "highcharts"),
                   p("Fonte:", a("Estatísticas de comércio da ONU", target = "_blank",
                                 href = "https://comtrade.un.org/data/"))
               ))
      )),

      #####
      tabPanel("Termos de troca", fluidRow(
        column(4,
               box(width = "100%",
                   checkboxGroupInput(
                     "termos_pais", label = "Países: ",
                     choices = structure(
                       unique(termos_troca$País)[-1],
                       names = c("América Latina", "Argentina", "Bolívia", "Brasil",
                                 "Chile", "Colômbia", "Costa Rica", "Ecuador",
                                 "El Salvador", "Guatemala", "Haiti", "Honduras",
                                 "México", "Nicarágua", "Panamá", "Paraguai", "Peru",
                                 "República Dominicana", "Uruguai",
                                 "Venezuela")[-1]),
                     selected = c("222", "233", "216"), inline = TRUE)
               ),
               box(width = "100%",
                   selectInput("termos_var", label = "Indicador: ", 
                               choices = list("Termos de trocas de bens" = 4357,
                                              "Termos de troca de serviços" = 4358,
                                              "Termos de troca de bens e serviços"= 4360),
                               selected = 4357),
                   sliderInput("termos_periodo", label = "Período: ", sep = "",
                               min = min(termos_troca$Años_desc), max = max(termos_troca$Años_desc),
                               value = range(termos_troca$Años_desc))
               )
        ), 
        column(8, box(title = NULL, width = "100%",
                      HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                      chartOutput("graf_termos", "highcharts"),
                      p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                    href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
      ))
      )),
      
      #####
      tabPanel("Capital", fluidRow(
        column(4, 
               box(width = "100%",
                   checkboxGroupInput(
                     "paises.capitais", "Países: ", inline = TRUE,
                     choices = structure(
                       unique(bal_pag$Pais)[c(4, 9:12, 15:16, 18, 20, 22:26, 29:34, 39:41)],
                       names = c("América Latina", "América Latina y el Caribe",
                                 "Antigua y Barbuda", "Argentina", "Asociación Latinoamericana de Integración (ALADI)",
                                 "Bahamas", "Barbados", "Belice", "Bolívia", "Brasil",
                                 "Chile", "Colômbia", "Comunidad Andina (CAN)",
                                 "Comunidad del Caribe (CARICOM)", "Costa Rica",
                                 "Cuba", "Dominica", "Equador", "El Caribe", "El Salvador",
                                 "Granada", "Guatemala", "Guyana", "Haiti", "Honduras",
                                 "Jamaica", "Mercado Común Centroamericano (MCCA)",
                                 "Mercado Común del Sur (MERCOSUR) ", "México",
                                 "Nicaragua", "Panamá", "Paraguai", "Peru",
                                 "República Dominicana", "Saint Kitts y Nevis",
                                 "San Vicente y las Granadinas", "Santa Lucía",
                                 "Suriname", "Trinidad e Tabago", "Uruguay",
                                 "Venezuela")[c(4, 9:12, 15:16, 18, 20, 22:26, 29:34, 39:41)]), 
                     selected = c("Argentina", "Chile", "Colombia"))
               ),
               box(width = "100%",
                   selectInput("capitais", label = "Indicador: ",
                               choices = structure(
                                 unique(bal_pag$Rubrica)[-c(4,8)],
                                 names = c("Investimento direto", 'Balança de rendas',
                                           'Balança de transferências correntes',
                                           'Exportações de bens e serviços',
                                           "Conta corrente", 'Conta de capital',
                                           'Conta financeira', 'Importações de bens e serviços',
                                           'Reservas internacionais')[-c(4,8)]
                                 ),
                               selected = "Balance de renta"),
                   sliderInput("capitais_periodo", "Período: ", sep = "",
                               min = min(bal_pag$Ano), max = max(bal_pag$Ano),
                               value = range(bal_pag$Ano))
               )
        ),
        column(8, 
               box(title = NULL, width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf4", 'highcharts'),
                   p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                 href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
      )),
      tabPanel("Trabalho", tabBox(width = "100%",
      tabPanel("Desemprego", fluidRow(
        column(4,
               box(width = "100%",
                   checkboxGroupInput(
                     "paises.desemprego", "Países: ",
                     choices = structure(
                       unique(desemprego$País_desc)[-c(1:2)],
                       names = c("América Latina (promedio ponderado)",
                                 "América Latina (promedio simple)", "Argentina",
                                 "Bolivia", "Brasil", "Chile", "Colômbia", "Costa Rica",
                                 "Ecuador", "El Salvador", "Guatemala", "Honduras",
                                 "México", "Nicarágua", "Panamá", "Paraguai", "Peru",
                                 "República Dominicana", "Uruguai", "Venezuela")[-c(1:2)]
                     ),
                     selected = c("Argentina", "Brasil", "Colombia"),
                     inline = TRUE)),
               
               box(width = "100%",
                   selectInput("genero.desemprego", "Gênero: ",
                               choices = list("Ambos" = 146,
                                              "Masculino" = 265,
                                              "Feminino" = 266)),
                   sliderInput("periodo.desemprego", label = "Período", sep = "",
                               min = range(desemprego$Años_desc)[1],
                               max = range(desemprego$Años_desc)[2],
                               value = range(desemprego$Años_desc))
               )
        ),
        column(8,
               box(title = NULL, width = "100%",
                   HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                   showOutput("graf_desemprego", 'highcharts'),
                   p("Fonte:", a("Comissão Econômica para a América Latina - CEPAL", target = "_blank",
                                 href = "http://estadisticas.cepal.org/cepalstat/WEB_CEPALSTAT/Portada.asp?")))
        )
        
      )),
    #####
    tabPanel("Greves", fluidRow(
      column(4,
             box(width = "100%",
                 checkboxGroupInput(
                   "greve.paises", "Países: ",
                   choices = structure(
                     unique(greves$ref_area.label)[-c(1:3, 5:7, 10:11, 33:34, 36)],
                     names = c("Aruba", "Anguilla", "Netherlands Antilles", "Argentina" ,
                               "Antigua and Barbuda" ,"Bahamas", "Bermuda", "Bolivia",
                               "Brazil", "Barbados", "Canada", "Chile", "Colômbia",
                               "Costa Rica", "Dominica", "República Dominicana", "Equador", "Guatemala",
                               "Guyana", "Honduras", "Haiti", "Jamaica" ,"Mexico",
                               "Nicaragua", "Panamá", "Peru", "Porto Rico", "Paraguai",
                               "El Salvador", "Suriname", "Trinidad e Tobago",
                               "Uruguay", "United States", "Saint Vincent and the Grenadines",
                               "Venezuela", "United States Virgin Islands")[-c(1:3, 5:7, 10:11, 33:34, 36)]),
                   selected = c(Argentina = "Argentina",
                                Bolívia = "Bolivia",
                                Mexico = "Mexico"), inline = TRUE)
             ),
             box(width = "100%",
                 selectInput(
                   "greve.indicador", "Indicador: ",
                   choices = list(
                     'Número de greves' = "Number of strikes and lockouts by economic activity null",
                     'Dias não trabalhados (em razão de greves)' = "Days not worked due to strikes and lockouts by economic activity null",
                     'Trabalhadores envolvidos em greves (milhares)' = "Workers involved in strikes and lockouts by economic activity (thousands)"),
                   selected = "Number of strikes and lockouts by economic activity null"),
                 
                 sliderInput("greve.anos", "Período: ", sep = "", 
                             min = 1970, max = 2015, value = c(1980, 2015))
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
                 checkboxGroupInput("paises.fronteira", "Países: ",
                                    choices = structure(
                                      c("Argentina", "Bolivia (Estado Plurinacional de)",
                                        "Brasil", "Chile", "Colombia", "Costa Rica",
                                        "Ecuador", "El Salvador", "Guatemala",
                                        "Guayana francesa", "Guyana", "Honduras",                       
                                        "México", "Nicaragua", "Panamá", "Paraguay",
                                        "Perú", "Suriname", "Uruguay", "Venezuela (República Bolivariana de)"),
                                      names = c("Argentina", "Bolívia", "Brasil",
                                                "Chile", "Colômbia", "Costa Rica",
                                                "Equador", "El Salvador", "Guatemala",
                                                "Guiana francesa", "Guiana", "Honduras",                       
                                                "México", "Nicaragua", "Panamá", "Paraguai",
                                                "Peru", "Suriname", "Uruguai", "Venezuela")),
                                    selected = c("Colombia", "Perú", "Urugay"), inline = TRUE)
             ),
             box(width = "100%",
                 selectInput("var.fronteira", "Indicador: ",
                             choices = list("Área usada para agricultura" =  "Agricultura",
                                            "Área usada para pastagens" = "Pastagens",
                                            "Total" = "Superficie agrícola")),
                 sliderInput("periodo.fronteira", label = "Período: ", sep = "", 
                             min = 1961, max = 2016, value = c(1990, 2016))
             )),
      column(8,
             box(title = NULL, width = "100%", 
                 HTML('<style>.rChart {width: 100%; height: 100%}</style>'),
                 showOutput("graf_fronteira", 'highcharts'),
                 p("Fonte:", a("Organização das Nações Unidade para a Alimentação e Agricultura - FAO", target = "_blank",
                               href = "http://www.fao.org/statistics/en/")))
      )
    ))
    ))
))