# ui.R
library(shiny)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plotly)

shinyUI(fluidPage(
  titlePanel("América Latina"),
  h4("Você está participando da criação de um painel com as informações
    básicas sobre a inserção das sociedades latino-americanas no mercado mundal, 
     visando tornar seu acompanhamento mais acessível ao público brasileiro."),
  h4("Aqui será possível acompanhar a evolução das estatísticas sobre comércio exterior, 
     preços das principais merdorias e fluxo de capitais.", align = "justify"),
  sidebarLayout(
    sidebarPanel(
      helpText("Altere as seleções para produzir o gráfico que deseja!"),
      selectInput("tipo", label = "Escolha  a categoria do comercio exterior", 
                  choices = list("Exportacao", "Importacao", "Exportacao e Importacao"), selected = "Exportacao"),
      
      sliderInput("ano", label = "Escolha o periodo",
                  min = 2011, max = 2015, value = 2013),
      
      sliderInput("quant", label = "Quantos paises?",
                  min = 3, max = 18, value = 5),
      br(),
      helpText("Estes selectore são para o gráfico de baixo."),
      
      selectInput("pais", label = "Deseja obter informações sobre qual pais?", 
                  choices = list("Argentina", "Bolivia (Plurinational State of)", "Brazil",
                                 "Chile", "Colombia", "Dominican Rep.", "Ecuador", "El Salvador", "Guatemala",
                                 "Guinea", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", 
                                 "Peru", "Uruguay", "Venezuela"), selected = "Brazil"),
      
      sliderInput("qt_merc", label = "Quantas mercadorias?",
                  min = 3, max = 10, value = 5),
      
      selectInput("parceiro", label = list(p("Escolha o parceiro comercial:"),helpText("Não está funcionando")),
                  choices = list("Mundo", "América Latina", "Estados Unidos"), selected = "Mundo"),
      
      selectInput("grupo.mercadoria", label = list("Escolha um grupo de mercadorias",helpText("Não está funcionando")), 
                  choices = list("Minerais", "Agropecuárias","Industriais"), selected = "Agropecuárias"),
      checkboxGroupInput("mercadoria", label = "Escolha as mercadorias", 
                         choices = list('ALUMINUM','BANANA_EU','BANANA_US','BARLEY','BEEF','CHICKEN','COAL_AUS',
                                        'COAL_COL','COAL_SAFRICA','COCOA','COCONUT_OIL','COFFEE_ARABIC','COFFEE_ROBUS',
                                        'COPPER','COPRA','COTTON_A_INDX','CRUDE_BRENT','CRUDE_DUBAI','CRUDE_PETRO',
                                        'CRUDE_WTI','DAP', 'FISH_MEAL','GOLD','GRNUT_OIL','IAGRICULTURE','IBEVERAGES',
                                        'IENERGY','IFATS_OILS','IFERTILIZERS','IFOOD','IGRAINS','IMETMIN','INONFUEL',
                                        'IRON_ORE','IRON_ORE_SPOT','ITIMBER','IOTHERFOOD','IOTHERRAWMAT','IRAW_MATERIAL',
                                        'KINATGAS','KALUMINUM','KBANANA_EU','KBANANA_US','KBARLEY','KBEEF','KCHICKEN',
                                        'KCOAL_AUS','KCOAL_COL','KCOAL_SAFRICA','KCOCOA','KCOCONUT_OIL','KCOFFEE_ARABIC',
                                        'KCOFFEE_ROBUS','KCOPPER','KCOPRA','KCOTTON_A_INDX','KCRUDE_BRENT','KCRUDE_DUBAI',
                                        'KCRUDE_PETRO','KCRUDE_WTI','KDAP','KFISH_MEAL','KGOLD','KGRNUT_OIL','KIAGRICULTURE','KIBEVERAGES',
                                        'KIENERGY','KIFATS_OILS','KIFERTILIZERS','KIFOOD','KIGRAINS','KIMETMIN','KIPRECIOUSMET','KINONFUEL',
                                        'KIOTHERFOOD','KIOTHERRAWMAT','KIRAW_MATERIAL','KIRON_ORE','KIRON_ORE_SPOT','KITIMBER','KLAMB',
                                        'KLEAD','KLOGS_CMR','KLOGS_MYS','KMAIZE','KNGAS_EUR','KNGAS_JP','KNGAS_US','KNICKEL','KORANGE',
                                        'KPALM_OIL','KPHOSROCK','KPLATINUM','KPLMKRNL_OIL','KPLYWOOD','KPOTASH','KRICE_05','KRICE_25',
                                        'KRICE_A1','KRICE_05_VNM','KRUBBER1_MYSG','KSAWNWD_CMR','KSAWNWD_MYS','KSHRIMP_MEX','KSILVER',
                                        'KSORGHUM','KSOYBEAN_MEAL','KSOYBEAN_OIL','KSOYBEANS','KSTL_JP_CROLL','KSTL_JP_HROLL',
                                        'KSTL_JP_REBAR','KSTL_JP_WIROD','KSUGAR_EU','KSUGAR_US','KSUGAR_','KTEA_AVG','KTEA_COLOMBO',
                                        'KTEA_KOLKATA','KTEA_MOMBASA','KTIN','KTOBAC_US','KTSP','KUREA_EE_BULK','KWHEAT_CANADI',
                                        'KWHEAT_US_HRW','KWHEAT_US_SRW','KWOODPULP','KZINC','LAMB','LEAD','LOGS_CMR','LOGS_MYS',
                                        'MAIZE','HMUV','NGAS_EUR','NGAS_JP','NGAS_US','NICKEL','ORANGE','PALM_OIL','PHOSROCK',
                                        'PLATINUM','PLMKRNL_OIL','PLYWOOD','POTASH','RICE_05','RICE_25','RICE_A1','RICE_05_VNM',
                                        'RUBBER1_MYSG','SAWNWD_CMR','SAWNWD_MYS','SHRIMP_MEX','SILVER','SORGHUM','SOYBEAN_MEAL',
                                        'SOYBEAN_OIL','SOYBEANS','STL_JP_CROLL','STL_JP_HROLL','STL_JP_REBAR','STL_JP_WIROD',
                                        'SUGAR_EU','SUGAR_US','SUGAR_','TEA_AVG','TEA_COLOMBO','TEA_KOLKATA','TEA_MOMBASA','TIN',
                                        'TOBAC_US','TSP','UREA_EE_BULK','WHEAT_CANADI','WHEAT_US_HRW','WHEAT_US_SRW','WOODPULP','ZINC'),
                         selected = c('IAGRICULTURE','IBEVERAGES','IENERGY', 'IGRAINS'))
    ),    
    mainPanel(
      downloadLink('download.graf1', "Clique aqui para baixar os dados deste gráfico!"),
      h3(textOutput("titulo1")),
      plotlyOutput("graf1"),
      br(),
      br(),
      downloadLink('download.graf2', "Clique aqui para baixar os dados deste gráfico!"),
      h3(textOutput("titulo2")),
      plotlyOutput("graf2"),
      br(),
      downloadLink('download.graf3', "Clique aqui para baixar os dados deste gráfico!"),
      h3(textOutput("titulo3")),
      sliderInput("periodo", label = "Visualizar os preços entre:",
                  min = 1960, max = 2015, value = c(1995,2015)),
      plotlyOutput("graf3")
    )
  )
  
))