# ui.R

shinyUI(fluidPage(
  titlePanel("Painel"),
  
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
      helpText("Estes selectore são para o gráfico de baixo.",
               p("(Os dois últimos são apenas ilustrativos)")),
      
      selectInput("pais", label = "Deseja obter informações sobre qual pais?", 
                  choices = list("Argentina", "Bolivia (Plurinational State of)", "Brazil",
                                 "Chile", "Colombia", "Dominican Rep.", "Ecuador", "El Salvador", "Guatemala",
                                 "Guinea", "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", "Paraguay", 
                                 "Peru", "Uruguay", "Venezuela"), selected = "Brazil"),
      
      sliderInput("qt_merc", label = "Quantas mercadorias?",
                  min = 3, max = 10, value = 5),
      
      selectInput("parceiro", label = "Escolha o parceiro comercial:", 
                  choices = list("Mundo", "América Latina", "Estados Unidos"), selected = "Mundo"),
      
      selectInput("mercadoria", label = "Escolha um grupo de mercadorias", 
                  choices = list("Minerais", "Agropecuárias","Industriais"), selected = "Agropecuárias")
    ),    
    mainPanel(
      plotOutput("graf1"),
      br(),
      br(),
      plotOutput("graf2")
    )
  )
  
))