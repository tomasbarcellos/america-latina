input <- list()
input$numero <- c(4,6)

ui <- shiny::pageWithSidebar(
  headerPanel("Teste"),
  sidebarPanel(
    sliderInput("numero", "Numero", 1, 10, c(1,4))
  ),
  mainPanel(
    conditionalPanel(
      condition = "input.numero[0] == input.numero[1]",
      leafletOutput("dinamico")
    ),
    
    conditionalPanel(
      condition = "input.numero[0] != input.numero[1]",
      plotlyOutput("dinamico2")
    )      
  )
)

server <- shinyServer(function(input, output) {
  output$dinamico <- renderLeaflet({
      leaflet() %>% addTiles()
    })
  
  output$dinamico2 <- renderPlotly({
      gg <- ggplot(mtcars, aes(mpg, disp)) + geom_point()
      ggplotly(gg)
    })
})

shinyApp(ui, server)
