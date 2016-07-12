# server.R

library(shiny)
library(dplyr)
library(ggplot2)

base <- readRDS("data/dados_dfJSON.rds")
paises <- as.character(unique(base$ptTitle))
shinyServer(
  function(input, output) {
    
    output$graf <- renderPlot({
      tipo <- switch(input$tipo,
                     "Exportacao" = "2",
                     "Importacao" = "1",
                     "Exportacao e Importacao" = "3") # esta errado, mas é para testar
      
      dado <- base %>% filter(rgCode == tipo, ptTitle == "World", yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>% summarise(Valor = sum(TradeValue)) %>% ungroup() %>% arrange(desc(Valor))
      
     ggplot(data = dado[1:input$quant, ], aes(x = rtTitle, y = Valor)) +
       geom_bar(stat = "identity", fill = colors()[1:input$quant]) +
       geom_text(aes(label = format(x = round(Valor/10^6, digits = 2), big.mark = ".", small.mark = ","), vjust = -0.5))

    })
    
  }
)