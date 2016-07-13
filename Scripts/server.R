# server.R

library(shiny)
library(dplyr)
library(ggplot2)

base <- readRDS("data/dados_dfJSON.rds")

shinyServer(
  function(input, output) {
    
    output$graf <- renderPlot({
      tipo <- switch(input$tipo,
                     "Exportacao" = "2",
                     "Importacao" = "1",
                     "Exportacao e Importacao" = "3") # esta errado, mas é para testar
      
      dado <- base %>% filter(rgCode == tipo, ptTitle == "World", yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>% summarise(Valor = sum(TradeValue)) %>% ungroup() %>%
        arrange(desc(Valor))
      
     ggplot(data = dado[1:input$quant, ], aes(x = rtTitle, y = Valor)) +
       geom_bar(stat = "identity", fill = colors()[1:input$quant]) +
       geom_text(aes(label = format(x = round(Valor/10^9, digits = 1), small.mark = "\\,")), vjust = -1.0) +
       scale_y_continuous(name = "Valor bilhões de US $",limits = c(0, max(dado$Valor)*1.05),
                          breaks = seq(0, max(dado$Valor), max(dado$Valor)/4)) +
       scale_x_discrete(name = "País") +
       labs(title = "Comércio exterior para os países selecionados")

    })
    
  }
)