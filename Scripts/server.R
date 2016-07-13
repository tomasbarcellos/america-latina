# server.R
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)

base <- readRDS("data/dados_dfJSON.rds")

shinyServer(
  function(input, output) {
    
    output$graf1 <- renderPlot({
      tipo <- switch(input$tipo,
                     'Exportacao' = '2',
                     'Importacao' = '1',
                     'Exportacao e Importacao' = '3') # esta errado, mas é para testar
      
      por_pais <- base %>% filter(rgCode == tipo, ptTitle == "World", yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>% summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
        arrange(desc(Valor))
      
     ggplot(data = por_pais[1:input$quant, ], aes(x = reorder(rtTitle, Valor), y = Valor)) +
       geom_bar(stat = 'identity') +
       geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                 hjust = 1.1, col = 'white', size = 6) +
       guides(fill = 'none') +
       ggtitle(paste("Comércio exterior para os países selecionados\n", input$tipo)) +
       theme_wsj(base_size = 15) +
       theme(axis.text.y = element_text(size = 9, face = 'bold', hjust = 1)) +
       coord_flip() +
       labs(x = '', y = "Bilhões de US$", fill = '')

    })
    
    output$graf2 <- renderPlot({
      tipo <- switch(input$tipo,
                     'Exportacao' = '2',
                     'Importacao' = '1',
                     'Exportacao e Importacao' = '3') # esta errado, mas é para testar
      
      por_merc <- base %>% 
        filter(rgCode == tipo, ptTitle == "World", yr == as.character(input$ano), rtTitle == input$pais) %>%
        group_by(cmdDescE) %>% summarise(codigo = first(cmdCode),
                                         Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
        ungroup() %>% arrange(desc(Valor))
      
      ggplot(data = por_merc[1:10, ], aes(x = reorder(cmdDescE, Valor), y = Valor)) +
        geom_bar(stat = 'identity') +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  vjust = 1.1, col = 'white', size = 6) +
        theme_economist(base_size = 15) +
        scale_fill_economist() +
        ggtitle(paste(input$tipo,"de", input$pais, "\nem", input$ano))
    })
    
  }
)