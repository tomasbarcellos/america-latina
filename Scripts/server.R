# server.R

base <- readRDS("data/dados_dfJSON.rds")

precos <- readRDS("data/precos_commodities.rds")
names(precos)[3] <- 'preco'

shinyServer(
  function(input, output) {
    output$titulo1 <- renderText(paste("Comércio exterior para os países selecionados\n", input$tipo))
    
    output$graf1 <- renderPlotly({
      tipo <- switch(input$tipo,
                     'Exportacao' = '[2|3]',
                     'Importacao' = '[1|4]',
                     'Exportacao e Importacao' = '[1-4]')
      
      por_pais <- base %>% filter(grepl(x = rgCode, tipo), ptTitle == "World", yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>% summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
        arrange(desc(Valor))
      
      graf.pais <- ggplot(data = por_pais[1:input$quant, ], aes(x = reorder(rtTitle, Valor), y = Valor)) +
        geom_bar(stat = 'identity') +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = 5.1, col = 'black', size = 6) +
        guides(fill = 'none') +
        theme_wsj(base_size = 14) +
        theme(axis.text.y = element_text(size = 9, face = 'bold', hjust = 1)) +
        coord_flip() +
        labs(x = '', y = "Bilhões de US$", fill = '')
      ggplotly(p = graf.pais)
    })
    output$titulo2 <- renderText(paste(input$tipo,"de", input$pais, "\nem", input$ano))
    
    output$graf2 <- renderPlotly({
      tipo <- switch(input$tipo,
                     'Exportacao' = '[2|3]',
                     'Importacao' = '[1|4]',
                     'Exportacao e Importacao' = '[1-4]')
      
      por_merc <- base %>% 
        filter(grepl(x = rgCode, tipo), ptTitle == "World", yr == as.character(input$ano), rtTitle == input$pais) %>%
        group_by(cmdCode) %>% summarise(Mercadoria = first(cmdDescEPt),
                                         Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
        arrange(desc(Valor))
      
      graf.merc <- ggplot(data = por_merc[1:input$qt_merc, ], aes(x = reorder(Mercadoria, Valor), y = Valor)) +
        geom_bar(stat = 'identity') +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = -4.1, col = 'black', size = 6) +
        theme_economist(base_size = 15) +
        scale_fill_economist() +
        coord_flip()
      ggplotly(graf.merc)
  
    })
    
    output$titulo3 <- renderText(paste("Preços de ",paste(input$mercadoria, collapse = " e "), sep = ""))
    output$graf3 <- renderPlotly({
      precos.dim <- precos %>% filter(Mercadoria == input$mercadoria, 
                                      Ano >= input$periodo[1], Ano <= input$periodo[2])
      graf.precos <- ggplot(precos, aes(x = Ano, y = preco)) + 
        geom_line(data = precos.dim,
                  aes(col = Mercadoria), alpha = 0.7) +
        theme_stata() +
        scale_fill_continuous()
      
      ggplotly(graf.precos)
    })
    output$download.graf1 <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(por_pais, con)
       }
      )
    output$download.graf2 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(por_merc, con)
      }
    )
    output$download.graf3 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(precos.dim, con)
      }
    )
  }
)