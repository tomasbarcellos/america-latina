# server.R
base <- readRDS("dados/dados_dfJSON.rds")
precos <- readRDS("dados/precos_commodities.rds")
capitais <- readRDS("dados/capitais_AL.rds")
desemprego <- read_feather('dados/desemprego.feather')
greves <- readRDS('dados/greves.RDS')

tabela_por_pais <- base %>% filter(ptTitle == "World") %>%
  group_by(rtTitle, rgDesc, yr) %>% summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
  arrange(desc(Valor))

tabela_por_merc <- base %>% 
  filter(ptTitle == "World") %>%
  group_by(cmdCode, rgDesc, yr, rtTitle) %>% summarise(Mercadoria = first(cmdDescEPt),
                                                       Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
  arrange(desc(Valor))

names(precos)[3] <- 'preco'

shinyServer(
  function(input, output) {
    
    # Comércio exterior por país
    output$titulo1 <- renderText(paste("Comercio exterior para os paises selecionados\n", input$tipo))
    
    output$graf1 <- renderPlotly({
      tipo <- switch(input$tipo,
                     "Exportacao" = '[2|3]',
                     'Importacao' = '[1|4]',
                     'Exportacao e Importacao' = '[1-4]')
      
      por_pais <- base %>% filter(grepl(x = rgCode, tipo), ptTitle == "World", yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>% summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
        arrange(desc(Valor))
      
      graf.pais <- ggplot(data = por_pais[1:input$quant, ], aes(x = reorder(rtTitle, Valor), y = Valor)) +
        geom_bar(stat = 'identity', fill = 'indianred', alpha = 0.9) +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = 5.1, col = 'black', size = 6) +
        guides(fill = 'none') +
        theme_bw(base_size = 14) +
        theme(axis.text.y = element_text(size = 9, face = 'bold', hjust = 1)) +
        labs(x = '', y = "Bilhoes de US$", fill = '') #+
      coord_flip()
      ggplotly(p = graf.pais)
    })
    
    # Balança comercial detalhada
    output$titulo2 <- renderText(paste(input$tipo,"de", input$pais, "\nem", input$ano))
    
    output$graf2 <- renderPlotly({
      tipo <- switch(input$tipo,
                     'Exportacao' = '[2|3]',
                     'Importacao' = '[1|4]',
                     'Exportacao e Importacao' = '[1-4]')
      
      por_merc <- base %>% 
        filter(grepl(x = rgCode, tipo), ptTitle == "World", grepl(x = yr, input$ano), rtTitle == input$pais) %>%
        group_by(cmdCode) %>% summarise(Mercadoria = first(cmdDescEPt),
                                        Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
        arrange(desc(Valor)) %>%
        ungroup() %>% mutate(soma_acu = cumsum(Valor), percentual = soma_acu*100/sum(Valor))
      
      graf.merc <- ggplot(data = por_merc %>% filter(percentual <= input$qt_merc), aes(x = reorder(Mercadoria, Valor), y = Valor)) +
        geom_bar(stat = 'identity', fill = 'indianred', alpha = 0.9) +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = -4.1, col = 'black', size = 6) +
        theme_bw(base_size = 15) +
        labs(x = "", y = "Volume de comercio, em bilhoes de US$") +
        coord_flip()
      ggplotly(graf.merc)
      
    })
    
    # Preços das principais mercadorias
    output$titulo3 <- renderText(paste("Precos de ",paste(input$mercadoria, collapse = " e "), sep = ""))
    output$graf3 <- renderPlotly({
      precos.dim <- precos %>% filter(Mercadoria %in% input$mercadoria, 
                                      Ano >= input$periodo[1], Ano <= input$periodo[2])
      graf.precos <- ggplot(precos.dim, aes(x = Ano, y = preco)) + 
        geom_line(data = precos.dim,
                  aes(col = Mercadoria), alpha = 0.9) +
        theme_bw() +
        scale_fill_discrete()
      
      ggplotly(graf.precos)
    })
    
    # Balança de capitais
    output$graf4 <- renderPlotly({
      var = switch (input$capitais,
                    "Entrada liquida de capitais autonomos" = "(1) Net inflows of autonomous capital",
                    "Entrada liquida de capitais nao-autonomos" = "(2) Net inflows of non-autonomous capital",
                    "Total da entrada liquida de capital" = "(3) Total net inflows of capital = (1) + (2)",
                    "Balanca de rendas" = "(4) Income balance",
                    "Transferencias liquidas" = "(5) Net resource transfers = (3) + (4)"
      )
      seletor <- capitais %>% filter(ano == max(ano)) %>% arrange(desc(valor)) %>% select(pais) %>% unique()
      paises.capitais <- input$paises.capitais[1]:input$paises.capitais[2]      
      
      graf.capitais <- ggplot(data = capitais %>% filter(variavel == var,
                                                         pais %in% seletor$pais[paises.capitais]),
                              aes(x = ano , y = valor, col = reorder(pais, valor))) +
        geom_point() + geom_smooth(alpha = 0.7, se = FALSE) +
        theme_bw() +
        scale_color_discrete() +
        theme(legend.position = "bottom")
      ggplotly(graf.capitais)
    })
    
    output$graf.desemprego <- renderPlotly({
      switch(as.character(input$periodo.desemprego),
             '2005' = '29175',
             '2006' = '29176',
             '2007' = '29177',
             '2008' = '29178',
             '2009' = '29179',
             '2010' = '29180',
             '2011' = '29181',
             '2012' = '29182',
             '2013' = '29183',
             '2014' = '29184',
             '2015' = '29185')
      
      dado <- desemprego %>% filter(genero == input$genero.desemprego,
                                    ano %in% input$periodo.desemprego)
      graf.des <- ggplot(dado, aes(x = pais, y = valor, fill = ano)) +
        geom_bar(stat = "identity", position = "dodge") +
        theme_bw()
      
      ggplotly(graf.des)
    })
    
    # Gráfico de greves
    output$graf.graves <- renderPlotly({
      dado_greve <- greves %>%
        filter(indicator.label == input$greve.indicador) %>%
        group_by(ref_area.label, time) %>%
        summarise(Pais = first(ref_area.label),
                  ano = first(time),
                  dado = sum(obs_value))
      
      graf.greve <- ggplot(dado_greve,
                           aes(ano, dado, color = Pais)) +
        geom_line() + 
        theme_bw()
      ggplotly(graf.greve)
    })
    
    # Botões para download dos dados dos gráficos 
    output$download.graf1 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(tabela_por_pais, con)
      }
    )
    output$download.graf2 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(tabela_por_merc, con)
      }
    )
    output$download.graf3 <- downloadHandler(
      filename = function() {
        paste('data-', Sys.Date(), '.csv', sep='')
      },
      content = function(con) {
        write.csv(precos, con)
      }
    )
  }
)