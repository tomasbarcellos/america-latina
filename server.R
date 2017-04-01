# server.R

shinyServer(
  function(input, output) {
    
    # Comércio exterior por país
    output$titulo1 <- renderText(paste("Comercio exterior para os paises selecionados\n", input$tipo))
    
    output$mapa <- renderLeaflet({
      por_pais <- base %>% 
        filter(grepl(x = rgCode, pattern = input$tipo),
               ptTitle == "World",
               rtTitle %in% input$quant,
               yr == as.character(input$ano)) %>%
        group_by(rtTitle) %>%
        summarise(ISO3 =  first(rt3ISO),
                  Valor = round(sum(TradeValue)/10^9, digits = 1),
                  etiqueta = paste0(": US$", Valor, " Bi")) %>%
        ungroup()
      
      formas <- sp::merge(shapes, por_pais)
      
      formas$etiqueta[is.na(formas$Valor)] <- ": Sem informações"
      
      formas %>% leaflet() %>% 
        addProviderTiles(providers$OpenMapSurfer) %>% 
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.2,
                    opacity = 1.0, fillOpacity = 0.9,
                    label = ~paste0(NAME, etiqueta),
                    fillColor = ~colorQuantile("Greens", Valor, probs = seq(0, 1, 0.2))(Valor),
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE)) %>%
        setView(lng = -75, lat = -15, zoom = 2)
    })
    
    output$graf1 <- renderPlotly({
      
      por_pais <- base %>% 
        filter(grepl(x = rgCode, pattern = input$tipo),
               ptTitle == "World",
               yr == as.character(input$ano),
               rtTitle %in% input$quant) %>%
        group_by(rtTitle) %>% summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
        arrange(desc(Valor))
      
      graf_pais <- ggplot(data = por_pais, aes(x = reorder(rtTitle, Valor), y = Valor)) +
        geom_bar(stat = 'identity', fill = 'indianred', alpha = 0.9) +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = 5.1, col = 'black', size = 6) +
        guides(fill = 'none') +
        theme_bw(base_size = 14) +
        theme(axis.text.y = element_text(size = 9, face = 'bold', hjust = 1)) +
        labs(x = '', y = "Bilhoes de US$", fill = '') 
      
      ggplotly(graf_pais)
    })
    
    # Balança comercial detalhada
    output$titulo2 <- renderText(paste(input$tipo,"de", input$pais, "\nem", input$ano))
    
    output$graf2 <- renderPlotly({
      
      por_merc <- base %>% 
        filter(grepl(x = rgCode, pattern = input$tipo),
               ptTitle == "World",
               yr == as.character(input$ano),
               rtTitle == input$pais) %>%
        group_by(cmdCode) %>% summarise(Mercadoria = first(cmdDescEPt),
                                        Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
        arrange(desc(Valor)) %>%
        ungroup() %>% mutate(soma_acu = cumsum(Valor), percentual = soma_acu*100/sum(Valor))
      
      graf_merc <- ggplot(data = por_merc %>% filter(percentual <= input$qt_merc), aes(x = reorder(Mercadoria, Valor), y = Valor)) +
        geom_bar(stat = 'identity', fill = 'indianred', alpha = 0.9) +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = -4.1, col = 'black', size = 6) +
        theme_bw(base_size = 15) +
        theme(axis.text.y = element_text(size = 8, face = 'bold')) +
        labs(x = " ", y = "Volume de comercio, em bilhoes de US$") +
        coord_flip()
      
      ggplotly(graf_merc)
    })
    
    # Preços das principais mercadorias
    output$titulo3 <- renderText(paste("Precos de ",paste(input$mercadoria, collapse = " e "), sep = ""))
    output$graf3 <- renderPlotly({
      precos.dim <- precos %>% filter(Mercadoria %in% input$mercadoria, 
                                      Ano >= input$periodo[1], Ano <= input$periodo[2])
      graf_precos <- ggplot(precos.dim, aes(x = Ano, y = preco)) + 
        geom_line(data = precos.dim,
                  aes(col = Mercadoria), alpha = 0.9, size = 2) +
        theme_bw() +
        # scale_fill_discrete() +
        labs(x = " ", y = "Índice (2010 = 100)")
      
      ggplotly(graf_precos) %>%
        layout(legend = list(orientation = 'h'))
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
      # seletor <- capitais %>% filter(ano == max(ano)) %>% arrange(desc(valor)) %>% select(pais) %>% unique()
      # paises.capitais <- input$paises.capitais[1]:input$paises.capitais[2]      
      
      graf_capitais <- ggplot(data = capitais %>% filter(variavel == var,
                                                         pais %in% input$paises.capitais #seletor$pais[paises.capitais]
                                                         ),
                              aes(x = ano , y = valor, col = reorder(pais, valor))) +
        geom_line(size = 2) +
        theme_bw() +
        scale_color_discrete() +
        # scale_x_discrete(limit = input$) +
        labs(x = " ", y = "Bilhões de dólares")
      
      ggplotly(graf_capitais) %>%
        layout(legend = list(orientation = 'h'))
    })
    
    # Termos de troca
    output$graf_termos <- renderPlotly({
      dado <- termos_troca %>% 
        filter(Rubro == input$termos_var,
               País %in% input$termos_pais)
      
      ggplot(dado, aes(x = as.numeric(Años_desc), 
                       y = valor, col = País_desc)) +
        geom_line(size = 2) +
        theme_bw() +
        # scale_x_discrete() +
        labs(x = " ", y = "Índice (2010 = 100)")
      
      ggplotly() %>%
        layout(legend = list(orientation = 'h'))
    })
    
    
    # Gráfico desemprego
    output$graf_desemprego <- renderPlotly({
      dado <- desemprego %>% filter(Sexo == input$genero.desemprego,
                                    between(Años_desc, input$periodo.desemprego[1],
                                            input$periodo.desemprego[2]),
                                    País_desc %in% input$paises.desemprego,
                                    `Escolaridad (EH)` == 1427)
      graf_des <- ggplot(dado, aes(x = Años_desc, y = valor, col = País_desc)) +
        geom_line(size = 2) +
        theme_bw() +
        # scale_x_discrete() +
        labs(x = " ", y = "Taxa de desemprego (%)")
      
      ggplotly(graf_des)  %>%
        layout(legend = list(orientation = 'h'))
    })
    
    # Gráfico de greves
    output$graf_greves <- renderPlotly({
      vertical <- switch(input$greve.indicador,
                         "Number of strikes and lockouts by economic activity null" = "Número de greves",
                         "Days not worked due to strikes and lockouts by economic activity null" = "Dias nao trabalhados (em razão de greves)",
                         "Workers involved in strikes and lockouts by economic activity (thousands)" = "Trabalhadores envolvidos em greves",
                         "Days not worked per 1000 workers due to strikes and lockouts by economic activity null" = "Dias não trabalhados (em razão de greves)\n por 1000 trabalhadores")
      
      dado_greve <- greves %>%
        filter(indicator.label == input$greve.indicador,
               ref_area.label %in% input$greve.paises,
               time > input$greve.anos[1],
               time < input$greve.anos[2]) %>%
        group_by(ref_area.label, time) %>%
        summarise(Pais = first(ref_area.label),
                  ano = first(time),
                  dado = sum(obs_value))
      
      graf_greve <- ggplot(dado_greve,
                           aes(ano, dado, color = Pais)) +
        geom_line(size = 2) + 
        theme_bw() +
        labs(x = " ", y = vertical)
      ggplotly(graf_greve) %>%
        layout(legend = list(orientation = 'h'))
    })
    
    # Grafico expansao agricola
    output$graf_fronteira <- renderPlotly({
    vertical <- switch(input$var.fronteira,
                       "plantada" = "Hectares",
                       "colhida" = "Hectares",
                       "quantidade" = "Toneladas",
                       "valor" = "Milhares de R$")
      
    dado_fronteira <- fronteira %>% filter(ano %in% input$periodo.fronteira[1]:input$periodo.fronteira[2])
    dado_fronteira <- dado_fronteira[ , c("ano", input$var.fronteira, "cultura")]
    names(dado_fronteira)[2] <- "y"
    graf_front <- ggplot(dado_fronteira, aes(ano, y, col = cultura)) +
      geom_path(size = 2) + 
      theme_bw() +
      labs(y = vertical)
    
    ggplotly(graf_front) %>%
      layout(legend = list(orientation = 'h'))
    })
    
    output$popup <- renderUI({
      bsModal("modalExample", paste0("Data for Row Number: ", 1), "", size = "large",
              column(12, "Ok")
      )
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