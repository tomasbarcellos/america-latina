# server.R

shinyServer(
  function(input, output) {
    
    # Comércio exterior por país
    output$mapa <- renderLeaflet({
      if (input$mapa_merc == "Total") {
        mercadoria <- TRUE
      } else {
        mercadoria <- base$pai_desc == input$mapa_merc
      }
      
      por_pais <- base %>% 
        filter(grepl(x = rgCode, pattern = input$tipo),
               rtTitle %in% input$quant,
               yr == as.character(input$ano), mercadoria) %>%
        group_by(rtTitle) %>%
        summarise(ISO3 =  first(rt3ISO),
                  Valor = round(sum(TradeValue)/10^6, digits = 1),
                  etiqueta = paste0(": US$ ",
                                    format(Valor, big.mark = ".", decimal.mark = ","),
                                    " Mi")) %>%
        ungroup()
      
      por_pais <- por_pais %>% mutate(cor = if (n() <= 2) {
        rep("#238B45", n())
      } else {
        colorBin("Greens", por_pais$Valor, 5)(por_pais$Valor)
      })
      
      formas <- sp::merge(shapes, por_pais)
      
      formas$etiqueta[is.na(formas$Valor)] <- ": Sem informações"
      
      subset(formas, formas$rtTitle %in% input$quant) %>% leaflet() %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(color = "#555", weight = 1, smoothFactor = 0.2,
                    opacity = 1.0, fillOpacity = 1,
                    label = ~paste0(NAME, etiqueta),
                    labelOptions = labelOptions(textsize = "15px"),
                    fillColor = ~cor, 
                    highlightOptions = highlightOptions(color = "#808080", weight = 2,
                                                        bringToFront = TRUE)) %>%
        setView(lng = -75, lat = -4, zoom = 3) %>% 
        addLegend("bottomleft", pal = colorBin("Greens", por_pais$Valor, 5), values = ~Valor,
                  labFormat = labelFormat(big.mark = ".", between = " a ",
                                          prefix = "US$ ", suffix = " Bi", digits = 1,
                                          transform = function(x) x/1000),
                  opacity = 1)
    })
    
    # Balança comercial detalhada
    output$graf2 <- renderChart2({
      chart <- base %>% 
        filter(grepl(x = rgCode, pattern = input$tipo2),
               between(as.numeric(as.character(yr)), input$ano2[1], input$ano2[2]) ,
               rtTitle == input$pais) %>%
        group_by(yr, Mercadoria = pai_desc) %>%
        summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
        hPlot(data = ., y = "Valor", x = "yr", group = "Mercadoria", type = "area",
              title = "Volume de comércio, em bilhões de US$")
      chart$plotOptions(area = list(stacking = 'normal', lineColor = '#d8d8d8',
                                    lineWidth = 2),
                        marker = list(enabled = 0), replace = TRUE)
      chart
    })
    
    # Balança de capitais
    output$graf4 <- renderChart2({
      if (input$capitais == "Total") {
        rubricas <- unique(bal_pag$Rubrica)
      } else {
        rubricas <- input$capitais
      }
      
      bal_pag %>% filter(Rubrica %in% rubricas,
                         Pais %in% input$paises.capitais,
                         between(Ano, input$capitais_periodo[1],
                                 input$capitais_periodo[2])) %>% 
        mutate(valor = round(valor, 1)) %>% 
        hPlot(data = ., y = "valor", x = "Ano", group = "Pais", 
              type = "line", title = "Conta corrente")
    })
    
    # Termos de troca
    output$graf_termos <- renderChart2({
      termos_troca %>%
        filter(Rubro == input$termos_var,
               between(Años_desc, input$termos_periodo[1],
                       input$termos_periodo[2]),
               País %in% input$termos_pais) %>% 
        mutate(Ano = as.numeric(Años_desc),
               País = País_desc, `Índice (2010 = 100)` = round(valor, 1)) %>% 
        hPlot(data = ., x = "Ano", y = "Índice (2010 = 100)",
              type = "line", group = "País")
    })
    
    # Gráfico desemprego
    output$graf_desemprego <- renderChart2({
      desemprego %>% filter(Sexo == input$genero.desemprego,
                            between(Años_desc, input$periodo.desemprego[1],
                                    input$periodo.desemprego[2]),
                            País_desc %in% input$paises.desemprego,
                            `Escolaridad (EH)` == 1427) %>%
        mutate(Ano = as.numeric(Años_desc),
               País = País_desc, `Taxa de desemprego (%)` = valor) %>% 
        hPlot(data = ., x = "Ano", y = "Taxa de desemprego (%)",
              type = "line", group = "País")
    })
    
    # Gráfico de greves
    output$graf_greves <- renderChart2({
      vertical <- switch(input$greve.indicador,
                         "Number of strikes and lockouts by economic activity null" = "Número de greves",
                         "Days not worked due to strikes and lockouts by economic activity null" = "Dias não trabalhados (em razão de greves)",
                         "Workers involved in strikes and lockouts by economic activity (thousands)" = "Trabalhadores envolvidos em greves",
                         "Days not worked per 1000 workers due to strikes and lockouts by economic activity null" = "Dias não trabalhados (em razão de greves)\n por 1000 trabalhadores")
      
      greves %>% filter(indicator.label == input$greve.indicador,
                        ref_area.label %in% input$greve.paises,
                        between(time, input$greve.anos[1], input$greve.anos[2])) %>%
        group_by(ref_area.label, time) %>%
        summarise(País = first(ref_area.label),
                  Ano = first(time),
                  Valor = sum(obs_value)) %>% 
        hPlot(data = ., type = "line", x = "Ano", y = "Valor",
              group = "País", title = vertical)
    })
    
    # Grafico expansao agricola
    output$graf_fronteira <- renderChart2({
      dado <- fronteira %>% filter(Produto == input$var.fronteira,
        País %in% input$paises.fronteira,
        between(Ano, input$periodo.fronteira[1],
                input$periodo.fronteira[2])) %>% 
        select(Produto, Ano, País, Valor) %>% 
      hPlot(data = ., type = "line", x = 'Ano', y = 'Valor', group = 'País')
    })
    
  }
)