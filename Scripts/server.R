# server.R
library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)

base <- readRDS("data/dados_dfJSON.rds")

vet_merc <- c("Máquinas elétricas", "Reatores nucleares e caldeiras", "Veículos (exc. trens)",
                "Combustíveis e óleos minerais", "Plástico", "Material fotográfico", "Químicos orgânicos",
                "Artigos de ferro ou aço", "Ferro ou aço", "Não especificado", "Pneu", "Papel",
                "Produtos farmaceutivos", "Alumínio", "Produtos químicos variados", "Cereais", "Carne",
                "Oleaginosas e castanhas", "Móveis", "Bronze", "Artigos metálicos", "Óleos essenciais",
                "Ferramentas e implementos", "Taninos", "Laticíneos, ovos e mel", "Químicos inorgânicos",
                "Brinquedos e jogos", "Artigos de vestuário de malha", "Resíduos da indústria alimentícia",
                "Artigos de vestuário, exceto de malha", "Fertilizantes", "Madeira e seus artigos",
                "Preparados alimentares diversos", "Algodão", "Vidro", "Pérolas e pedras preciosas", "Bebidas",
                "Frutas e nozes", "Tecidos impregnados", "Locomotivas", "Filamento sintéticos",
                "Poupa de madeira", "Couro e peles", "Açúcares", "Substancia albuminóides", "Sabão e detergentes",
                "Artigos de couro", "Manufaturados diversos", "Minérios , escórias e cinzas", "Livros e jornais",
                "Pastas, feltros e falsos tecidos", "Artigos de pedra", "Produtos de cerâmica", "Tecidos de malha",
                "Fibras sintéticas", "Preparados de vegetais e frutas", "Peixes e crustáceos", "Outros artigos têxteis",
                "Sal", "Cacau", "Bens fotográficos", "Produtos farináceos", "Reglógios", "Vegetais comestíveis",
                "Preparados de carne ou peixe", "Tecidos especiais", "Aviões e suas partes",
                "Explosivos", "Outros metais básicos", "Carpet", "Café", "Animais vivos", "Níquel",
                "Produtos de origem animal", "Zinco", "Armas e munição", "Máquina chapeleira?",
                "Laca e goma", "Chubo", "Plantas vivas", "Instrumentos musicais", "Navios e barcos",
                "Estanho", "Tabaco", "Algodão", "Cortiça", "Peles artificiais", "Manufaturas de palha",
                "Outras fibras vegetais", "Artigos preparados com penas", "Seda", "Guarda-chuva",
                "Vegetais de cestaria", "Obras de arte", "erro1", "erro2", "erro3", rep("erro", 50))

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
       theme_wsj(base_size = 14) +
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
                                         Valor = round(sum(TradeValue)/10^9, digits = 1))
      
      por_merc <- cbind(por_merc, merc = vet_merc[1:nrow(por_merc)], stringsAsFactors = F) %>%
        ungroup() %>% arrange(desc(Valor))
      
      ggplot(data = por_merc[1:input$qt_merc, ], aes(x = reorder(merc, Valor), y = Valor)) +
        geom_bar(stat = 'identity') +
        geom_text(aes(label = format(x = Valor, decimal.mark = ",")),
                  hjust = 1.1, col = 'white', size = 6) +
        theme_economist(base_size = 15) +
        scale_fill_economist() +
        ggtitle(paste(input$tipo,"de", input$pais, "\nem", input$ano)) +
        coord_flip()
    })
    
  }
)