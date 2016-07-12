# Retire o jogo da velha caso não possua os pacoques listados abaixo:
# install.packages("ggplot2")

library(dplyr)
library(ggplot2)

precos <- readRDS("precos_commodities.rds")
export <- readRDS("dados_dfJSON.rds")

# Verifica quais colunas são inuteis (apenas NAs)
str(export)
# Seleciona apenas dados de exportação
export <- export %>% filter(Trade.Flow == "Export")
# Organiza dados por valor decrescente
export <- export %>% arrange(desc(Trade.Value..US..))


ven <- export %>% filter(Reporter == "Venezuela", 
                         Partner == "World",
                         Year == 2013)

# Verifica tamanho do objeto
object.size(export)/10^6 # em Mb

# Tabela com volume de exportação por categoria de mercadoria em 2015
por_merc_2015 <- export %>% 
  group_by(Commodity) %>%
  filter(Partner == "World", Year == 2015) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

por_merc_pais_2015 <- export %>% 
  group_by(Commodity, Reporter) %>%
  filter(Partner == "World", Year == 2015) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

por_parc_2015 <- export %>%
  group_by(Partner) %>%
  filter(Year == 2015) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

# Cria uma tabela da exportação por pais para cada ano
por_pais_2011 <- export %>% 
  group_by(Reporter) %>%
  filter(Partner == "World", Year == 2011) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

por_pais_2012 <- export %>% 
  group_by(Reporter) %>%
  filter(Partner == "World", Year == 2012) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

por_pais_2013 <- export %>% 
  group_by(Reporter) %>%
  filter(Partner == "World", Year == 2013) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange((Valor))

por_pais_2014 <- export %>% 
  group_by(Reporter) %>%
  filter(Partner == "World", Year == 2014) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

por_pais_2015 <- export %>% 
  group_by(Reporter) %>%
  filter(Partner == "World", Year == 2015) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange(desc(Valor))

# Ve anos em que há mais países.
data.frame(onze = length(unique(por_pais_2011$Reporter)),
           doze = length(unique(por_pais_2012$Reporter)),
           treze = length(unique(por_pais_2013$Reporter)),
           quatorze = length(unique(por_pais_2014$Reporter)),
           quinze = length(unique(por_pais_2015$Reporter)))
# 2013 e 2014 tem mesma quantidade, porém venezuela (em geral 2ª exportadora) está fora de 2014
por_pais_2013$Reporter <- factor(x = por_pais_2013$Reporter,levels = por_pais_2013$Reporter,ordered = T)

require(scales)
gg <- ggplot(por_pais_2013, aes(x = Reporter,y =  Valor, fill = Reporter))
gg + geom_bar(stat = "identity") +  
  geom_text(aes(label=round(Valor/10e6,2)), position= position_dodge(width=0.9), vjust=-.5, color="black") + 
  coord_flip() + 
  labs(ylab = seq(0,25*10^9, 5*10^9), ggtitle()) + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)



# # Cria tabela com as 5 mais exportadas e a organiza
# dez_mais <- por_merc_2015[1:10, ]
# names(dez_mais)[1] <- "Mercadoria"
# dez_mais$Mercadoria <- factor(dez_mais$Mercadoria, ordered = T)
# 
# # Gráfico demonstrando itens mais exportados
# g <- ggplot(dez_mais, aes(Mercadoria, Valor))
# 
# g + geom_bar(stat = "identity", aes(fill = Mercadoria)) +
#   scale_fill_brewer(palette = "Reds") +
#   coord_flip() +
#   theme(legend.position = "bottom")
# 
# gg <- ggplot(data = por_merc_2015[1:5, ], aes(Commodity, Valor))
# 
# gg +   
# stat_summary(data = por_merc_2015[1:5, ],aes(x = Commodity, y = Valor, fill = Commodity), geom = "bar")
