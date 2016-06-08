install.packages("dplyr")
install.packages("ggplot2")
library(dplyr)
library(ggplot2)

rm(list=ls())

export <- readRDS("dados_df.rds")

# Verifica quais colunas são inuteis (apenas NAs)
str(export)
# Elimina colunas inuteis
export <- export[ , -c(15:21, 25:31, 33:35)]
# Seleciona apenas dados de exportação
export <- export %>% filter(Trade.Flow == "Export")
# Organiza dados por valor decrescente
export <- export %>% arrange(desc(Trade.Value..US..))

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
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

por_merc_2013 <- export %>% 
  group_by(Commodity) %>%
  filter(Partner == "World", Year == 2013) %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange((Valor))
  
outras_merc_2013 <- por_merc_2013[1:132, ]
por_merc_2013 <- por_merc_2013[133:146, ]
por_merc_2013 <- rbind(data.frame(Commodity = "Outras",
                                  Valor = sum(outras_merc_2013$Valor)),
                       por_merc_2013)

por_merc_2013$Commodity <- factor(por_merc_2013$Commodity, ordered = TRUE)
str(por_merc_2013$Valor)
mm <- ggplot(por_merc_2013, aes(x = Commodity,y =  Valor, fill = Commodity))
mm + geom_bar(stat = "identity") +  
  geom_text(aes(label=round(Valor/10e6,2)), position= position_dodge(width=0.9), vjust=-.5, color="black") + 
  coord_flip() + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)


br_por_merc_2013 <- export %>% 
  group_by(Commodity) %>%
  filter(Partner == "World", Year == 2013, Reporter == "Brazil") %>%
  summarise(Valor = sum(Trade.Value..US..)) %>%
  ungroup() %>%
  arrange((Valor))

br_outras_merc_2013 <- br_por_merc_2013[1:83, ]
br_por_merc_2013 <- br_por_merc_2013[84:97, ]
br_por_merc_2013 <- rbind(data.frame(Commodity = "Outras",
                                  Valor = sum(br_outras_merc_2013$Valor)),
                       br_por_merc_2013)
br_por_merc_2013$Commodity <- factor(br_por_merc_2013$Commodity, ordered = TRUE)

bb <- ggplot(br_por_merc_2013, aes(x = Commodity,y =  Valor, fill = Commodity))
bb + geom_bar(stat = "identity") +  
  geom_text(aes(label=round(Valor/10e6,2)), position= position_dodge(width=0.9), vjust=-.5, color="black") + 
  coord_flip() + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)
