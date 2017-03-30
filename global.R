# global.R

library(shiny)
library(shinydashboard)
library(shinyBS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(stringr)
library(plotly)
library(feather)

base <- readRDS("dados/dados_dfJSON.rds")
precos <- readRDS("dados/precos_commodities.rds")
capitais <- readRDS("dados/capitais_AL.rds")
desemprego <- readRDS('dados/desemprego.RDS')
greves <- readRDS('dados/greves.RDS')
fronteira <- readRDS('dados/fronteira-agricola.RDS')
termos_troca <- readRDS("dados/termos_troca.RDS")

options(scipen = 9e4) #

base$rtTitle <- as.character(base$rtTitle)

tabela_por_pais <- base %>% filter(ptTitle == "World") %>%
  group_by(rtTitle, rgDesc, yr) %>% 
  summarise(Valor = round(sum(TradeValue)/10^9, digits = 1)) %>% ungroup() %>%
  arrange(desc(Valor))

tabela_por_merc <- base %>% 
  filter(ptTitle == "World") %>%
  group_by(cmdCode, rgDesc, yr, rtTitle) %>%
  summarise(Mercadoria = first(cmdDescEPt),
            Valor = round(sum(TradeValue)/10^9, digits = 1)) %>%
  arrange(desc(Valor))

names(precos)[3] <- 'preco'