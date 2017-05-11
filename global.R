# global.R

library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(stringr)
library(sp)
library(leaflet)
library(rCharts)

base <- readRDS("dados/comercioAL_mundo.RDS") %>%
  select(pai_desc, rgCode, ptTitle, rtTitle, yr, rt3ISO, TradeValue)
precos <- readRDS("dados/precos_commodities.rds")
desemprego <- readRDS('dados/desemprego.RDS')
greves <- readRDS('dados/greves.RDS')
fronteira <- readRDS('dados/fronteira_agri_AL.RDS')
termos_troca <- readRDS("dados/termos_troca.RDS")
shapes <- readRDS('dados/shapes.RDS')
bal_pag <- readRDS('dados/BP_AL.RDS')

options(scipen = 9e4, shiny.fullstacktrace = TRUE)

base$rtTitle <- as.character(base$rtTitle)

names(precos)[3] <- 'preco'

