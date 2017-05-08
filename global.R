# global.R

library(shiny)
library(shinydashboard)
# library(shinyBS)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(ggthemes)
library(stringr)
# library(plotly)
library(sp)
library(leaflet)
library(rCharts)

base <- readRDS("dados/comercioAL.RDS")
precos <- readRDS("dados/precos_commodities.rds")
capitais <- readRDS("dados/capitais_AL.rds")
desemprego <- readRDS('dados/desemprego.RDS')
greves <- readRDS('dados/greves.RDS')
fronteira <- readRDS('dados/fronteira_agri_AL.RDS')
termos_troca <- readRDS("dados/termos_troca.RDS")
shapes <- readRDS('dados/shapes.RDS')
# reservas <- readRDS('dados/reservas.RDS')
# SMN <- readRDS('dados/sal_min_nec.RDS')
# concentracao <- readRDS('dados/concentracao.RDS')
bal_pag <- readRDS('dados/BP_AL.RDS')

options(scipen = 9e4, shiny.fullstacktrace = TRUE)

base$rtTitle <- as.character(base$rtTitle)

names(precos)[3] <- 'preco'

# plot <- Highcharts$new()
# plot$chart(type = "spline")
# plot$series(data = )
# plot$yAxis("um texto")
