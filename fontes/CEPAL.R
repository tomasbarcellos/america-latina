library(RCEPAL)
termos_troca <- CEPAL_stat(883)

desemprego <- CEPAL_stat(119)

desnutricao <- CEPAL_stat(164) # precisa tirar ano como num na funcao


saveRDS(termos_troca, 'dados/termos_troca.RDS')
saveRDS(desemprego, 'dados/desemprego.RDS')
rm(list=ls())
