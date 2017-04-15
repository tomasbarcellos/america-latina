library(RCEPAL)
termos_troca <- CEPAL_stat(883)

desemprego <- CEPAL_stat(119)

saveRDS(termos_troca, 'dados/termos_troca.RDS')
saveRDS(desemprego, 'dados/desemprego.RDS')
rm(list=ls())
