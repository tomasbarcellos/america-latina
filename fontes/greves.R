# dados de greves
# dados baixados manualmente de ilostat
greves <- read.csv(file = 'ilostat-export.csv', stringsAsFactors = FALSE)
greves[, 13:20] <- NULL
greves$X <- NULL
greves$indicator <- NULL
greves$obs_value <- as.numeric(gsub(greves$obs_value,
                                    pattern = "\\.", replacement = ''))
greves <- greves[!is.na(greves$obs_value), ]
greves$time <- as.numeric(greves$time)
greves$id <- paste(greves$time, greves$ref_area.label)

saveRDS(greves, "greves.RDS")
