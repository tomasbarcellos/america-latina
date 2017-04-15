library(rgdal)

url <- 'http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip'

td <- tempdir()

tf <- tempfile(fileext = ".zip")

download.file(url, tf)

unzip(tf, exdir = td)

file.remove(tf); file.remove(dir(td, full.names = TRUE)[1])

shape <- readOGR(dsn = td)

AL <- c('ARG', 'BHS', 'BLZ', 'BOL', 'BRA', 'CHL', 'COL', 'CRI', 'CUB',
        'DOM', 'ECU', 'SLV', 'GUF', 'GTM', 'GUY', 'HTI', 'HND', 'JAM','MEX',
        'ABW', 'SUR','NIC', 'PRY', 'PER', 'PAN', 'PRI', 'TTO', 'URY', 'VEN')

shape_AL <- subset(shape, shape$ISO3 %in% AL,
                   c("ISO3", "NAME", "AREA", "LON", "LAT"))

saveRDS(shape_AL, 'dados/shapes.RDS')
