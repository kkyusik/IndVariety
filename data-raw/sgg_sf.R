library(sf)
library(dplyr)
sgg.sf <- read_sf(dsn = "data-raw", layer = "SG_2015")
sgg.sf <- sgg.sf %>% select(sg_code, geometry)
usethis::use_data(sgg.sf)
