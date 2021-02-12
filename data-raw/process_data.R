# Getting spatial data 
library(sf)
library(fasterize)
library(raster)
library(dplyr)

# Shapefile: filter to Moramanga District
mada_communes <- read_sf("data-raw/mada_communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mora_communes <- filter(mada_communes, ADM2_EN == "Moramanga")
mora_communes$id <- 1:nrow(mora_communes)

# pop raster: mask to Moramanga District
mada_pop <- raster("data-raw/wp_2015_1x1.tif")
mora_pop <- crop(mada_pop, mora_communes)
mora_pop <- mask(mora_pop, mora_communes)

# fasterize & aggregate 
mora_ids <- fasterize(mora_communes, mora_pop, field = "id")[]
pop_data <- data.frame(mora_ids, pop = mora_pop[])
pop_data %>%
  group_by(mora_ids) %>%
  summarize(pop = sum(pop, na.rm = TRUE)) -> pop_data

# join to sf & fix names 
mora_communes %>%
  left_join(pop_data, by = c("id" = "mora_ids")) %>%
  select(district = ADM2_EN, commune = ADM3_EN, commcode = ADM3_PCODE, pop) -> pop_sf

# write out to data
st_write(pop_sf, "data/shapefile/mora_communes.shp")
