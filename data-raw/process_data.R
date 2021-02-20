# Getting spatial data 
library(sf)
library(dplyr)

# Shapefile: filter to Moramanga District
mada_communes <- read_sf("data-raw/mada_communes/mdg_admbnda_adm3_BNGRC_OCHA_20181031.shp")
mora_communes <- filter(mada_communes, ADM2_EN == "Moramanga")
mora_communes$id <- 1:nrow(mora_communes)

# pop data from census
# link to release: https://www.instat.mg/recensement-general-de-la-population-et-de-lhabitat-rgph/resultats-definitifs-rgph-3/
# and tome 2 pg. 67: https://www.instat.mg/wp-content/uploads/Resultat-globaux-RGPH3-Tome-02.pdf
pop <- c(14836L, 6788L, 15835L, 17545L, 8823L, 13206L, 35923L, 7566L, 7679L, 17418L, 
         9219L, 12755L, 14493L, 7339L, 10374L, 9023L, 20380L, 
         21609L, 57084L, 19656L, 6036L, 13601L, 3536L)
census_name <- c("Fierenana", "Moramanga", "Mandialaza", "Morarano Gara", 
                 "Ampasipotsy Mandialaza", "Ambohidronono", "Ambohibary", 
                 "Belavabary", "Anosibe Ifody", "Sabotsy Anjiro", 
                 "Vodiriana", "Antanandava", "Mangarivotra", 
                 "Ambatovola", "Andasibe", "Ampasipotsy Gara", 
                 "Beforona", "Lakato", "Moramanga",
                 "Andaingo", "Bembary", "Amboasary", "Analasoa")  

# census report error on page 68 (second, smaller Moramanga is likely Antanaditra)
# also reported Analasoa as a separate commune but considered in shapefile as part of Morarano Gara
# similarly for Bembary considered a part of Andaingo
commune_name <- c("Fierenana", "Antaniditra", "Mandialaza", "Morarano Gara", 
                  "Ampasipotsy Mandialaza", "Ambohidronono", "Ambohibary", 
                  "Belavabary", "Anosibe Ifody", "Sabotsy Anjiro", 
                  "Vodiriana", "Antanandava", "Mangarivotra", 
                  "Ambatovola", "Andasibe", "Ampasimpotsy Gara", 
                  "Beforona", "Lakato", "Moramanga",
                  "Andaingo", "Andaingo", "Amboasary", "Analasoa")   
pop_data <- tibble(pop = pop, commune = commune_name, census_name = census_name)

pop_data %>%
  group_by(commune) %>%
  summarise(pop = sum(pop)) -> pop_data

# join to sf & fix names 
mora_communes %>%
  left_join(pop_data, by = c("ADM3_EN" = "commune")) %>%
  select(district = ADM2_EN, commune = ADM3_EN, commcode = ADM3_PCODE, pop) -> pop_sf

# write out to data
st_write(pop_sf, "data/shapefile/mora_communes.shp", append = FALSE)
