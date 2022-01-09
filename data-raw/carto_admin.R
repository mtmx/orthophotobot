## import des contours administratifs commune et noms départements

library(CARTElette)
library(COGugaison)
library(sf)
library(dplyr)

COMM_sf <- charger_carte(COG=2020, nivsupra = "COM")

st_crs(COMM_sf)
colnames(COMM_sf)

geo_communes <- COMM_sf %>%
  select(INSEE_COM, INSEE_DEP, NOM_COM) %>%
  left_join(libelles_supracom_2020 %>%
              filter(NIVGEO %in% "DEP") %>%
              select(INSEE_DEP = CODGEO, NOM_DEP = LIBGEO),
            by = "INSEE_DEP")


rm(COMM_sf)

usethis::use_data(geo_communes, overwrite = TRUE)


