## import des contours administratifs commune et noms départements

library(COGugaison)
library(sf)
library(dplyr)

COMM_sf <- sf::st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/communes-avec-outre-mer.geojson")
st_crs(COMM_sf)
colnames(COMM_sf)
COMM_sf <- st_make_valid(COMM_sf)
geo_communes <- COMM_sf %>%
  select(INSEE_COM = code,  NOM_COM = nom) %>%
  mutate(INSEE_DEP = case_when(substr(INSEE_COM,1,2) %in% "97" ~ substr(INSEE_COM,1,3) ,
                               TRUE ~ substr(INSEE_COM,1,2))) %>%
  left_join(libelles_supracom_2020 %>%
              filter(NIVGEO %in% "DEP") %>%
              select(INSEE_DEP = CODGEO, NOM_DEP = LIBGEO),
            by = "INSEE_DEP")


rm(COMM_sf)

usethis::use_data(geo_communes, overwrite = TRUE)


