## code to prepare `poi` dataset goes here
library(dplyr)

# stades

base_sport_equip <- readxl::read_excel("../data/2020_Equipements.xlsx")

base_stades <- base_sport_equip %>%
  filter(EquNbPlaceTribune > 2000) %>%
  select(ComLib,InsNom,EquNom,EquipementTypeLib,EquipementFamille, EquGPSX, EquGPSY,EquNbPlaceTribune) %>%
  filter(EquipementFamille %in% "Terrain de grands jeux") %>%
  filter(!is.na(InsNom)) %>%
  distinct(InsNom, .keep_all = T) %>%
  select(nom_poi = InsNom, latitude_poi = EquGPSY, longitude_poi = EquGPSX)

rm(base_sport_equip)


# péages

library(ows4R)
library(httr)
library(purrr)
api_topo <- "https://wxs.ign.fr/topographie/geoportail/wfs"
topo_client <- WFSClient$new(api_topo,
                             serviceVersion = "2.0.0")
topo_client$getFeatureTypes(pretty = TRUE)

url <- parse_url(api_topo)

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  nature = "Port",
                  srsName = "EPSG:2154",
                  typename = "BDTOPO_V3:equipement_de_transport"

)

# avec filtre
url$query <- list(service = "wfs",
                  #version = "2.0.0", # optional
                  request = "GetFeature",
                  # typename = "BDTOPO_V3:equipement_de_transport",
                  # typename = "BDTOPO_V3:terrain_de_sport",
                  # typename = "BDTOPO_V3:plan_d_eau",
                  typename = "BDTOPO_V3:equipement_de_transport",
                  srsName = "EPSG:2154",
                  # filter = "<Filter><PropertyIsEqualTo><PropertyName>regions:nature</PropertyName><Literal>'Vlaams Gewest'</Literal></PropertyIsEqualTo></Filter>"
                  # cql_filter="nature='Stade'",
                  cql_filter = "nature_detaillee='Arènes';statut_du_toponyme='Collecté'"
                  # "statut_du_toponyme='Collecté'"
)

request <- build_url(url)
bdtopo_equip_transport <- read_sf(request)



# culture

library(janitor)
library(tidyr)
library(data.table)

base_culture <- fread("../data/base-des-lieux-et-des-equipements-culturels.csv") %>%
  clean_names() %>%
  separate(coordonnees_gps_lat_lon, c("latitude","longitude"), sep = ",") %>%
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
  filter(!is.na(latitude) & !is.na(longitude))

bases_sites_archeo <-
  base_culture %>%
  filter(precision_equipement %in%  "Site archéologique") %>%
  filter(!is.na(nom)) %>%
  distinct(nom, .keep_all = T) %>%
  select(nom_poi = nom, latitude_poi = latitude, longitude_poi = longitude)

bases_espaces_proteges <-
  base_culture %>%
  filter(type_equipement_ou_lieu %in%  "Espaces protégés") %>%
  filter(!is.na(precision_equipement) & !precision_equipement %in% "") %>%
  distinct(precision_equipement, .keep_all = T) %>%
  select(nom_poi = precision_equipement, latitude_poi = latitude, longitude_poi = longitude)


rm(base_culture)

# concaténation poi

poi <- base_stades %>% mutate(type_poi = "stades") %>%
  rbind.data.frame(bases_sites_archeo %>% mutate(type_poi = "archeo"))  %>%
  rbind.data.frame(bases_espaces_proteges %>% mutate(type_poi = "unesco"))

rm(base_stades, bases_sites_archeo,bases_espaces_proteges)

usethis::use_data(poi, overwrite = TRUE)
