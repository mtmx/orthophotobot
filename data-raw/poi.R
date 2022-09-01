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
  filter(archeologie_detail %in% c("Oppidum", "Grotte", "Grotte Ornée", "Ville", "Thermes", "Allée Couverte", "Amphithéâtre", "Sanctuaire", "Tumulus", "Ensemble Mégalithique", "Forteresse", "Mausolée", "Fortification", "Église", "Habitat Fortifié", "Dolmen", "Temple", "Alignements", "Aqueduc", "Carrières")) %>%
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


# ajout de sources péages

library(ows4R)
library(httr)
library(sf)

api_topo <- "https://wxs.ign.fr/topographie/geoportail/wfs"
topo_client <- WFSClient$new(api_topo,
                             serviceVersion = "2.0.0")
topo_client$getFeatureTypes(pretty = TRUE)

url <- parse_url(api_topo)

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:equipement_de_transport",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Péage'"
)

request <- build_url(url)

bdtopo_peage <- read_sf(request) %>% filter(importance <= 5 & !is.na(toponyme) ) %>%
  mutate(date_creation = as.Date(date_creation)) %>%
  filter(date_creation <= as.Date("2020-01-01", format = "%Y-%m-%d"))  %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "péages")

library(purrr)
bdtopo_peage <- bdtopo_peage %>%
  st_transform(4326) %>%
mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
       latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()


# phares

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:construction_ponctuelle",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Phare'"
)

request <- build_url(url)

bdtopo_phare <- read_sf(request) %>% filter(importance <= 3 & !is.na(toponyme) )  %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "phares")  %>%
  st_transform(4326) %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()

# chateaux

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:zone_d_habitation",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Château'"
)

request <- build_url(url)

bdtopo_chateau <- read_sf(request) %>% filter(importance <= 4 & !is.na(toponyme) )  %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "chateaux")  %>%
  st_centroid() %>%
  st_transform(4326) %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()


# lotissements

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:zone_d_habitation",
                  srsName = "EPSG:2154",
                  cql_filter="nature_detaillee='Lotissement'"
)

request <- build_url(url)

bdtopo_lotissements <- read_sf(request) %>% filter(importance <= 6 & !is.na(toponyme) )  %>%
  sample_n(300) %>%
  select(nom_poi = toponyme ) %>%

  mutate(type_poi = "lotissements")  %>%
  st_centroid() %>%
  st_transform(4326) %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()

# plages

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:detail_orographique",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Plage'"
)

request <- build_url(url)

bdtopo_plage <- read_sf(request) %>% filter(importance <= 4 & !is.na(toponyme) )  %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "plages")  %>%
  st_transform(4326) %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()

# gares

bdtopo_gare <- st_read("https://ressources.data.sncf.com/explore/dataset/liste-des-gares/download?format=geojson&timezone=Europe/Berlin&use_labels_for_header=false") %>%
  slice(1:500) %>%
  filter(!is.na(libelle) ) %>%
  select(nom_poi = libelle ) %>% mutate(nom_poi = paste0("Gare - ", nom_poi)) %>%
  mutate(type_poi = "gares") %>%
  mutate(longitude_poi = map_dbl(geometry, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()


# aéroports

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:aerodrome",
                  srsName = "EPSG:2154"
)

request <- build_url(url)

bdtopo_aero <- read_sf(request) %>% filter(!is.na(toponyme) ) %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "aéroports") %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()

# camping

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:zone_d_activite_ou_d_interet",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Camping'"
)

request <- build_url(url)

bdtopo_campings <- read_sf(request) %>%
  filter(importance <= 4 & !is.na(toponyme) )  %>%
  select(nom_poi = toponyme ) %>%
  mutate(nom_poi = paste0("Camping - ",nom_poi)) %>%
  mutate(type_poi = "campings") %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()


# carrière

url$query <- list(service = "wfs",
                  request = "GetFeature",
                  typename = "BDTOPO_V3:zone_d_activite_ou_d_interet",
                  srsName = "EPSG:2154",
                  cql_filter="nature='Carrière'"
)

request <- build_url(url)

bdtopo_carrieres <- read_sf(request) %>%
  filter(importance <= 4 & !is.na(toponyme) ) %>%
  select(nom_poi = toponyme ) %>%
  mutate(type_poi = "carrières") %>%
  mutate(longitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[1]]),
         latitude_poi = map_dbl(geometrie, ~st_centroid(.x)[[2]])) %>%
  st_drop_geometry()

# concaténation poi

poi <- poi %>%
  rbind.data.frame(bdtopo_campings)  %>%
  rbind.data.frame(bdtopo_aero ) %>%
  rbind.data.frame(bdtopo_carrieres)

rm(bdtopo_campings)
rm(bdtopo_aero)
rm(bdtopo_carrieres)

# suppression des poi hors contours communaux
poi <- poi %>%
  st_as_sf(coords = c("longitude_poi", "latitude_poi"),
           crs = 4326, remove = FALSE) %>%
  st_join(geo_communes, join = st_within) %>%
  filter(!is.na(INSEE_COM)) %>%
  st_drop_geometry() %>%
  select(c(nom_poi, latitude_poi, longitude_poi, type_poi))


usethis::use_data(poi, overwrite = TRUE)
