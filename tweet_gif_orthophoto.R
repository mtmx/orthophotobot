

library(sf)
library(stringr)
library(dplyr)
library(rtweet)
library(usethis)

orthophotobot_token <- rtweet::create_token(
  app = "orthophotobot",
  consumer_key =    Sys.getenv("TWITTER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_API_KEY_SECRET"),
  access_token =    Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)


load("./data/poi.rda")
load("./data/geo_communes.rda")
load("./data/dates_orthophotohisto_tab.rda")

# extraction d'un poi au hasard

rdm_point <-
  poi %>%
  st_as_sf(coords = c( "longitude_poi" , "latitude_poi"),
           crs = 4326, remove = FALSE) %>%
  sample_n(., 1)


# nom du point
rdm_point_nom <- rdm_point %>% st_drop_geometry() %>% select(nom_poi) %>% pull() %>% as.vector()%>% str_trim()

# commune du point
rdm_point_comm <- rdm_point %>% st_join(geo_communes) %>% st_drop_geometry() %>% select(NOM_COM) %>% pull() %>% as.vector() %>% str_trim()

# département du point
rdm_point_dep <- rdm_point %>% st_join(geo_communes) %>% st_drop_geometry() %>% select(NOM_DEP) %>% pull() %>% as.vector() %>% str_trim()

# année de prise de vue orthophotohisto
rdm_point_annee_pdv_orthophotohisto <- rdm_point %>% st_join(geo_communes) %>% st_drop_geometry() %>% select(INSEE_DEP) %>%
  left_join(dates_orthophotohisto_tab, by = c("INSEE_DEP" = "code_dep")) %>%
  select(annee_pdv) %>%
  pull() %>% as.vector() %>% str_trim()

# coordonnées
lon_poi <- rdm_point %>% pull(longitude_poi)
lat_poi <- rdm_point %>% pull(latitude_poi)

# suppression du poi dans le fichier source

poi <- poi %>%
  filter(!latitude_poi == lat_poi & !longitude_poi == lon_poi)

usethis::use_data(poi, overwrite = TRUE)


# url geoportail
rdm_point_url <- paste0("https://www.geoportail.gouv.fr/carte?c=",lon_poi,",",lat_poi,"&z=16&l0=ORTHOIMAGERY.ORTHOPHOTOS::GEOPORTAIL:OGC:WMTS(1)&l1=ORTHOIMAGERY.ORTHOPHOTOS.1950-1965::GEOPORTAIL:OGC:WMTS(1)&permalink=yes")

# bounding box
bbox_xy_wgs1984 <- rdm_point %>%
  st_transform(2154) %>%
  st_buffer(dist = 250) %>%
  st_bbox() %>% st_as_sfc() %>% st_sf() %>% st_transform(4326) %>% st_bbox()


# coordonnées des coins
xmin_bbox <- bbox_xy_wgs1984$xmin %>% as.vector()
xmax_bbox <- bbox_xy_wgs1984$xmax %>% as.vector()
ymin_bbox <- bbox_xy_wgs1984$ymin %>% as.vector()
ymax_bbox <- bbox_xy_wgs1984$ymax %>% as.vector()

usethis::ui_done("poi")

# téléchargement des images

url_img_ortho <- paste0("https://wxs.ign.fr/ortho/geoportail/r/wms?LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO&EXCEPTIONS=text/xml&FORMAT=image/jpeg&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&STYLES=&CRS=EPSG:4326&BBOX=",ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox,"&WIDTH=1256&HEIGHT=1256")

download.file(url = url_img_ortho,
              destfile = paste0("./data/jpg/img_ortho_actu_poi_",lat_poi,"_",lon_poi, ".jpg"),
              mode = 'wb')

# orthohisto

url_img_ortho_histo <- paste0("https://wxs.ign.fr/orthohisto/geoportail/r/wms?LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.1950-1965&EXCEPTIONS=text/xml&FORMAT=image/jpeg&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&STYLES=&CRS=EPSG:4326&BBOX=",ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox,"&WIDTH=3000&HEIGHT=3000")

download.file(url = url_img_ortho_histo,
              destfile = paste0("./data/jpg/img_ortho_histo_poi_",lat_poi,"_",lon_poi, ".jpg"),
              mode = 'wb')

usethis::ui_done("download images")

# création du gif

library(magick)

ortho_actu <- image_read( paste0("./data/jpg/img_ortho_actu_poi_",lat_poi,"_",lon_poi, ".jpg"))
ortho_histo <- image_read( paste0("./data/jpg/img_ortho_histo_poi_",lat_poi,"_",lon_poi, ".jpg"))

logo_ign <- image_read('./data/img/logo_ign.png') %>%
  image_scale(., "25")


orthophoto_gif <-  image_resize(c(ortho_histo, ortho_histo, ortho_actu,ortho_actu, ortho_histo), '500x500!') %>%
  image_morph(8) %>%
  image_animate(fps=5, optimize = T) %>%
  image_composite(., logo_ign, offset = "+474+485")

image_write(orthophoto_gif,
            paste0("./data/gif/orthophoto_poi_",lat_poi,"_",lon_poi, ".gif"))


usethis::ui_done("creation gif")

# post du tweet
post_tweet(status = paste0(rdm_point_nom, "\n",
                           rdm_point_comm, " (", rdm_point_dep,")\n",
                           emojis %>% filter(description %in% "camera") %>% pull(code), " ", rdm_point_annee_pdv_orthophotohisto, " / 2020", "\n",
                           emojis %>% filter(description %in% "world map") %>% pull(code), " ", rdm_point_url),
           media = paste0("./data/gif/orthophoto_poi_",lat_poi,"_",lon_poi, ".gif"),
           token = orthophotobot_token)

usethis::ui_done("post tweet")
