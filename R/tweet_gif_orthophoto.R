
library(rgdal)
library(sf)
library(raster)
library(tidyverse)
library(hrbrthemes)
library(stringi)



library(sf)
library(stringr)


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

# bbox
bbox_xy_wgs1984 <- rdm_point %>%
  st_transform(2154) %>%
  st_buffer(dist = 300) %>%
  st_bbox() %>% st_as_sfc() %>% st_sf() %>% st_transform(4326) %>% st_bbox()


# coordonnées des angles
xmin_bbox <- bbox_xy_wgs1984$xmin %>% as.vector()
xmax_bbox <- bbox_xy_wgs1984$xmax %>% as.vector()
ymin_bbox <- bbox_xy_wgs1984$ymin %>% as.vector()
ymax_bbox <- bbox_xy_wgs1984$ymax %>% as.vector()

# ortho
url_img_ortho <- paste0("https://wxs.ign.fr/ortho/geoportail/r/wms?LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.BDORTHO&EXCEPTIONS=text/xml&FORMAT=image/jpeg&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&STYLES=&CRS=EPSG:4326&BBOX=",ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox,"&WIDTH=1256&HEIGHT=1256")

# orthohisto

url_img_ortho_histo <- paste0("https://wxs.ign.fr/orthohisto/geoportail/r/wms?LAYERS=ORTHOIMAGERY.ORTHOPHOTOS.1950-1965&EXCEPTIONS=text/xml&FORMAT=image/jpeg&SERVICE=WMS&VERSION=1.3.0&REQUEST=GetMap&STYLES=&CRS=EPSG:4326&BBOX=",ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox,"&WIDTH=3000&HEIGHT=3000")

download.file(url = url_img_ortho_histo,
              # destfile = paste0('./data/jpg/img_ortho_histo_',ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox, '.jpg'),
              destfile = './data/jpg/img_ortho_histo_poi.jpg',
              mode = 'wb')

download.file(url = url_img_ortho,
              # destfile = paste0('./data/jpg/img_ortho_actu_',ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox, '.jpg'),
              destfile = './data/jpg/img_ortho_actu_poi.jpg',
              mode = 'wb')



library(magick)
# ortho_actu <- image_scale(image_read( paste0('./jpg/img_ortho_actu_',ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox, '.jpg')))
# ortho_histo <- image_scale(image_read( paste0('./jpg/img_ortho_histo_',ymin_bbox,",",xmin_bbox,",",ymax_bbox,",",xmax_bbox, '.jpg')))

ortho_actu <- image_scale(image_read( './data/jpg/img_ortho_actu_poi.jpg'))
ortho_histo <- image_scale(image_read('./data/jpg/img_ortho_histo_poi.jpg'))



my_gif <-  image_resize(c(ortho_histo, ortho_histo, ortho_actu,ortho_actu, ortho_histo), '600x600!') %>%
  image_morph(15) %>%
  image_animate(fps=5, optimize = TRUE) %>%
  image_annotate(., "© IGN", size = 13, color = "white",
                 # boxcolor = "pink",
                 degrees = 0,
                 location = "+552+580")


image_write(my_gif, paste("./data/gif/ortho_poi.gif"))

