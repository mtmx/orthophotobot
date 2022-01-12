## code to prepare `date_orthophotohisto` dataset goes here
library(pdftools)
library(stringr)
library(purrr)
library(magrittr)


pdf_orthophotohisto <- pdf_text("https://geoservices.ign.fr/sites/default/files/2021-10/DC_BDORTHOHisto_1-0.pdf")

# page du tableau
dates_orthophotohisto <-
pdf_orthophotohisto %>%
  str_split("\n")%>% .[[16]]

# lignes du tableau
dates_orthophotohisto <-
dates_orthophotohisto[-c(1:4,41:47)]

# suppression des espaces
dates_orthophotohisto <- dates_orthophotohisto[2:36] %>%
  str_squish() %>%
  strsplit(split = " ")

# conversion en tableau
f_structure_pdv <- function(nl, n_tab){
  if (n_tab ==1) {
    c1 = 1
    c2 = 2
    c3 = 3
  } else if (n_tab ==2) {
    c1 = 4
    c2 = 5
    c3 = 6
  } else if (n_tab ==3) {
    c1 = 7
    c2 = 8
    c3 = 9
  }

dates_orthophotohisto[[nl]][c1] %>%
  bind_cols(dates_orthophotohisto[[nl]][c2]) %>%
  bind_cols(dates_orthophotohisto[[nl]][c3]) %>%
  as.data.frame() %>%
  set_colnames(c("code_dep", "nom_dep", "annee_pdv"))
}

dates_orthophotohisto_tab <-
  map2(seq(1,length(dates_orthophotohisto),1),1,~f_structure_pdv(.x,.y)) %>% bind_rows() %>%
  rbind.data.frame(map2(seq(1,length(dates_orthophotohisto),1),2,~f_structure_pdv(.x,.y)) %>% bind_rows()) %>%
  rbind.data.frame(map2(seq(1,length(dates_orthophotohisto),1),3,~f_structure_pdv(.x,.y)) %>% bind_rows()) %>%
  mutate(code_dep = str_pad(code_dep,2,side = "left", pad = "0"))

rm(dates_orthophotohisto,pdf_orthophotohisto)


usethis::use_data(dates_orthophotohisto_tab, overwrite = TRUE)
