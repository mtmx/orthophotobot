library(usethis)
use_mit_license("mtmx")

usethis::use_readme_rmd( open = TRUE )

usethis::use_data_raw( name = "carto_admin", open = TRUE )
usethis::use_data_raw( name = "poi", open = TRUE )
usethis::use_data_raw( name = "date_orthophotohisto", open = TRUE )

### ADD R FUNCTIONS AND THEIR DOCUMENTATION
use_r("tweet_gif_orthophoto")

# dependences
usethis::use_package("dplyr")
usethis::use_package("sf")
usethis::use_package("stringr")
usethis::use_package("magick")
usethis::use_package("rtweet")


devtools::check()
