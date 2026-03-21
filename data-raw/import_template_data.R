## code to import `data_perm` 
library(readr)
data_perm <- readr::read_csv("data-raw/permission_template.csv")
usethis::use_data(data_perm, overwrite = TRUE)


## code to import `template_data_geographics`

template_data_geographics <- readr::read_csv("data-raw/template_geographic.csv")
usethis::use_data(template_data_geographics, overwrite = TRUE)
