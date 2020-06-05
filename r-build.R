require(devtools)
if (!dir.exists("ppr")) {
  usethis::create_package("ppr")
}

usethis::use_package("dplyr")
usethis::use_package("rlang")
usethis::use_package("readxl")
usethis::use_package("caret")
usethis::use_package("rgdal")
usethis::use_package("sp")
usethis::use_package("glmnet")
usethis::use_package("sf")
usethis::use_data_raw()
