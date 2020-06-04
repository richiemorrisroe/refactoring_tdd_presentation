require(devtools)
if (!dir.exists("ppr")) {
  usethis::create_package("ppr")
}

usethis::use_package("dplyr")
usethis::use_package("rlang")
usethis::use_package("readxl")
usethis::use_data_raw()
