## code to prepare `timeuse_data` dataset goes here

timeuse_data <- readRDS("./data-raw/timeuse_data.rds")

usethis::use_data(timeuse_data, overwrite = TRUE)
