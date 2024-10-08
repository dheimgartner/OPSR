## code to prepare `telework_data` dataset goes here

telework_data <- read.delim("./data-raw/telework_data.csv", sep = ",")
telework_data <- telework_data[names(telework_data) != "X"]

usethis::use_data(telework_data, overwrite = TRUE)
