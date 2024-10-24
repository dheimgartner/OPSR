## code to prepare `telework_data` dataset goes here

telework_data <- read.delim("./data-raw/telework_data.csv", sep = ",")
telework_data <-
  telework_data %>%
  dplyr::select(ExternalID, weight, everything()) %>%
  dplyr::rename_with(function(x) tolower(x)) %>%
  dplyr::rename(id = externalid)

skimr::skim(telework_data)  # all are numeric => no missings

usethis::use_data(telework_data, overwrite = TRUE)
