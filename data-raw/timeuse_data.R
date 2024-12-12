## code to prepare `timeuse_data` dataset goes here

library(tidyverse)

timeuse_data <- readRDS("./data-raw/timeuse_data.rds")

names(timeuse_data)

rn <- tibble::tribble(
  ~from, ~to,
  "isco_clerical_support_workers", "isco_clerical",
  "isco_craft_and_related_trades_workers", "isco_craft",
  "isco_elementary_occupations", "isco_elementary",
  "isco_plant_and_machine_operators_and_assemblers", "isco_plant",
  "isco_service_and_sales_workers", "isco_service",
  "isco_skilled_agricultural_forestry_and_fishery_workers", "isco_agri",
  "isco_technicians_and_associate_professionals", "isco_tech",
  "orders_online_frequently", "freq_onl_order",
  "vacation_during_study", "vacation"
)

rn_vec <- setNames(rn$from, rn$to)

timeuse_data <- rename(timeuse_data, all_of(rn_vec))

usethis::use_data(timeuse_data, overwrite = TRUE)
