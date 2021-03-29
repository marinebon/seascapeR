librarian::shelf(
  dplyr,
  here,
  readr,
  usethis)

ss_vars <- here("data-raw/ss_vars.csv") %>%
  read_csv()
use_data(ss_vars, overwrite = T)
