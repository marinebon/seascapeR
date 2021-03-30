librarian::shelf(
  dplyr,
  here,
  readr,
  usethis)

# [ss_vars - Google Sheet](https://docs.google.com/spreadsheets/d/1vkIM355LMHRGgWjg6l8I9a0tJ7n0N9Pku4wt2q0vRec/edit#gid=1024001418)
ss_vars_csv <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR8ZH3gIShBfrsYynDmyI1TxTIGMpq8RnamDvVv3ODJe6OEE4PZBjGlYAzDBx63Kb4IEee4iFq4-kYC/pub?gid=1024001418&single=true&output=csv"
ss_vars <- read_csv(ss_vars_csv)
use_data(ss_vars, overwrite = T)
