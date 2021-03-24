librarian::shelf(
  dplyr,
  here,
  readr,
  usethis)

ss_gl_classes <- here("data-raw/global_classes.txt") %>%
  read_delim(delim = "|", na = c("NaN")) %>%
  rename(
    CLASS      = `SEASCAPE ID NUMBER`,
    NAME       =  `NOMINAL DESCRIPTOR`)
    # sst_c      = `SST  (°C)`,
    # sss_psu    = `SSS (psu)`,
    # adt_m      = `ADT (m)`,
    # ice_pct    = `ICE (%)`,
    # cdom_perm  = `CDOM (m^-1)`,
    # chla_mgm3  = `CHLA (mg m^-3)`,
    # nflh       = `NFLH (W m^-2 um^-1 sr^-1)`,
    # nflh_chl   = `NFLH:CHL`,
    # lat        = `LATITUDE`,
    # dom_hemi   = `DOMINANT HEMISPHERE`,
    # dom_season = `DOMINANT SEASON) %>%
use_data(ss_gl_classes, overwrite = T)
