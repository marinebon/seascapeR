## code to prepare nms: National Marine Sanctuary codes and names

nms <- list(
  nmsas   = "American Samoa",
  cbnms   = "Cordell Bank",
  cinms   = "Channel Islands",
  fknms   = "Florida Keys",
  gfnms   = "Greater Farallones",
  grnms   = "Gray's Reef",
  hihwnms = "Hawaiian Islands Humpback Whale",
  mbnms   = "Monterey Bay",
  mbpr    = "Mallows Bay-Potomac River",
  mnms    = "Monitor Boundary",
  ocnms   = "Olympic Coast",
  pmnm    = "Papahānaumokuākea",
  sbnms   = "Stellwagen Bank",
  tbnms   = "Thunder Bay")

attr(nms, "source") <- "https://sanctuaries.noaa.gov/library/imast_gis.html"

usethis::use_data(nms, overwrite = T)

