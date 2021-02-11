#' Get Seascape dataset information from ERDDAP server
#'
#' @param dataset `\{region\}_\{frequency\}` of dataset. Valid values (so far): "global_8day" or "global_monthly" (default).
#'
#' @return ERDDAP \code{\link[rerddap]{info}} object
#' @import rerddap
#' @export
#'
#' @examples
#' get_seascape_info() # default: dataset = "global_monthly"
#' get_seascape_info("global_8day")
get_seascape_info <- function(dataset = "global_monthly"){

  dataset_to_id <- list(
    global_8day    = "noaa_aoml_seascapes_8day",
    global_monthly = "noaa_aoml_4729_9ee6_ab54")

  info(dataset_to_id[[dataset]], url = "https://cwcgom.aoml.noaa.gov/erddap/")
}

#' Get date range of Seascape dataset
#'
#' @param info ERDDAP info object, as returned by \code{\link{get_seascape_info}})
#'
#' @return date range of min and max
#' @import dplyr stringr
#' @export
#'
#' @examples
#' s_info <- get_seascape_info()
#' get_date_rng(s_info)
get_date_rng <- function(info){

  info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT") %>%
    as.Date()
}

#' Map Seascape using Web Map Server (WMS) image tiles
#'
#' Returns interactive map with Seascapes as image tiles (not data).
#'
#' So far this uses the \href{https://tile.gbif.org/ui/}{GBIF background tiles}
#' since the ERDDAP WMS only displays in geographic project (EPSG = 4326).
#'
#' @param date date to map, eg \code{Date("2020-11-15")}
#' @param ctr_lon center longitude, in decimal degrees. Defaults to -81.3.
#' @param ctr_lat center longitude, in decimal degrees. Defaults to 24.5.
#' @param ctr_dd width around center, in decimal degrees. Defaults to 10.
#' @param basemap_style any style available from \href{https://tile.gbif.org/ui/}{GBIF background tiles}. Defaults to "gbif-geyser".
#' @param var variable. One of "CLASS" (default) or "P" for probability.
#'
#' @return Leaflet \code{\link[leaflet]{leaflet}} interactive map widget
#' @import leaflet leafem
#' @export
#'
#' @examples
#' s_info <- get_seascape_info()
#'
#' # map most recent Seascape
#' s_date_rng <- get_date_rng(s_info)
#' map_seascape_wms(s_date_rng[2], ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10)
map_seascape_wms <- function(date, ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, var = "CLASS", basemap_style = "gbif-geyser"){
  # TODO: get URL and dataset_id from s_info as argument
  # TODO: optional marker

  # date = s_date_rng[2]; ctr_lon = -81.3; ctr_lat = 24.5

  # render a map with the latest seascape
  leaflet(
    options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
    # basemap from GBIF in 4326
    addTiles(glue("//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style={basemap_style}")) %>%
    addWMSTiles(
      baseUrl = glue("https://cwcgom.aoml.noaa.gov/erddap/wms/noaa_aoml_4729_9ee6_ab54/request?"),
      layers = glue("noaa_aoml_4729_9ee6_ab54:{var}"),
      options = WMSTileOptions(
        version = "1.3.0", format = "image/png", transparent = T, opacity = 0.7,
        time = format(date,"%Y-%m-%dT00:00:00Z")))  %>%
    addMarkers(lng = ctr_lon, lat = ctr_lat)  %>%
    addMouseCoordinates() %>%
    fitBounds(ctr_lon - ctr_dd, ctr_lat - ctr_dd, ctr_lon + ctr_dd, ctr_lat + ctr_dd) %>%
    addLegend(
      position="bottomright",
      title = paste0("CLASS<br>", format(date,"%Y-%m-%d")),
      colorNumeric("Spectral", c(1,33), reverse=T), seq(1,33))
}

#' Get Seascape data
#'
#' @param ctr_lon center longitude, in decimal degrees. Defaults to -81.3.
#' @param ctr_lat center longitude, in decimal degrees. Defaults to 24.5.
#' @param ctr_dd width around center, in decimal degrees. Defaults to 10.
#' @param dataset `\{region\}_\{frequency\}` of dataset. Valid values (so far): "global_8day" or "global_monthly" (default).
#' @param var variable. One of "CLASS" (default) or "P" for probability.
#' @param date_beg date begin to fetch, as character (`"2003-01-15"`) or Date (`Date("2003-01-15")`).
#' @param date_end date end to fetch, as character (`"2020-11-15"`) or Date (`Date("2020-11-15")`).
#'
#' @return Raster \code{\link[raster]{raster}} layer if one date, \code{\link[raster]{stack}} if more
#' @import glue lubridate purrr raster tidyr
#' @export
#'
#' @examples
#' r_monthly <- get_seascape_data(ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, dataset = "global_monthly", var = "CLASS", date_beg = "2020-11-01", date_end = "2020-12-01")
#' r_monthly
#' r_8day <- get_seascape_data(ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, dataset = "global_8day", var = "CLASS", date_beg = "2020-11-01", date_end = "2020-12-01")
#' r_8day
get_seascape_data <- function(ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, dataset = "global_monthly", var = "CLASS", date_beg = "2020-11-01", date_end = "2020-12-01"){
  # ctr_lon = -81.3; ctr_lat = 24.5; ctr_dd = 10
  # date_beg = '2020-11-01'; date_end = '2020-12-01' # as character (YYYY-MM-DD) or Date
  # var = "CLASS" # define variable, ie product P: probability or CLASS: seascape class

  select = dplyr::select

  s_info  <- get_seascape_info(dataset)
  s_dates <- get_date_rng(s_info)

  date_beg <- as.Date(date_beg)
  date_end <- as.Date(date_end)

  if (!int_overlaps(
    interval(  date_beg, date_end  ),
    interval(s_dates[1], s_dates[2]))){
    stop(glue("Date range requested ({date_beg} to {date_end}) does not overlap with Seascapes ({s_dates[1]} to {s_dates[2]})."))
  }

  if (date_end > s_date_rng[2]){
    warning(glue("The date_end {date_end} > Seascapes end ({s_dates[2]}) so decreasing to {s_dates[2]}."))
    date_end <- s_date_rng[2]
  }

  if (date_beg < s_date_rng[1]){
    warning(glue("The date_beg {date_beg} < Seascape begin ({s_dates[1]}) so increasing to {s_dates[1]}."))
    date_beg <- s_date_rng[1]
  }

  v <- try(griddap(
    s_info,
    longitude = c(ctr_lon - ctr_dd, ctr_lon + ctr_dd),
    latitude  = c(ctr_lat - ctr_dd, ctr_lat + ctr_dd),
    time = c(date_beg, date_end), fields = var))

  tbl <- tibble(v$data) %>%
    # TODO: inform Maria/Joaquin about uneven intervals
    # unique(tbl$lon) %>% sort() %>% diff() %>% unique() %>% as.character()
    #   0.0499954223632812 0.0500030517578125
    #                  160                240
    mutate(
      lon  = round(lon, 3),
      lat  = round(lat, 3),
      date = as.Date(time, "%Y-%m-%dT12:00:00Z")) %>%
    select(-time) %>%
    group_by(date) %>%
    nest() %>%
    mutate(
      raster = map(data, function(x){
        coordinates(x) <- ~ lon + lat
        gridded(x) <- T
        r <- raster(x)
        crs(r) <- 4326
        r }))

  if (nrow(tbl) == 1){
    r <- tbl$raster[[1]]
    return(r)
  } else {
    stk <- raster::stack(tbl$raster)
    names(stk) <- glue("date_{tbl$date}")
    stk
  }
}

# create map with downloaded monthly data (for now only works with a single time period)
#' Map Seascape raster
#'
#' @param r raster layer
#' @param var variable. One of "CLASS" (default) or "P" for probability.
#' @param palette color palette. Default: "Spectral".
#' @param basemap basemap for leaflet. Default: `leaflet::providers$Esri.OceanBasemap`.
#' @param opacity transparency. Default: 0.8.
#'
#' @return Leaflet \code{\link[leaflet]{leaflet}} interactive map widget
#' @import leaflet
#' @export
#'
#' @examples
#' r_monthly <- get_seascape_data(ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, dataset = "global_monthly", var = "CLASS", date_beg = "2020-11-01", date_end = "2020-12-01")
#' map_seascape_raster(r_monthly)
#'
#' r_8day <- get_seascape_data(ctr_lon = -81.3, ctr_lat = 24.5, ctr_dd = 10, dataset = "global_8day", var = "CLASS", date_beg = "2020-11-01", date_end = "2020-12-01")
#' r_8day
#' r_monthly
#' map_seascape_raster(raster(r_8day, 1))
#' map_seascape_raster(raster(r_8day, 2))
map_seascape_raster <- function(r, var = "CLASS", palette = "Spectral", basemap = providers$Esri.OceanBasemap, opacity = 0.8){

  pal <- colorNumeric(
    palette, values(r), reverse=T, na.color = "transparent") # other palette: c("#0C2C84", "#41B6C4", "#FFFFCC")

  leaflet() %>%
    addProviderTiles(basemap) %>%
    addRasterImage(r, colors = pal, opacity = opacity) %>%
    addLegend(
      pal = pal, values = values(r), title = var)
}
