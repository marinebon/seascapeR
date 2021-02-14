#' Create polygon from bounding box
#'
#' Utility function to create polygon from bounding box
#'
#' @param lon_min longitude, minimum
#' @param lat_min latitude, minimum
#' @param lon_max longitude, maximum
#' @param lat_max latitude, maximum
#'
#' @return Returns a spatial feature \link[sf]{sf} polygon.
#' @importFrom magrittr %>%
#' @importFrom sf st_as_sfc
#' @importFrom sf st_bbox
#' @importFrom raster extent
#' @export
#'
#' @examples
#' # Florida Keys area
#' lon = -81.3; lat = 24.5; w = 10
#' bbox_ply(lon - w, lat - w, lon + w, lat + w)
bbox_ply <- function(lon_min, lat_min, lon_max, lat_max){
  raster::extent(lon_min, lon_max, lat_min, lat_max) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc()
}

#' Get polygon from link or NOAA Sanctuary code
#'
#' Given a sanctuary code or link (ie URL), download the zip, unzip it, and read
#' first shapefile as a spatial feature. This function was originally designed
#' to pull from one of the zip links found at
#' \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#' GIS}.
#'
#' @param sanctuary NOAA Sanctuary code with which to form `url`. Sanctuary
#'   codes can be found at
#'   \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#'   GIS}.
#' @param url Link from which to fetch a polygon. Originally designed to pull
#'   from one of the zip links found at
#'   \href{https://sanctuaries.noaa.gov/library/imast_gis.html}{NOAA Sanctuaries
#'   GIS}.
#' @param dir_ply Directory to download locally into. This directory serves as
#'   a cache to skip operations if already performed. If zip file exists, skip
#'   downloading. If directory to unzip exists, skip unzipping.
#' @param verbose Verbose messages describing operations and choices, such as
#'   skipping download or unzip and which shapefile used if more than one found.
#'
#' @return Returns a spatial feature \link[sf]{sf} polygon data frame.
#' @import fs
#' @importFrom R.utils isUrl
#' @importFrom here here
#' @importFrom glue glue
#' @importFrom sf read_sf
#' @export
#' @concept read
#'
#' @examples
#' # mbnms: Monterey Bay National Marine Sanctuary
#' ply_mbnms <- get_url_ply(sanctuary = "mbnms")
#' ply_mbnms
#' plot(ply_mbnms[1])
#'
#' # fknms: Florida Keys National Marine Sanctuary
#' ply_fknms <- get_url_ply(sanctuary = "fknms")
#' ply_fknms
#' plot(ply_fknms[1])
get_url_ply <- function(
  sanctuary = NULL,
  url       = NULL,
  dir_ply   = here::here("data_ss/ply"),
  verbose   = F){
  # url = "https://sanctuaries.noaa.gov/library/imast/mbnms_py2.zip"
  # dir_ply = here::here("data_seascapeR")

  if (is.null(sanctuary) & is.null(url))
    stop("Please provide a url or sanctuary argument")

  if (!is.null(sanctuary))
    url = glue("https://sanctuaries.noaa.gov/library/imast/{sanctuary}_py2.zip")

  if(!R.utils::isUrl(url))
    stop(glue("The url '{url}' does not appear to be a URL."))

  dir_create(dir_ply)

  f <- path(dir_ply, basename(url))
  if (file.exists(f)){
    if (verbose)
      message(glue("
      Found file: {f}
        Skipping download: {url}"))
  } else {
    if (verbose)
      message(glue("
      Downloading
        from: {url}
        to:   {f}"))
    download.file(url, f)
  }

  if (path_ext(f) == "zip"){
    dir_unzip <- fs::path(dir_ply, fs::path_ext_remove(basename(f)))
    if (dir_exists(dir_unzip)){
      if (verbose)
        message(glue("
        Found dir: {dir_unzip}
          Skipping unzip of file: {f}"))
    } else {
      if (verbose)
        message(glue("
        Unzipping
          from: {f}
          to:   {dir_unzip}"))
      unzip(f, exdir = dir_unzip)
    }
    dir_shp <- dir_unzip
  } else {
    dir_shp <- dirname(f)
  }

  shps <- list.files(dir_shp, ".*\\.shp$", full.names = T)
  if (length(shps) == 0)
    stop(glue("No shapefiles (*.shp) found in {dir_shp}"))
  if (length(shps) > 1)
    message(glue("
    More than one shapefile (*.shp) found in {dir_shp}
      Reading only first: {basename(shps[1])}"))

  if (verbose)
    message(glue("
    Reading spatial features from
      shapefile: {shps[1]}"))
  sf::read_sf(shps[1]) %>%
    st_transform(crs = 4326)
}

#' Get Seascape grids from polygon and date range
#'
#' Given a polygon and date range, fetch Seascape data and return a raster layer
#' for a single date or raster stack if multiple dates found for given date range.
#'
#' @param ss_info SeaScape ERDDAP info object, as returned by \code{\link{get_ss_info}})
#' @param ss_var SeaScape variable. One of "CLASS" (default) or "P" for probability.
#' @param ply polygon as spatial feature \code{\link[sf]{sf}}, as returned by
#'   \code{\link{get_url_ply}} or \code{\link{bbox_to_ply}}
#' @param date_beg date begin to fetch, as character (`"2003-01-15"`) or Date
#'   (`Date("2003-01-15")`).
#' @param date_end date end to fetch, as character (`"2020-11-15"`) or Date
#'   (`Date("2020-11-15")`). Defaults to most recent date available from `ss_info`.
#'
#' @return Raster \code{\link[raster]{raster}} layer if one date,
#'   \code{\link[raster]{stack}} if more
#' @import lubridate purrr sf tidyr
#' @importFrom glue glue
#' @importFrom raster raster
#' @importFrom raster crs
#' @importFrom raster mask
#' @importFrom raster plot
#' @importFrom sp coordinates
#' @importFrom sp gridded
#' @export
#' @concept read
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#' grds
#'
get_ss_grds <- function(
  ss_info,
  ply,
  ss_var    = "CLASS",
  date_beg  = "2020-01-01",
  date_end  = max(get_ss_dates(ss_info)),
  dir_tif   = NULL){

  # ss_info
  # ply
  # ss_var    = "CLASS"
  # date_beg  = "2020-01-01"; date_end  = "2021-01-01"
  # dir_tif   = here("data_ss/mbnms_global_monthly_2020-01-01_to_2021-01-01")
  # write_tif = T

  if (!is.null(dir_tif)){
    dir_create(dir_tif)

    tifs <- list.files(path = dir_tif, pattern='grd_.*\\.tif$', full.names=T)

    if (length(tifs) == 1)
      grd <- raster::raster(tifs[1])
    if (length(tifs) > 1)
      grd <- raster::stack(tifs)
    if (length(tifs) > 0){
      names(grd) <- names(grd) %>% str_replace("^grd_", "")
      message("Reading existing grids ([dir_tif]/grd_[ss_var]_[date].tif) vs fetching fresh data via ERDDAP.")
      return(grd)
    }
  }

  # ply = mbnms_ply
  # dataset = "global_monthly"; ss_var = "CLASS" # define variable, ie product P: probability or CLASS: seascape class
  # date_beg = '2020-11-01'; date_end = '2020-12-01'
  select = dplyr::select

  s_dates <- get_ss_dates(ss_info)

  date_beg <- as.Date(date_beg)
  date_end <- as.Date(date_end)

  if (!int_overlaps(
    interval(  date_beg, date_end  ),
    interval(s_dates[1], s_dates[2]))){
    stop(glue("Date range requested ({date_beg} to {date_end}) does not overlap with Seascapes ({s_dates[1]} to {s_dates[2]})."))
  }

  if (date_end > s_dates[2]){
    warning(glue("The date_end {date_end} > Seascapes end ({s_dates[2]}) so decreasing to {s_dates[2]}."))
    date_end <- s_dates[2]
  }

  if (date_beg < s_dates[1]){
    warning(glue("The date_beg {date_beg} < Seascape begin ({s_dates[1]}) so increasing to {s_dates[1]}."))
    date_beg <- s_dates[1]
  }

  bb <- sf::st_bbox(ply)
  # TODO: check bb's crs==4326 and within range of dataset product
  # TODO: unionize ply

  nc <- try(griddap(
    x         = attr(ss_info, "datasetid"),
    fields    = ss_var,
    url       = ss_info$base_url,
    longitude = c(bb["xmin"], bb["xmax"]),
    latitude  = c(bb["ymin"], bb["ymax"]),
    time      = c(date_beg, date_end),
    fmt       = "nc"))
  if ("try-error" %in% nc)
    stop(glue("
    Problem fetching data from ERDDAP server using:
      rerddap::griddap(
        x         = '{attr(ss_info, 'datasetid')}',
        fields    = '{ss_var}',
        url       = '{ss_info$base_url}',
        longitude = c({bb['xmin']}, {bb['xmax']}),
        latitude  = c({bb['ymin']}, {bb['ymax']}),
        time      = c('{date_beg}', '{date_end}'))"))
  # TODO: save data (ply_mbnms; grd_mbnms, tbl_mbnms), document in data.R; mv this to read.R
  # TODO: dashboard like ecoidx_dashboard of

  tbl <- tibble(nc$data) %>%
    mutate(
      # round b/c of uneven intervals
      #   unique(tbl$lon) %>% sort() %>% diff() %>% unique() %>% as.character()
      #     0.0499954223632812 0.0500030517578125
      #   TODO: inform Maria/Joaquin about uneven intervals
      lon  = round(lon, 3),
      lat  = round(lat, 3),
      date = as.Date(time, "%Y-%m-%dT12:00:00Z")) %>%
    select(-time) %>%
    group_by(date) %>%
    nest() %>%
    mutate(
      raster = map(data, function(x){
        sp::coordinates(x) <- ~ lon + lat
        sp::gridded(x) <- T
        grd <- raster::raster(x)
        raster::crs(grd) <- 4326
        grd }))

  if (nrow(tbl) == 1){
    grd <- tbl$raster[[1]]
    names(grd) <- glue("{ss_var}_{tbl$date[[1]]}")

    if (!is.null(dir_tif))
      raster::writeRaster(
        grd, glue("{dir_tif}/grd_{names(grd)}.tif"), overwrite = T)

  } else {
    grd <- raster::stack(tbl$raster)
    names(grd) <- glue("{ss_var}_{tbl$date}")

    if (!is.null(dir_tif))
      raster::writeRaster(
        grd, paste0(dir_tif,"/grd"), names(grd),
        bylayer=T, format='GTiff', overwrite = T)
  }

  # raster::plot(grd); plot(ply, add = T, col = scales::alpha("blue", 0.3))
  raster::mask(grd, ply)
}

#' Get Seascape dataset information
#'
#' Get Seascape dataset information from ERDDAP server.
#'
#' @param dataset `{region}_{frequency}` of dataset. Valid values (so far): "global_8day" or "global_monthly" (default).
#'
#' @return ERDDAP \code{\link[rerddap]{info}} object
#' @import rerddap
#' @export
#' @concept read
#'
#' @examples
#' get_ss_info() # default: dataset = "global_monthly"
#' get_ss_info("global_8day")
get_ss_info <- function(dataset = "global_monthly"){

  dataset_to_id <- list(
    global_8day    = "noaa_aoml_seascapes_8day",
    global_monthly = "noaa_aoml_4729_9ee6_ab54")

  info(dataset_to_id[[dataset]], url = "https://cwcgom.aoml.noaa.gov/erddap/")
}

#' Get date range of Seascape dataset
#'
#' @param ss_info ERDDAP info object, as returned by \code{\link{get_ss_info}})
#'
#' @return date range of min and max
#' @import dplyr stringr
#' @export
#' @concept read
#'
#' @examples
#' ss_info <- get_ss_info()
#' get_ss_dates(ss_info)
get_ss_dates <- function(ss_info){

  ss_info$alldata$time %>%
    filter(attribute_name=="actual_range") %>%
    pull(value) %>%
    str_split(", ", simplify = T) %>%
    as.numeric() %>%
    as.POSIXct(origin = "1970-01-01", tz = "GMT") %>%
    as.Date()
}

