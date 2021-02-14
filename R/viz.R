#' Map Seascape image tiles from web map server (wms)
#'
#' Returns interactive map with Seascapes as image tiles (not data) from web map server (wms).
#'
#' So far this uses the \href{https://tile.gbif.org/ui/}{GBIF background tiles}
#' since the ERDDAP WMS only displays in geographic project (EPSG = 4326).
#'
#' @param ss_info SeaScape ERDDAP info object, as returned by \code{\link{get_ss_info}})
#' @param ss_var SeaScape variable. One of `"CLASS"` (default) or `"P"` for probability.
#' @param date date of seascape variable to map. Defaults to most recent date available from `ss_info`, eg \code{Date("2020-11-15")}.
#' @param ply optional polygon as spatial feature \code{\link[sf]{sf}}, as returned by
#'   \code{\link{get_url_ply}} or \code{\link{bbox_to_ply}}, to use for initial extent of map and optionally plot (see `ply_color`).
#' @param ply_color   polygon color. To not plot polygon and just use for initial extent of map set `ply_color = NA`. Default: `"blue"`.
#' @param ply_opacity transparency of polygon Default: `0.2`.
#' @param basemap_style any style available from \href{https://tile.gbif.org/ui/}{GBIF background tiles}. Defaults to `"gbif-geyser"`.
#'
#' @return Leaflet \code{\link[leaflet]{leaflet}} interactive map widget
#' @import dplyr leaflet leafem
#' @importFrom glue glue
#' @importFrom leafem addMouseCoordinates
#' @importFrom sf st_bbox
#' @export
#' @concept viz
#'
#' @examples
#' ss_info <- get_ss_info()
#'
#' # map most recent Seascape
#' map_ss_wms(ss_info)
map_ss_wms <- function(
  ss_info,
  ply           = NULL,
  date          = max(get_ss_dates(ss_info)),
  ss_var        = "CLASS",
  ply_color     = "blue",
  ply_opacity   = 0.2,
  basemap_style = "gbif-geyser"){
  # TODO: get URL and dataset_id from ss_info as argument
  # TODO: optional marker

  # date = max(get_ss_dates(ss_info))
  # ss_var="CLASS"; ply_color="blue"; ply_opacity=0.2; basemap_style="gbif-geyser"

  # TODO: check bb's crs==4326 and within range of dataset product
  # TODO: unionize ply

  ss_dataset = attr(ss_info, "datasetid")
  ss_url     = ss_info$base_url

  # render a map with the latest seascape
  m <- leaflet(
    options = leafletOptions(
      crs = leafletCRS(crsClass = "L.CRS.EPSG4326"))) %>%
    # basemap from GBIF in 4326
    addTiles(glue(
      "//tile.gbif.org/4326/omt/{z}/{x}/{y}@1x.png?style=<basemap_style>",
      .open = "<", .close = ">")) %>%
    addWMSTiles(
      baseUrl = glue("{ss_url}wms/{ss_dataset}/request?"),
      layers  = glue("{ss_dataset}:{ss_var}"),
      options = WMSTileOptions(
        version = "1.3.0", format  = "image/png",
        transparent = T, opacity = 0.7,
        time = strftime(date,"%Y-%m-%dT00:00:00Z")))  %>%
    leafem::addMouseCoordinates() %>%
    addLegend(
      position ="bottomright",
      title    = glue("{ss_var}<br>{strftime(date,'%Y-%m-%d')}"),
      # TODO: attr(ss_info, n_classes = 33) per ss_var
      pal = colorNumeric("Spectral", c(1,33), reverse=T), seq(1,33))

  # TODO: addLayersControl()

  if (!is.null(ply)){
    bb <- sf::st_bbox(ply) %>% as.double()

    m <- m %>%
      fitBounds(bb[1], bb[2], bb[3], bb[4])

    if (!is.na(ply_color)){
      m <- m %>%
        addPolygons(
          data        = ply,
          color       = ply_color,
          fillColor   = ply_color,
          fillOpacity = ply_opacity)
    }
  }

  m
}

#' Map Seascape grid
#'
#' Map Seascape grid with optional polygon as interactive map.
#'
#' @param grd         grid
#' @param ply         polygon
#' @param grd_palette color palette. Default: "Spectral".
#' @param ply_color   polygon color
#' @param grd_opacity transparency of grid. Default: 0.8.
#' @param ply_opacity transparency of polygon fill color. Default: 0.
#' @param basemap basemap for leaflet. Default: `leaflet::providers$Esri.OceanBasemap`.
#'
#' @return Leaflet \code{\link[leaflet]{leaflet}} interactive map widget
#' @import leaflet
#' @importFrom raster values
#' @importFrom raster raster
#' @export
#' @concept viz
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#'
#' # get first grid
#' grd  <- raster::raster(grds, 1)
#'
#' map_ss_grd(grd)
#'
map_ss_grd <- function(
  grd,
  ply = NULL,
  grd_palette = "Spectral",
  ply_color = "blue",
  grd_opacity = 0.8,
  ply_opacity = 0,
  basemap = leaflet::providers$Esri.OceanBasemap){

  # grd; grd_palette = "Spectral"; grd_opacity = 0.8
  # ply; ply_color = "blue"; ply_opacity = 0.2
  # basemap = providers$Esri.OceanBasemap

  if (!"RasterLayer" %in% class(grd))
    stop("Expecting class(grd) == raster::RasterLayer")

  pal_grd <- colorNumeric(
    grd_palette, values(grd), reverse=T, na.color = "transparent") # other palette: c("#0C2C84", "#41B6C4", "#FFFFCC")

  m <- suppressWarnings({
    leaflet() %>%
      addProviderTiles(basemap) %>%
      addRasterImage(
        grd,
        colors  = pal_grd,
        opacity = grd_opacity) %>%
      addLegend(
        pal    = pal_grd,
        values = values(grd),
        title  = names(grd)) })

  if (!is.null(ply)){
    m <- m %>%
      addPolygons(
        data = ply,
        color = ply_color,
        fillColor = ply_color,
        fillOpacity = ply_opacity)
  }

  m
}

#' Plot Seascape time series from table
#'
#' Plor Seascape as an interactive stacked and filled time series plot.
#'
#' @param tbl table in same format as output by `sum_ss_grds_to_ts`.
#' @param sum_var column name of summary variable to display; one of percent
#'   (`"pct_cells"`; default) or number of cells (`"n_cells"`).
#' @param show_legend When to display the legend. Specify "follow" (default) to
#'   have the legend show as overlay to the chart which follows the mouse.
#'   Specify "always" to always show the legend. Specify "onmouseover" to only
#'   display it when a user mouses over the chart. The "auto" option results in
#'   "always" when more than one series is plotted and "onmouseover" when only a
#'   single series is plotted.
#'
#' @return \code{\link[dygraphs]{dygraph}} interactive plot
#' @import dplyr dygraphs purrr stringr
#' @importFrom tabularaster as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom xts xts
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stringr str_split
#' @export
#' @concept viz
#'
#' @examples
#'
#' # prep input table of time series data
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#' tbl  <- sum_ss_grds_to_ts(grds)
#'
#' plot_ss_ts(tbl)
#'
plot_ss_ts <- function(tbl, sum_var = "pct_cells", show_legend = "follow"){

  # sum_var = "pct_cells"; show_legend = "follow"

  # pivot wide for plotting ----
  # col_pfx <- stringr::str_split(sum_var, "_")[[1]][1]

  d <- tbl %>%
    arrange(desc(is.na(cellvalue)), cellvalue, date) %>%
    select(date, cellvalue, all_of(sum_var)) %>%
    tidyr::pivot_wider(
      id_cols = date,
      names_from  = cellvalue,
      values_from = all_of(sum_var), values_fill = 0)

  d_xts <- xts(d %>% select(-date), order.by = d$date)

  pal <- RColorBrewer::brewer.pal(11, "Spectral")

  dygraph(d_xts, main = "Seascape CLASS") %>%
    dyOptions(
      fillGraph = TRUE, fillAlpha = 0.6,
      stackedGraph = TRUE,
      colors = c("gray", rev(colorRampPalette(pal)(ncol(d)-2)))) %>%
    dyLegend(show = show_legend) %>%
    dyRangeSelector(height = 20) %>%
    dyAxis(
      "y",
      valueFormatter = "function(v){return (v*100).toFixed(1) + '%'}",
      axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}") %>%
    dyHighlight(
      highlightCircleSize = 3,
      highlightSeriesOpts = list(
        fillAlpha = 1,
        strokeWidth = 3),
      highlightSeriesBackgroundAlpha = 0.4,
      hideOnMouseOut = T)
}
