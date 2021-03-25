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
#' ss_i <- get_ss_info("global_8day")
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-09-01")
#' tbl  <- sum_ss_grds_to_ts(grds)
#'
#' plot_ss_ts(tbl)
#'
plot_ss_ts <- function(
  tbl,
  sum_var = "pct_cells",
  fillAlpha = 0.8,
  show_legend = "follow"){

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

  dygraph(d_xts, main = "Seascape Class") %>%
    dyOptions(
      fillGraph = T, fillAlpha = fillAlpha,
      stackedGraph = T,
      colors = c("gray", rev(colorRampPalette(pal)(ncol(d)-2))),
      retainDateWindow = T) %>%
    dyLegend(show = show_legend) %>%
    dyRangeSelector(height = 20) %>%
    dyAxis(
      "y",
      valueRange         = c(0, 1.001),
      valueFormatter     = "function(v){return (v*100).toFixed(1) + '%'}",
      axisLabelFormatter = "function(v){return (v*100).toFixed(0) + '%'}") %>%
    dyHighlight(
      highlightCircleSize = 3,
      highlightSeriesOpts = list(
        fillAlpha = 1,
        strokeWidth = 3),
      highlightSeriesBackgroundAlpha = 0.4,
      hideOnMouseOut = T)
}

#' Plot Seascape Class Variable
#'
#' Plot the provided value relative to all average values for the
#' given variable across Seascape Classes.
#'
#' @param var variable to plot, for getting distribution of all Class averages from \code{\link{ss_gl_classes}}
#' @param val average value to plot as a vertical line
#' @param n_bins number of bins for generating histogram. If NULL (default), then perform a density histogram curve.
#' @param ln_color color of vertical line; default = `"red"`
#' @param ln_size width of vertical line; default = `5`
#' @param ply_color color of polygon representing distribution of all average values for `var`; default = `"black"`
#' @param ply_alpha color of polygon representing distribution of all average values for `var`; default = `0.5`
#' @param tbl_classes table of values from which to extract `var`; default = \code{\link{ss_gl_classes}}
#'
#' @return
#' @concept viz
#' @export
#' @import dplyr ggplot2 ggthemes
#'
#' @examples
#'
plot_ss_class_var <- function(
  var, val,
  #txt_size = 40,
  n_bins = NULL,
  ply_color = "black", ply_alpha = 0.5,
  ln_color = "red", ln_size = 5,
  tbl_classes = ss_gl_classes){
  # devtools::load_all()
  # tbl_classes = ss_gl_classes; ply_color = "black"; ply_alpha = 0.5; ln_color = "red"; ln_size = 5
  # var = "SST (°C)"
  # val = ss_gl_classes %>% slice(1) %>% pull(var)
  # n_bins = NULL

  g <- tbl_classes %>%
    select(!!var) %>%
    filter(!is.na(!!as.symbol(var))) %>%
    ggplot(aes(x=!!as.symbol(var))) +
    theme_tufte(base_family = "") +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA),
      plot.background  = element_rect(fill = "transparent", color = NA),
      axis.title       = element_blank(),
      axis.text.x      = element_blank(),
      axis.text.y      = element_blank(),
      axis.ticks       = element_blank())

  if (is.null(n_bins)){
    g <- g +
      geom_density(alpha = ply_alpha, fill = ply_color, color=NA)
  } else {
    # default: n_bins = 10
    g +
      geom_histogram(bins = n_bins, alpha = ply_alpha, fill = ply_color, color=NA)
  }
  g +
    geom_vline(
      xintercept = val, color = ln_color, size = ln_size)
}

svg2img_inline <- function(f_svg){
  # not using inline img anymore
  paste(
    '<img src="data:image/svg+xml;utf8,',
    readLines(f_svg) %>%
      .[-1] %>%
      paste(collapse = "\n"),
    '">')
}

#' Plot Seascape Class variable as SVG
#'
#' This function wraps around \code{\link{plot_ss_class_var}} to write the ggplot as a vector
#' format file (*.svg) and return a path to the file that could be used for the
#' web.
#'
#' @param var variable to plot, for getting distribution of all Class averages from \code{\link{ss_gl_classes}}
#' @param val average value to plot as a vertical line
#' @param g_svg local file path to output ggplot as scalable vector graphic (*.svg)
#' @param web_svg web prefix with which to return the path in HTML as an image
#' @param ht_ratio ratio of height to width; default = `0.3`
#' @param ... other parameters to pass onto \code{\link{plot_ss_class_var}}
#'
#' @return
#' @concept viz
#' @import ggplot2
#' @export
#'
#' @examples
plot_ss_class_var_svg <- function(var, val, g_svg, web_svg, redo = F, ht_ratio = 0.3, ...){

  svg2img <- function(g_svg, web_svg)
    glue("<img src='{web_svg}/{basename(g_svg)}'>")

  if (file.exists(g_svg)  & !redo)
    return(svg2img(g_svg, web_svg))

  w=9;
  g <- plot_ss_class_var(var, val, ...)
  ggsave(file = g_svg, plot = g, width = w, height = w*ht_ratio, bg = "transparent")
  svg2img(g_svg, web_svg)
}


#' Table of Seascape Class with plots per variable
#'
#' Produce a table of variable average values for the Seascape Class with a
#' distribution plot relative to all Classes. This function wraps around
#' \code{\link{plot_ss_class_var}}.
#'
#' @param class integer identifier for Seascape Class
#' @param dir_svg local filesystem directory where to store SVG files
#' @param web_svg web prefix for referencing the SVG files
#' @param tbl_classes table of values from which to extract `var`; default =
#'   \code{\link{ss_gl_classes}}
#' @param ... other parameters to pass onto \code{\link{plot_ss_class_var}}
#'
#' @return
#' @concept viz
#' @export
#' @import dplyr glue kableExtra knitr purrr tidyr
#'
#' @examples
tbl_ss_class <- function(class, dir_svg, web_svg, tbl_classes = ss_gl_classes, ...){

  f <- ss_gl_classes %>%
    filter(CLASS == !!class) %>%
    select(NAME, LATITUDE, `DOMINANT HEMISPHERE`, `DOMINANT SEASON`) %>%
    pivot_longer(everything(), names_to = "var", values_to = "val") %>%
    mutate(
      lbl = glue("{var}: {val}")) %>%
    pull(lbl)
  f <- c(glue("CLASS: {class}"), f)
  f <- glue("- {f}")
  h <- markdown::markdownToHTML(text = paste(f, collapse = "\n"), fragment.only = T)

  d <- ss_gl_classes %>%
    select_if(is.numeric) %>%
    pivot_longer(-CLASS, names_to = "var", values_to = "val") %>%
    group_by(var) %>%
    mutate(
      min = min(val, na.rm = T),
      max = max(val, na.rm = T)) %>%
    ungroup() %>%
    filter(CLASS == !!class) %>%
    mutate(
      v      = recode(
        var,
        `SST (°C)`                     = "sst",
        `SSS (psu)`                    = "sss",
        `ADT (m)`                      = "adt",
        `ICE (%)`                      = "ice",
        `CDOM (m^-1^)`                 = "cdom",
        `CHLA (mg m^-3^)`              = "chla",
        `NFLH (W m^-2^ µm^-1^ sr^-1^)` = "nflh",
        `NFLH:CHL`                     = "nflh-chl"),
      g_svg  = glue("{dir_svg}/ss_cl{CLASS}_{v}.svg"),
      g_html = pmap_chr(
        list(var, val, g_svg, web_svg),
        plot_ss_class_var_svg, ...))

  d %>%
    select(Variable = var, `Class Avg` = val, `Relative to All Classes` = g_html, `All Min`=min, `All Max`=max) %>%
    #knitr::kable(escape = F, align = "lcccc") %>%
    kbl(escape = F, align = "lcccc") %>%
    #kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = T)
    kable_material(c("striped", "hover", "condensed"), full_width = T) %>%
    footnote(general = h, escape = F, footnote_as_chunk = T, general_title = "")
}
