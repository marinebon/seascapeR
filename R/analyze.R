#' Summarize Seascape grids into time series table
#'
#' Summarize Seascape grids into a table having columns: `date`, `cellvalue` (ie
#' CLASS), number of cells (`n_cells`) and percent of cells (`pct_cells`).
#'
#' This function is particularly helpful in between using `get_ss_grds()` and
#' `plot_ss_ts()`.
#'
#' Note that the minimum number of class `NA` cells across all dates get
#' subtracted from each of the `NA` class across all dates. These NAs are mostly
#' attributable to the polygon mask within the polygon's bounding box, as given
#' by `ply` argument to `get_ss_grds()`, since the resulting grids are square
#' and get assigned `NA` values outside the polygon within the polygon's
#' bounding box. Some of the `NA` values may also be attributable to the land
#' mask or consistent cloud cover across all dates. Attributes get assigned to
#' the output table object:
#'
#' - `attr(d, "n_cells")`: original number of cells per date
#'
#' - `attr(d, "n_cells_na")`: minimum number of NA cells which get subtracted
#' from the original
#'
#' - `attr(d, "pct_cells_na")`: `n_cells_na/n_cells`, so a percentage of cells
#' subtracted
#'
#' @param grds raster stack with more than one date, as returned by
#'   \code{\link{get_ss_grds}}
#' @param ts_csv path to csv to save this time series table. Default is NULL, in
#'   which case the table is not saved. If path is set and already exists then
#'   that will be read in if all dates in the `grds` are present instead of
#'   recalculating and writing to `ts_csv`. Another file with attributes (per
#'   Details) is also saved using the same path with the extra `*_attr.csv`
#'   suffix.
#' @param verbose show messages of process. Useful for debugging. Default:
#'   FALSE.
#'
#' @return `tibble` of data
#' @import dplyr purrr stringr
#' @importFrom tabularaster as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom readr read_csv write_csv cols
#' @export
#' @concept analyze
#'
#' @examples
#' ply  <- get_url_ply("mbnms")
#' ss_i <- get_ss_info()
#' grds <- get_ss_grds(ss_i, ply, date_beg = "2020-01-01")
#' tbl  <- sum_ss_grds_to_ts(grds)
#' tbl
#'
#' # extra attributes assigned; see Details
#' attr(tbl, "n_cells")
#' attr(tbl, "n_cells_na")
#' attr(tbl, "pct_cells_na")
#'
sum_ss_grds_to_ts <- function(grds, ts_csv = NULL, verbose = F){

  # devtools::load_all()
  # ply  <- get_url_ply("mbnms"); ss_i <- get_ss_info()
  # grds <- get_ss_grds(ss_i, ply, date_beg = "2018-01-01",
  #   dir_tif = here::here("data_ss/mbnms_global_monthly"))
  # ts_csv <- here::here("data_ss/mbnms_global_monthly_CLASS.csv")
  # sum_ss_grds_to_ts(grds, here::here("data_ss/mbnms_global_monthly_CLASS.csv"), verbose = T)

  grds_dates <- tibble(
    date = names(grds) %>%
      str_split("_") %>%
      map(2) %>%
      unlist() %>%
      str_replace_all("[.]", "-") %>%
      as.Date()) %>%
    tibble::rownames_to_column(var = "dimindex") %>%
    mutate(
      dimindex = as.integer(dimindex))

  if (!is.null(ts_csv)){
    ts_attr_csv <- glue("{fs::path_ext_remove(ts_csv)}_attr.csv")

    if (file.exists(ts_csv)){

      if (verbose)
        message("Reading ts_csv")

      d <- readr::read_csv(ts_csv, col_types = cols())

      grds_dates_match <- grds_dates$date %in% unique(d$date)
      if (all(grds_dates_match)){

        if (!all(unique(d$date) %in% grds_dates$date))
          stop("WHOAH! Haven't handled this condition
               that all dates in `grds` are found in `ts_csv`,
               but not all dates in `ts_csv` are found in `grds`.
               What does this mean?")

        if (verbose)
          message("All grds_dates found in ts_csv$date, so returning vs recalculating.")

        d_attr <- readr::read_csv(ts_attr_csv, col_types = cols())

        with(d_attr, {
          attr(d, "n_cells")      <- n_cells
          attr(d, "n_cells_na")   <- n_cells_na
          attr(d, "pct_cells_na") <- pct_cells_na })

        return(d)
      } else {
        if (verbose)
          message(
            glue("Only {sum(grds_dates_match)} of {length(grds_dates_match)} grds_dates found in ts_csv$date, so recalculating."))
      }
    } else {
      if (verbose)
        message(
          glue("The file ts_csv {ts_csv} was not found, so calculating."))
    }
  }

  if (verbose)
    message("Converting raster stack to tibble, then summarizing by date, class.")
  d <- tabularaster::as_tibble(grds) %>%
    left_join(
      grds_dates, by = "dimindex") %>%
    group_by(date, cellvalue) %>%
    summarize(n_cells = n(), .groups = "drop")

  if (verbose)
    message("Calculating min(n_cells) for CLASS = NA for whole time series.")
  n_cells <- d %>%
    group_by(date) %>%
    summarize(n_cells = sum(n_cells)) %>%
    pull(n_cells) %>%
    .[1]

  n_NA_min <- d %>%
    filter(is.na(cellvalue)) %>%
    pull(n_cells) %>%
    min()

  if (verbose)
    message("Deducting min(n_cells) for CLASS = NA for all days across time series, so day with min(n_cells) becomes 0 for CLASS = NA.")
  d <- bind_rows(
    d %>%
      filter(!is.na(cellvalue)),
    d %>%
      filter(is.na(cellvalue)) %>%
      mutate(
        n_cells = n_cells - n_NA_min)) %>%
    arrange(
      date, cellvalue) %>%
    group_by(date) %>%
    mutate(
      pct_cells = n_cells / sum(n_cells)) %>%
    ungroup() %>%
    arrange(date, cellvalue)

  if (verbose)
    message("Adding attributes n_cells, n_cells_na, pct_cells_na.")

  attr(d, "n_cells")      <- n_cells
  attr(d, "n_cells_na")   <- n_NA_min
  attr(d, "pct_cells_na") <- n_NA_min/n_cells

  if (!is.null(ts_csv)){
    if (verbose)
      message("Writing to ts_csv and ts_attr_csv.")

    readr::write_csv(d, ts_csv)

    d_attr <- tibble(
      n_cells      = n_cells,
      n_cells_na   = n_NA_min,
      pct_cells_na = n_NA_min/n_cells)

    readr::write_csv(d_attr, ts_attr_csv)
  }
  d
}
