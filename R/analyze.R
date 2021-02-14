#' Summarize Seascape grids into time series table
#'
#' Summarize Seascape grids into a table of: `date`, number of cells (`ncells_*`) and percent (`pct_*`) of variable
#' (eg `ss_var = "CLASS"` or `"P"`).
#'
#' This function is particularly helpful in between using `get_ss_grds()` and `plot_ss_ts()`.
#'
#' Note that the `ncells_NA` gets subtracted from the overall minimum number of NAs to account for masked area.
#'
#' @param grds raster stack with more than one date, as returned by
#'   \code{\link{get_ss_grds}}
#'
#' @return `tibble` of data
#' @import dplyr purrr stringr
#' @importFrom tabularaster as_tibble
#' @importFrom tidyr pivot_wider
#' @importFrom tibble rownames_to_column
#' @importFrom readr read_csv write_csv
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
sum_ss_grds_to_ts <- function(grds, ts_csv = NULL){

  if (!is.null(ts_csv) && file.exists(ts_csv)){
    message("Reading ts_csv vs re-running.")
    return(readr::read_csv(ts_csv))
  }

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

  # convert raster stack to tibble and summarize by date, class
  d <- tabularaster::as_tibble(grds) %>%
    left_join(
      grds_dates, by = "dimindex") %>%
    group_by(date, cellvalue) %>%
    summarize(n_cells = n(), .groups = "drop")

  # adjust NAs by min(NA) ----
  n_NA_min <- d %>%
    filter(is.na(cellvalue)) %>%
    pull(n_cells) %>%
    min()

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

  if (!is.null(ts_csv))
    readr::write_csv(d, ts_csv)

  d
}
