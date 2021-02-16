# seascapeR 0.2.2

* Added `get_ss_dates_all()` to get a list of all dates between `date_beg` and `date_end` based on availability of SeaScape dataset (via `get_ss_info()`)

* Modified `get_ss_grds()` to fetch missing rasters from ERDDAP that are not found in `dir_tif` cache.

# seascapeR 0.2.1

* Added `nms` sanctuary lookup and improved URL handling for them in `get_url_ply()`.

# seascapeR 0.2.0

* +`get_url_ply()` for sanctuary, `bbox_ply()`, `get_ss_grds(ss_info, ply)`;
cache ss_grds.tif and ss_tbl_ts.csv; renamed SeaScape functions to `ss`

# seascapeR 0.1.1

* Added `plot_seascape_ts()` for time series plot of raster stack.

# seascapeR 0.1.0

* Created initial set of functions from @eqmh R script.
