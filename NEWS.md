# seascapeR 0.4.2

- Patched `get_ss_grds()` to pass date as character and avoid error "time must be given as character strings" (#6).

# seascapeR 0.4.1

- Fixed `plot_ss_ts()` to no longer prefix default color with gray for NA since NAs now get dropped. Added optional argument colors to enable synchronization of timeseries colors with map colors.

# seascapeR 0.4.0

- Fixed time series wonkiness #3 with new values by only storing `n_cells` in output
of `sum_ss_grds_to_ts()` and calculating max y percent in `plot_ss_ts()`.

- fixed `get_ss_grds()` to work with ERDDAP change from using `lon` and `lat` to `longitude` and `latitude`.

# seascapeR 0.3.3

Added `del_cache` (default: False) argument to `get_ss_grds()`, since an updated ERDDAP dataset first requires removal of any caches (via `rerddap::cache_delete_all(force = T)`).

# seascapeR 0.3.2

Added `ss_vars` for Seascape variable descriptions.

# seascapeR 0.3.1

Added argument `n_bins` to `plot_ss_class_var()` for option of producing binned histogram versus density histogram curve.

# seascapeR 0.3.0

Added Seascape class information with dataset `ss_gl_classes` and functions `plot_ss_class_var()` and `plot_ss_class_var_svg()` used by `tbl_ss_class()`

# seascapeR 0.2.3

Changed `sum_ss_grds_to_ts()` to recalculate if not all dates in `grds` found in `ts_csv`.

# seascapeR 0.2.2

* Added `get_ss_dates_all()` to get a list of all dates between `date_beg` and `date_end` based on availability of SeaScape dataset (via `get_ss_info()`).

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
