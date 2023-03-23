
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- DELETE data_ss/ before running -->

# seascapeR <img src="man/figures/logo.png" align="right" />

<!--
[![pkgdown](https://github.com/marinebon/seascapeR/workflows/pkgdown/badge.svg)](https://github.com/marinebon/seascapeR/actions?query=workflow%3Apkgdown)
-->
<!--
[![R build status](https://github.com/mitchelloharawild/icon/workflows/R-CMD-check/badge.svg)](https://github.com/mitchelloharawild/icon/actions?workflow=R-CMD-check)
[![Coverage status](https://codecov.io/gh/mitchelloharawild/icon/branch/master/graph/badge.svg)](https://codecov.io/gh/mitchelloharawild/icon?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/icon)](https://cran.r-project.org/package=icon)
-->
<!-- [![Downloads](http://cranlogs.r-pkg.org/badges/icon?color=brightgreen)](https://cran.r-project.org/package=icon) -->

The `seascapeR` package provides functions for fetching, analyzing and
visualizing
[Seascapes](https://coastwatch.noaa.gov/cw/satellite-data-products/multi-parameter-models/seascape-pelagic-habitat-classification.html),
a global and regional dynamic sea water classification product derived
from satellite imagery by [Maria Kavanaugh
(OSU)](https://ceoas.oregonstate.edu/people/maria-kavanaugh).

In particular, this package is meant to help nodes in the
[MarineBON.org](https://marinebon.org) network extract Seascape data,
especially across [NOAA Sanctuaries](https://sanctuaries.noaa.gov), for
comparison with biological data from eDNA, sound, telemetry and other
observational data to evaluate how dynamic water masses relate to
ecosystem function.

## Shiny app

Besides the documentation below and in [Get
Started](https://marinebon.org/seascapeR/articles/seascapeR.html), to
see an example of how `seascapeR` functions get used to fetch data
across sanctuaries, check out the
[get_data.R](https://github.com/marinebon/seascape_app/blob/main/get_data.R)
script. The gathered data from this script then feeds the [Seascapes for
Sanctuaries app](https://shiny.marinebon.app/seascapes/) built with
[Shiny](https://shiny.rstudio.com). To see how the app generates maps
and time series plots, see the app’s code at
[app.R](https://github.com/marinebon/seascape_app/blob/main/app/app.R).
To see how the Seascape definitions with accompanying relative
histograms are rendered in
[classes.html](https://shiny.marinebon.app/seascapes/classes.html) see
the source [Rmarkdown](https://rmarkdown.rstudio.com) file
[classes.Rmd](https://github.com/marinebon/seascape_app/blob/main/app/www/classes.Rmd).

<!-- [multi-column layout](https://bookdown.org/yihui/rmarkdown-cookbook/multi-column.html] -->

<div style="display: flex;">

<div>

[Seascapes for Sanctuaries](https://shiny.marinebon.app/seascapes/)
Shiny app:

![](man/figures/README-shiny_map-ts.png)

</div>

<div>

[Seascape Classes](https://shiny.marinebon.app/seascapes/classes.html)
described:

![](man/figures/README-shiny_classes.png)

</div>

</div>

## Install

``` r
remotes::install_github("marinebon/seascapeR")
```

## Use

Load the library and map Seascape classes using a web map server (wms)
that loads image tiles (not data) interactively (zoom, pan) from R.

``` r
library(seascapeR)
#> Warning: replacing previous import 'dplyr::group_rows' by
#> 'kableExtra::group_rows' when loading 'seascapeR'
#> Registered S3 method overwritten by 'hoardr':
#>   method           from
#>   print.cache_info httr

# variables
sanctuary   = "mbnms"          # or see: ?get_url_ply
ss_dataset  = "global_monthly" # or "global_8day"
ss_var      = "CLASS"          # or "P"
date_beg    = "2020-01-01"
date_end    = "2021-01-01"

# paths
dir_data = here::here("data_ss")
dir_ply  = glue::glue("{dir_data}/ply")
dir_grd  = glue::glue(
  "{dir_data}/{sanctuary}_{ss_dataset}")
ts_csv   = glue::glue(
  "{dir_data}/{sanctuary}_{ss_dataset}_{ss_var}.csv")

# get sanctuary polygon
ply <- get_url_ply(
  sanctuary = sanctuary, 
  dir_ply   = dir_ply)
ply
#> Geometry set for 1 feature 
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -123.1401 ymin: 35.5 xmax: -121.1036 ymax: 37.88163
#> Geodetic CRS:  WGS 84
#> MULTIPOLYGON (((-122.5 35.9, -123 35.9, -123 35...

# get SeaScape dataset information
ss_info <- get_ss_info(dataset = ss_dataset)
ss_info
#> <ERDDAP info> noaa_aoml_4729_9ee6_ab54 
#>  Base URL: https://cwcgom.aoml.noaa.gov/erddap 
#>  Dataset Type: griddap 
#>  Dimensions (range):  
#>      time: (2003-01-15T12:00:00Z, 2023-02-15T12:00:00Z) 
#>      latitude: (-89.975, 89.975) 
#>      longitude: (-179.975, 179.975) 
#>  Variables:  
#>      CLASS: 
#>          Units: None 
#>      P: 
#>          Units: Punits

# map SeaScape using web map server (wms) image tiles
map_ss_wms(ss_info, ply, ss_var = ss_var)
```

![](man/figures/README-map_seascape_wms-1.png)<!-- -->

``` r

# get SeaScape grids within polyon for date range 
grds <- get_ss_grds(
  ss_info, ply, 
  ss_var    = ss_var, 
  date_beg  = date_beg, 
  date_end  = date_end,
  dir_tif   = dir_grd)

# get first grid, a raster layer in the raster stack grds
grd <- raster::raster(grds, 1)

# map SeaScape grid
map_ss_grd(grd)
```

![](man/figures/README-map_seascape_wms-2.png)<!-- -->

``` r

# summarize SeaScape grids into a time series table
tbl <- sum_ss_grds_to_ts(grds, ts_csv = ts_csv)
tbl
#> # A tibble: 61 × 3
#>    date       cellvalue n_cells
#>    <date>         <dbl>   <dbl>
#>  1 2020-01-15         7       2
#>  2 2020-01-15        12      31
#>  3 2020-01-15        14     565
#>  4 2020-01-15        21       2
#>  5 2020-01-15        NA    1368
#>  6 2020-02-15        14     547
#>  7 2020-02-15        19      11
#>  8 2020-02-15        21      44
#>  9 2020-02-15        NA    1366
#> 10 2020-03-15         7       3
#> # … with 51 more rows

# plot SeaScape time series
plot_ss_ts(tbl, show_legend = "always")
```

![](man/figures/README-map_seascape_wms-3.png)<!-- -->

### Interactivity

Note that when you run the code above in any of these R environments,
you get an interactive visualization:

1.  [RStudio](https://rstudio.com/products/rstudio/download/) Console;
2.  knitted html from an [Rmarkdown](https://rmarkdown.rstudio.com/)
    document; or
3.  a [Shiny](https://shiny.rstudio.com/) app.

The interactive map allows you to zoom and pan. The time series has
sliders to change the time window and hover over to get values.

For more on how to use the `seascapeR` functions and view these
interactive visualizations, see the [Getting
Started](articles/seascapeR.html) article.

### Data files

Data files were cached by setting path variables and feeding as
arguments to functions in the following order:

1.  `get_url_ply()`: Based on `dir_ply`, save \*.zip, unzip and
    shapefile with file components \*.shp, etc, readable by any GIS
    program.
2.  `get_ss_grds()`: Based on `dir_grd`, save grids (aka rasters) as
    GeoTIFs (\*.tif) with filenames of form
    “grd\_$$ss\_var$$\_$$date$$.tif”, readable by any GIS program.
3.  `plot_ss_ts()`: Based on `ts_csv`, save the table as a
    comma-seperated value (\*.csv) file, readable by any spreadsheet
    program.

``` r
fs::dir_tree(dir_data)
#> /Users/bbest/Github/marinebon/seascapeR/data_ss
#> ├── fknms_global_8day
#> │   ├── grd_CLASS_2022.10.16.tif
#> │   ├── grd_CLASS_2022.10.24.tif
#> │   ├── grd_CLASS_2022.11.01.tif
#> │   ├── grd_CLASS_2022.11.09.tif
#> │   ├── grd_CLASS_2022.11.17.tif
#> │   ├── grd_CLASS_2022.11.25.tif
#> │   ├── grd_CLASS_2022.12.03.tif
#> │   ├── grd_CLASS_2022.12.11.tif
#> │   ├── grd_CLASS_2022.12.19.tif
#> │   ├── grd_CLASS_2022.12.27.tif
#> │   ├── grd_CLASS_2023.01.01.tif
#> │   ├── grd_CLASS_2023.01.09.tif
#> │   ├── grd_CLASS_2023.01.17.tif
#> │   ├── grd_CLASS_2023.01.25.tif
#> │   ├── grd_CLASS_2023.02.02.tif
#> │   └── grd_CLASS_2023.02.10.tif
#> ├── fknms_global_8day_CLASS.csv
#> ├── mbnms_global_monthly
#> │   ├── grd_CLASS_2020.01.15.tif
#> │   ├── grd_CLASS_2020.02.15.tif
#> │   ├── grd_CLASS_2020.03.15.tif
#> │   ├── grd_CLASS_2020.04.15.tif
#> │   ├── grd_CLASS_2020.05.15.tif
#> │   ├── grd_CLASS_2020.06.15.tif
#> │   ├── grd_CLASS_2020.07.15.tif
#> │   ├── grd_CLASS_2020.08.15.tif
#> │   ├── grd_CLASS_2020.09.15.tif
#> │   ├── grd_CLASS_2020.10.15.tif
#> │   ├── grd_CLASS_2020.11.15.tif
#> │   ├── grd_CLASS_2020.12.15.tif
#> │   └── grd_CLASS_2021.01.15.tif
#> ├── mbnms_global_monthly_CLASS.csv
#> └── ply
#>     ├── fknms_py2
#>     │   ├── FKNMS_py.sbn
#>     │   ├── FKNMS_py.sbx
#>     │   ├── fknms_py.dbf
#>     │   ├── fknms_py.kml
#>     │   ├── fknms_py.prj
#>     │   ├── fknms_py.shp
#>     │   ├── fknms_py.shp.htm
#>     │   ├── fknms_py.shp.xml
#>     │   ├── fknms_py.shx
#>     │   └── fknms_py.xml
#>     ├── fknms_py2.zip
#>     ├── mbnms_py2
#>     │   ├── mbnms_py.dbf
#>     │   ├── mbnms_py.html
#>     │   ├── mbnms_py.kml
#>     │   ├── mbnms_py.prj
#>     │   ├── mbnms_py.sbn
#>     │   ├── mbnms_py.sbx
#>     │   ├── mbnms_py.shp
#>     │   ├── mbnms_py.shp.htm
#>     │   └── mbnms_py.shx
#>     └── mbnms_py2.zip
```

## Contribute

For more on how to contribute to `seascapeR` package development, see
[CONTRIBUTE.md](https://github.com/marinebon/seascapeR/blob/main/CONTRIBUTE.md).
